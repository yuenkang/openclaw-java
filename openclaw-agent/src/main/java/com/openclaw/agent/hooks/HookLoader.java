package com.openclaw.agent.hooks;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.stream.Stream;

/**
 * Hook loader: discovers and registers workspace hook handlers.
 * Corresponds to TypeScript's hooks/loader.ts + hooks/workspace.ts.
 *
 * <p>
 * In the Java implementation, hooks are loaded from HOOK.md files
 * found in bundled/managed/workspace directories. Shell-script
 * handlers (.sh) are executed via ProcessBuilder; .js handlers are
 * not supported (TypeScript-specific).
 * </p>
 */
@Slf4j
public class HookLoader {

    private static final String HOOK_MARKER = "HOOK.md";
    private static final int MAX_SCAN_DEPTH = 3;

    private final InternalHookRegistry registry;

    public HookLoader(InternalHookRegistry registry) {
        this.registry = registry;
    }

    /**
     * Load and register all workspace hooks.
     *
     * @param cfg          current configuration
     * @param workspaceDir workspace directory to scan
     * @return number of handlers successfully loaded
     */
    public int loadAll(OpenClawConfig cfg, String workspaceDir) {
        if (cfg == null || cfg.getHooks() == null) {
            log.debug("Hooks config is null, skipping hook loading");
            return 0;
        }

        int loaded = 0;

        // Discover hook entries in workspace
        List<HookTypes.HookEntry> entries = discoverHooks(workspaceDir);
        log.info("Discovered {} hook entries in workspace {}", entries.size(), workspaceDir);

        for (HookTypes.HookEntry entry : entries) {
            HookTypes.HookMetadata meta = entry.getMetadata();
            if (meta == null || meta.getEvents() == null || meta.getEvents().isEmpty()) {
                log.debug("Skipping hook '{}' — no events defined", entry.getHook().getName());
                continue;
            }

            // Check invocation policy
            if (entry.getInvocation() != null && !entry.getInvocation().isEnabled()) {
                log.debug("Skipping hook '{}' — disabled by invocation policy", entry.getHook().getName());
                continue;
            }

            // Create shell handler if handler path is a .sh file
            String handlerPath = entry.getHook().getHandlerPath();
            if (handlerPath != null && handlerPath.endsWith(".sh") && Files.isRegularFile(Path.of(handlerPath))) {
                InternalHookRegistry.HookHandler handler = createShellHandler(handlerPath,
                        entry.getHook().getBaseDir());
                for (String event : meta.getEvents()) {
                    registry.register(event, handler);
                }
                log.info("Registered shell hook: {} -> {}", entry.getHook().getName(),
                        String.join(", ", meta.getEvents()));
                loaded++;
            } else {
                log.debug("Skipping hook '{}' — handler not a .sh script: {}", entry.getHook().getName(), handlerPath);
            }
        }

        return loaded;
    }

    /**
     * Discover hook entries in the workspace directory tree.
     */
    public List<HookTypes.HookEntry> discoverHooks(String workspaceDir) {
        List<HookTypes.HookEntry> entries = new ArrayList<>();
        Path wsPath = Path.of(workspaceDir);

        // Scan: .openclaw/hooks/, hooks/, and workspace root
        scanDirectory(wsPath.resolve(".openclaw").resolve("hooks"), HookTypes.HookSource.OPENCLAW_MANAGED, entries);
        scanDirectory(wsPath.resolve("hooks"), HookTypes.HookSource.OPENCLAW_WORKSPACE, entries);

        return entries;
    }

    private void scanDirectory(Path dir, HookTypes.HookSource source, List<HookTypes.HookEntry> entries) {
        if (!Files.isDirectory(dir))
            return;

        try (Stream<Path> walk = Files.walk(dir, MAX_SCAN_DEPTH)) {
            walk.filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().equals(HOOK_MARKER))
                    .forEach(hookMd -> {
                        try {
                            HookTypes.HookEntry entry = parseHookEntry(hookMd, source);
                            if (entry != null) {
                                entries.add(entry);
                            }
                        } catch (Exception e) {
                            log.debug("Failed to parse hook at {}: {}", hookMd, e.getMessage());
                        }
                    });
        } catch (IOException e) {
            log.debug("Error scanning hooks in {}: {}", dir, e.getMessage());
        }
    }

    private HookTypes.HookEntry parseHookEntry(Path hookMdPath, HookTypes.HookSource source) {
        try {
            String content = Files.readString(hookMdPath);
            Map<String, String> frontmatter = HookFrontmatter.parseFrontmatter(content);

            Path baseDir = hookMdPath.getParent();
            String name = baseDir != null ? baseDir.getFileName().toString() : "unknown";

            // Look for handler.sh in the same directory
            Path handlerSh = baseDir != null ? baseDir.resolve("handler.sh") : null;
            String handlerPath = (handlerSh != null && Files.isRegularFile(handlerSh))
                    ? handlerSh.toAbsolutePath().toString()
                    : null;

            // Parse metadata from frontmatter
            // Use HookFrontmatter for structured metadata resolution
            HookTypes.HookMetadata metadata = HookFrontmatter.resolveOpenClawMetadata(frontmatter);
            if (metadata == null) {
                // Fallback: parse basic frontmatter fields directly
                metadata = HookTypes.HookMetadata.builder()
                        .events(HookFrontmatter.normalizeStringList(frontmatter.get("events")))
                        .always("true".equalsIgnoreCase(frontmatter.get("always")))
                        .hookKey(frontmatter.get("hookKey"))
                        .emoji(frontmatter.get("emoji"))
                        .homepage(frontmatter.get("homepage"))
                        .build();
            }

            HookTypes.Hook hook = HookTypes.Hook.builder()
                    .name(name)
                    .description(frontmatter.getOrDefault("description", ""))
                    .source(source)
                    .filePath(hookMdPath.toAbsolutePath().toString())
                    .baseDir(baseDir != null ? baseDir.toAbsolutePath().toString() : "")
                    .handlerPath(handlerPath)
                    .build();

            HookTypes.HookInvocationPolicy invocation = HookFrontmatter.resolveHookInvocationPolicy(frontmatter);

            return HookTypes.HookEntry.builder()
                    .hook(hook)
                    .frontmatter(frontmatter)
                    .metadata(metadata)
                    .invocation(invocation)
                    .build();
        } catch (IOException e) {
            log.debug("Failed to read hook file {}: {}", hookMdPath, e.getMessage());
            return null;
        }
    }

    // parseFrontmatter and parseMetadata are now delegated to HookFrontmatter
    // which in turn uses MarkdownFrontmatter from openclaw-common.

    /**
     * Create a HookHandler that executes a shell script.
     */
    private InternalHookRegistry.HookHandler createShellHandler(String scriptPath, String workDir) {
        return event -> {
            try {
                ProcessBuilder pb = new ProcessBuilder("/bin/sh", scriptPath);
                pb.directory(Path.of(workDir).toFile());
                pb.environment().put("OPENCLAW_HOOK_EVENT_TYPE", event.getType().key());
                pb.environment().put("OPENCLAW_HOOK_EVENT_ACTION", event.getAction());
                pb.environment().put("OPENCLAW_HOOK_SESSION_KEY", event.getSessionKey());
                pb.redirectErrorStream(true);
                pb.inheritIO();

                Process process = pb.start();
                int exitCode = process.waitFor();
                if (exitCode != 0) {
                    log.warn("Shell hook {} exited with code {}", scriptPath, exitCode);
                }
            } catch (Exception e) {
                log.error("Failed to execute shell hook {}: {}", scriptPath, e.getMessage());
            }
        };
    }
}
