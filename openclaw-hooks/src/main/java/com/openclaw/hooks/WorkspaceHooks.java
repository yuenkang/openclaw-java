package com.openclaw.hooks;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.stream.Stream;

/**
 * Workspace hook loading and snapshot building.
 * Scans directories for hook definitions (HOOK.md files) and builds snapshots.
 * Corresponds to TypeScript's hooks/workspace.ts.
 */
@Slf4j
public class WorkspaceHooks {

    private static final String HOOK_MD = "HOOK.md";
    private static final List<String> HANDLER_CANDIDATES = List.of(
            "handler.ts", "handler.js", "handler.java", "index.ts", "index.js");

    /**
     * Load hook entries from a directory.
     */
    public static List<HookTypes.HookEntry> loadHookEntriesFromDir(
            String dir, HookTypes.HookSource source, String pluginId) {

        Path dirPath = Path.of(dir);
        if (!Files.isDirectory(dirPath)) {
            return Collections.emptyList();
        }

        List<HookTypes.HookEntry> entries = new ArrayList<>();

        try (Stream<Path> children = Files.list(dirPath)) {
            children.filter(Files::isDirectory).forEach(hookDir -> {
                Path hookMdPath = hookDir.resolve(HOOK_MD);
                if (!Files.exists(hookMdPath))
                    return;

                try {
                    String content = Files.readString(hookMdPath);
                    Map<String, String> frontmatter = HookFrontmatter.parseFrontmatter(content);
                    String name = frontmatter.getOrDefault("name",
                            hookDir.getFileName().toString());
                    String description = frontmatter.getOrDefault("description", "");

                    // Find handler file
                    String handlerPath = null;
                    for (String candidate : HANDLER_CANDIDATES) {
                        Path candidatePath = hookDir.resolve(candidate);
                        if (Files.exists(candidatePath)) {
                            handlerPath = candidatePath.toString();
                            break;
                        }
                    }

                    if (handlerPath == null) {
                        log.debug("Hook '{}' has HOOK.md but no handler file in {}",
                                name, hookDir);
                        return;
                    }

                    HookTypes.Hook hook = HookTypes.Hook.builder()
                            .name(name)
                            .description(description)
                            .source(source)
                            .pluginId(pluginId)
                            .filePath(hookMdPath.toString())
                            .baseDir(hookDir.toString())
                            .handlerPath(handlerPath)
                            .build();

                    // Use HookFrontmatter for metadata + invocation policy
                    HookTypes.HookMetadata metadata = HookFrontmatter.resolveOpenClawMetadata(frontmatter);
                    if (metadata == null) {
                        metadata = HookTypes.HookMetadata.builder()
                                .events(HookFrontmatter.normalizeStringList(frontmatter.get("events")))
                                .always("true".equalsIgnoreCase(frontmatter.get("always")))
                                .hookKey(frontmatter.get("hookKey"))
                                .emoji(frontmatter.get("emoji"))
                                .build();
                    }
                    HookTypes.HookInvocationPolicy invocation = HookFrontmatter
                            .resolveHookInvocationPolicy(frontmatter);

                    entries.add(HookTypes.HookEntry.builder()
                            .hook(hook)
                            .frontmatter(frontmatter)
                            .metadata(metadata)
                            .invocation(invocation)
                            .build());

                } catch (IOException e) {
                    log.warn("Failed to load hook from {}: {}", hookDir, e.getMessage());
                }
            });
        } catch (IOException e) {
            log.warn("Failed to scan hooks directory {}: {}", dir, e.getMessage());
        }

        return entries;
    }

    /**
     * Load all hook entries from workspace, bundled, managed, and extra
     * directories.
     */
    public static List<HookTypes.HookEntry> loadHookEntries(
            String workspaceDir, OpenClawConfig config,
            String managedHooksDir, String bundledHooksDir) {

        String effectiveManaged = managedHooksDir != null
                ? managedHooksDir
                : resolveDefaultManagedDir();
        String workspaceHooksDir = Path.of(workspaceDir, "hooks").toString();

        // Load from each source with precedence (later sources override earlier)
        Map<String, HookTypes.HookEntry> merged = new LinkedHashMap<>();

        // 1. Extra directories from config
        if (config != null && config.getHooks() != null
                && config.getHooks().getInternal() != null
                && config.getHooks().getInternal().getLoad() != null) {
            var load = config.getHooks().getInternal().getLoad();
            List<String> extraDirs = load.getExtraDirs();
            if (extraDirs != null) {
                for (String extra : extraDirs) {
                    loadHookEntriesFromDir(extra.trim(),
                            HookTypes.HookSource.OPENCLAW_WORKSPACE, null)
                            .forEach(e -> merged.put(e.getHook().getName(), e));
                }
            }
        }

        // 2. Bundled hooks
        if (bundledHooksDir != null) {
            loadHookEntriesFromDir(bundledHooksDir,
                    HookTypes.HookSource.OPENCLAW_BUNDLED, null)
                    .forEach(e -> merged.put(e.getHook().getName(), e));
        }

        // 3. Managed hooks
        loadHookEntriesFromDir(effectiveManaged,
                HookTypes.HookSource.OPENCLAW_MANAGED, null)
                .forEach(e -> merged.put(e.getHook().getName(), e));

        // 4. Workspace hooks (highest precedence)
        loadHookEntriesFromDir(workspaceHooksDir,
                HookTypes.HookSource.OPENCLAW_WORKSPACE, null)
                .forEach(e -> merged.put(e.getHook().getName(), e));

        return new ArrayList<>(merged.values());
    }

    /**
     * Filter hook entries based on configuration and eligibility.
     */
    public static List<HookTypes.HookEntry> filterHookEntries(
            List<HookTypes.HookEntry> entries,
            OpenClawConfig config,
            HookConfig.HookEligibilityContext eligibility) {

        return entries.stream()
                .filter(entry -> HookConfig.shouldIncludeHook(entry, config, eligibility))
                .toList();
    }

    /**
     * Build a workspace hook snapshot.
     */
    public static HookTypes.HookSnapshot buildWorkspaceHookSnapshot(
            String workspaceDir, OpenClawConfig config,
            String managedHooksDir, String bundledHooksDir,
            HookConfig.HookEligibilityContext eligibility,
            Integer snapshotVersion) {

        List<HookTypes.HookEntry> allEntries = loadHookEntries(workspaceDir, config, managedHooksDir, bundledHooksDir);
        List<HookTypes.HookEntry> eligible = filterHookEntries(allEntries, config, eligibility);

        List<HookTypes.HookSnapshotItem> items = eligible.stream()
                .map(e -> HookTypes.HookSnapshotItem.builder()
                        .name(e.getHook().getName())
                        .events(e.getMetadata() != null ? e.getMetadata().getEvents() : List.of())
                        .build())
                .toList();

        List<HookTypes.Hook> resolvedHooks = eligible.stream()
                .map(HookTypes.HookEntry::getHook)
                .toList();

        return HookTypes.HookSnapshot.builder()
                .hooks(items)
                .resolvedHooks(resolvedHooks)
                .version(snapshotVersion)
                .build();
    }

    // parseFrontmatter and resolveMetadata are now delegated to HookFrontmatter
    // which in turn uses MarkdownFrontmatter from openclaw-common.

    private static String resolveDefaultManagedDir() {
        String home = System.getProperty("user.home");
        return Path.of(home, ".openclaw", "hooks").toString();
    }
}
