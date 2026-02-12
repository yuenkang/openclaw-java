package com.openclaw.agent.runtime;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;

/**
 * Workspace directory management: workspace creation, bootstrap file
 * scaffolding, and bootstrap file loading for sessions.
 * Mirrors {@code agents/workspace.ts}.
 */
@Slf4j
public final class WorkspaceManager {

    private WorkspaceManager() {
    }

    // --- Constants ---

    public static final String DEFAULT_AGENTS_FILENAME = "AGENTS.md";
    public static final String DEFAULT_SOUL_FILENAME = "SOUL.md";
    public static final String DEFAULT_TOOLS_FILENAME = "TOOLS.md";
    public static final String DEFAULT_IDENTITY_FILENAME = "IDENTITY.md";
    public static final String DEFAULT_USER_FILENAME = "USER.md";
    public static final String DEFAULT_HEARTBEAT_FILENAME = "HEARTBEAT.md";
    public static final String DEFAULT_BOOTSTRAP_FILENAME = "BOOTSTRAP.md";
    public static final String DEFAULT_MEMORY_FILENAME = "MEMORY.md";
    public static final String DEFAULT_MEMORY_ALT_FILENAME = "memory.md";

    private static final Set<String> SUBAGENT_BOOTSTRAP_ALLOWLIST = Set.of(DEFAULT_AGENTS_FILENAME,
            DEFAULT_TOOLS_FILENAME);

    // --- Public types ---

    public record WorkspaceBootstrapFile(
            String name,
            String path,
            String content,
            boolean missing) {
    }

    // --- Workspace resolution ---

    /** Resolve the default agent workspace directory. */
    public static String resolveDefaultAgentWorkspaceDir() {
        String profile = System.getenv("OPENCLAW_PROFILE");
        if (profile != null)
            profile = profile.trim();
        String home = System.getProperty("user.home");

        if (profile != null && !profile.isEmpty()
                && !"default".equalsIgnoreCase(profile)) {
            return Path.of(home, ".openclaw", "workspace-" + profile).toString();
        }
        return Path.of(home, ".openclaw", "workspace").toString();
    }

    // --- Bootstrap file loading ---

    /**
     * Load all bootstrap files from a workspace directory.
     */
    public static List<WorkspaceBootstrapFile> loadWorkspaceBootstrapFiles(String dir) {
        Path resolvedDir = Path.of(dir).toAbsolutePath();

        List<BootstrapEntry> entries = new ArrayList<>(List.of(
                new BootstrapEntry(DEFAULT_AGENTS_FILENAME, resolvedDir.resolve(DEFAULT_AGENTS_FILENAME)),
                new BootstrapEntry(DEFAULT_SOUL_FILENAME, resolvedDir.resolve(DEFAULT_SOUL_FILENAME)),
                new BootstrapEntry(DEFAULT_TOOLS_FILENAME, resolvedDir.resolve(DEFAULT_TOOLS_FILENAME)),
                new BootstrapEntry(DEFAULT_IDENTITY_FILENAME, resolvedDir.resolve(DEFAULT_IDENTITY_FILENAME)),
                new BootstrapEntry(DEFAULT_USER_FILENAME, resolvedDir.resolve(DEFAULT_USER_FILENAME)),
                new BootstrapEntry(DEFAULT_HEARTBEAT_FILENAME, resolvedDir.resolve(DEFAULT_HEARTBEAT_FILENAME)),
                new BootstrapEntry(DEFAULT_BOOTSTRAP_FILENAME, resolvedDir.resolve(DEFAULT_BOOTSTRAP_FILENAME))));

        // Add memory files if they exist
        entries.addAll(resolveMemoryBootstrapEntries(resolvedDir));

        List<WorkspaceBootstrapFile> result = new ArrayList<>();
        for (BootstrapEntry entry : entries) {
            try {
                String content = Files.readString(entry.filePath);
                result.add(new WorkspaceBootstrapFile(
                        entry.name, entry.filePath.toString(), content, false));
            } catch (IOException e) {
                result.add(new WorkspaceBootstrapFile(
                        entry.name, entry.filePath.toString(), null, true));
            }
        }
        return result;
    }

    /**
     * Filter bootstrap files for sub-agent sessions (only AGENTS.md and TOOLS.md).
     */
    public static List<WorkspaceBootstrapFile> filterBootstrapFilesForSession(
            List<WorkspaceBootstrapFile> files, String sessionKey) {
        if (sessionKey == null || !isSubagentSessionKey(sessionKey)) {
            return files;
        }
        return files.stream()
                .filter(f -> SUBAGENT_BOOTSTRAP_ALLOWLIST.contains(f.name()))
                .toList();
    }

    /**
     * Ensure the workspace directory exists and optionally scaffold bootstrap
     * files.
     */
    public static Map<String, String> ensureAgentWorkspace(
            String dir, boolean ensureBootstrapFiles) throws IOException {
        String rawDir = (dir != null && !dir.isBlank()) ? dir.trim()
                : resolveDefaultAgentWorkspaceDir();
        Path resolvedDir = Path.of(rawDir).toAbsolutePath();
        Files.createDirectories(resolvedDir);

        Map<String, String> result = new LinkedHashMap<>();
        result.put("dir", resolvedDir.toString());

        if (!ensureBootstrapFiles) {
            return result;
        }

        String[] bootstrapNames = {
                DEFAULT_AGENTS_FILENAME, DEFAULT_SOUL_FILENAME,
                DEFAULT_TOOLS_FILENAME, DEFAULT_IDENTITY_FILENAME,
                DEFAULT_USER_FILENAME, DEFAULT_HEARTBEAT_FILENAME,
                DEFAULT_BOOTSTRAP_FILENAME
        };

        for (String name : bootstrapNames) {
            Path filePath = resolvedDir.resolve(name);
            result.put(name.replace(".md", "").toLowerCase() + "Path", filePath.toString());
            writeFileIfMissing(filePath, "# " + name.replace(".md", "") + "\n");
        }
        return result;
    }

    // --- Internal helpers ---

    private record BootstrapEntry(String name, Path filePath) {
    }

    private static List<BootstrapEntry> resolveMemoryBootstrapEntries(Path resolvedDir) {
        String[] candidates = { DEFAULT_MEMORY_FILENAME, DEFAULT_MEMORY_ALT_FILENAME };
        List<BootstrapEntry> entries = new ArrayList<>();
        Set<String> seen = new HashSet<>();

        for (String name : candidates) {
            Path filePath = resolvedDir.resolve(name);
            if (!Files.isRegularFile(filePath))
                continue;
            try {
                String realPath = filePath.toRealPath().toString();
                if (seen.contains(realPath))
                    continue;
                seen.add(realPath);
                entries.add(new BootstrapEntry(name, filePath));
            } catch (IOException e) {
                entries.add(new BootstrapEntry(name, filePath));
            }
        }
        return entries;
    }

    private static void writeFileIfMissing(Path filePath, String content) throws IOException {
        try {
            Files.writeString(filePath, content,
                    StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
        } catch (FileAlreadyExistsException ignored) {
            // File already exists — expected
        }
    }

    /** Strip YAML front matter from a template file. */
    static String stripFrontMatter(String content) {
        if (content == null || !content.startsWith("---"))
            return content;
        int endIndex = content.indexOf("\n---", 3);
        if (endIndex == -1)
            return content;
        int start = endIndex + "\n---".length();
        return content.substring(start).stripLeading();
    }

    private static boolean isSubagentSessionKey(String key) {
        return key != null && key.contains(":sub:");
    }
}
