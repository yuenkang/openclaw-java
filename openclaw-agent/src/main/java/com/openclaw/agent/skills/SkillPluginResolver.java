package com.openclaw.agent.skills;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.nio.file.*;
import java.util.*;

/**
 * Resolves skill directories contributed by plugins.
 * Corresponds to TypeScript skills/plugin-skills.ts.
 */
@Slf4j
public final class SkillPluginResolver {

    private SkillPluginResolver() {
    }

    /**
     * Minimal plugin manifest record for skill resolution.
     */
    public record PluginRecord(
            String id,
            String origin, // "workspace" | "global"
            String kind, // "memory" | "tool" | etc.
            String rootDir,
            List<String> skills,
            boolean enabled) {
    }

    /**
     * Resolve plugin skill directories for a workspace.
     * Reads plugin manifests, checks enabled state, and returns the
     * absolute paths to plugin skill directories.
     *
     * @param workspaceDir workspace root directory
     * @param config       optional config for plugin enable/disable overrides
     * @return list of resolved plugin skill directory paths
     */
    public static List<String> resolvePluginSkillDirs(String workspaceDir, OpenClawConfig config) {
        if (workspaceDir == null || workspaceDir.isBlank()) {
            return List.of();
        }

        List<PluginRecord> plugins = loadPluginManifests(workspaceDir, config);
        if (plugins.isEmpty())
            return List.of();

        Set<String> seen = new LinkedHashSet<>();
        List<String> resolved = new ArrayList<>();

        for (PluginRecord record : plugins) {
            if (record.skills() == null || record.skills().isEmpty())
                continue;
            if (!record.enabled())
                continue;

            for (String raw : record.skills()) {
                String trimmed = raw.trim();
                if (trimmed.isEmpty())
                    continue;

                Path candidate = Path.of(record.rootDir()).resolve(trimmed).toAbsolutePath();
                if (!Files.exists(candidate)) {
                    log.warn("Plugin skill path not found ({}): {}", record.id(), candidate);
                    continue;
                }

                String canonical = candidate.toString();
                if (seen.contains(canonical))
                    continue;
                seen.add(canonical);
                resolved.add(canonical);
            }
        }

        return resolved;
    }

    // ── Plugin manifest loading (simplified) ────────────────────────

    /**
     * Load plugin manifests from workspace and global config directories.
     * This is a simplified version — the full implementation would use
     * PluginManifestRegistry from the plugins subsystem.
     */
    private static List<PluginRecord> loadPluginManifests(
            String workspaceDir, OpenClawConfig config) {
        List<PluginRecord> records = new ArrayList<>();

        // Workspace plugins
        Path workspacePluginsDir = Path.of(workspaceDir, ".openclaw", "plugins");
        scanPluginDir(workspacePluginsDir, "workspace", records, config);

        // Global plugins
        String configDir = System.getProperty("user.home") + "/.openclaw";
        Path globalPluginsDir = Path.of(configDir, "plugins");
        scanPluginDir(globalPluginsDir, "global", records, config);

        return records;
    }

    private static void scanPluginDir(Path pluginsDir, String origin,
            List<PluginRecord> records,
            OpenClawConfig config) {
        if (!Files.isDirectory(pluginsDir))
            return;

        try (var stream = Files.newDirectoryStream(pluginsDir)) {
            for (Path child : stream) {
                if (!Files.isDirectory(child))
                    continue;
                Path manifest = child.resolve("manifest.json");
                if (!Files.isRegularFile(manifest))
                    continue;

                try {
                    String content = Files.readString(manifest);
                    // Simple JSON parsing for id, kind, skills fields
                    String id = extractJsonString(content, "id");
                    String kind = extractJsonString(content, "kind");
                    List<String> skills = extractJsonStringArray(content, "skills");

                    if (id == null || id.isBlank())
                        continue;

                    boolean enabled = isPluginEnabled(id, config);
                    records.add(new PluginRecord(
                            id, origin, kind,
                            child.toAbsolutePath().toString(),
                            skills, enabled));
                } catch (Exception e) {
                    log.debug("Failed to read plugin manifest {}: {}", manifest, e.getMessage());
                }
            }
        } catch (Exception e) {
            log.debug("Failed to scan plugins directory {}: {}", pluginsDir, e.getMessage());
        }
    }

    @SuppressWarnings("unchecked")
    private static boolean isPluginEnabled(String pluginId, OpenClawConfig config) {
        if (config == null || config.getPlugins() == null)
            return true;
        Object pluginsConfig = config.getPlugins();
        if (pluginsConfig instanceof Map<?, ?> map) {
            Object entry = ((Map<String, Object>) map).get(pluginId);
            if (entry instanceof Map<?, ?> entryMap) {
                Object enabled = entryMap.get("enabled");
                if (enabled instanceof Boolean b)
                    return b;
            }
        }
        return true;
    }

    // ── Simple JSON helpers (avoid full Jackson dependency here) ─────

    private static String extractJsonString(String json, String key) {
        String pattern = "\"" + key + "\"\\s*:\\s*\"([^\"]+)\"";
        var matcher = java.util.regex.Pattern.compile(pattern).matcher(json);
        return matcher.find() ? matcher.group(1) : null;
    }

    private static List<String> extractJsonStringArray(String json, String key) {
        String pattern = "\"" + key + "\"\\s*:\\s*\\[([^\\]]*)]";
        var matcher = java.util.regex.Pattern.compile(pattern).matcher(json);
        if (!matcher.find())
            return List.of();
        String array = matcher.group(1);
        List<String> result = new ArrayList<>();
        var itemMatcher = java.util.regex.Pattern.compile("\"([^\"]+)\"").matcher(array);
        while (itemMatcher.find()) {
            result.add(itemMatcher.group(1));
        }
        return result;
    }
}
