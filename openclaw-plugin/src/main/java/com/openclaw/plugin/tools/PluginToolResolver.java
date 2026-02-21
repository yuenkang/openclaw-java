package com.openclaw.plugin.tools;

import com.openclaw.plugin.PluginTypes;
import com.openclaw.plugin.registry.PluginRegistry;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.function.Function;

/**
 * Plugin tool resolver — resolves plugin-registered tools for injection into
 * the Agent tool registry.
 * Corresponds to TypeScript's plugins/tools.ts resolvePluginTools().
 */
@Slf4j
public class PluginToolResolver {

    // =========================================================================
    // Types
    // =========================================================================

    /**
     * A resolved plugin tool ready for Agent consumption.
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ResolvedPluginTool {
        private String name;
        private String description;
        private String pluginId;
        private boolean optional;
        private Object schema; // JSON Schema for tool parameters
        /** Handler function: accepts tool params, returns result string. */
        private Function<Map<String, Object>, String> handler;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginToolMeta {
        private String pluginId;
        private boolean optional;
    }

    // =========================================================================
    // Resolution
    // =========================================================================

    /**
     * Resolve all plugin-registered tools from the registry.
     *
     * @param registry          plugin registry
     * @param existingToolNames tool names already registered (to detect conflicts)
     * @param toolAllowlist     optional allowlist for optional tools
     * @return list of resolved plugin tools
     */
    public static List<ResolvedPluginTool> resolvePluginTools(
            PluginRegistry registry,
            Set<String> existingToolNames,
            List<String> toolAllowlist) {

        List<ResolvedPluginTool> tools = new ArrayList<>();
        Set<String> existing = existingToolNames != null
                ? new HashSet<>(existingToolNames)
                : new HashSet<>();
        Set<String> allowlist = normalizeAllowlist(toolAllowlist);
        Set<String> blockedPlugins = new HashSet<>();

        for (var entry : registry.getTools()) {
            if (blockedPlugins.contains(entry.getPluginId())) {
                continue;
            }

            String pluginIdKey = normalizeToolName(entry.getPluginId());
            if (existing.contains(pluginIdKey)) {
                String msg = String.format(
                        "plugin id conflicts with core tool name (%s)", entry.getPluginId());
                log.error(msg);
                registry.addDiagnostic(PluginTypes.PluginDiagnostic.builder()
                        .pluginId(entry.getPluginId())
                        .level("error")
                        .message(msg)
                        .build());
                blockedPlugins.add(entry.getPluginId());
                continue;
            }

            if (entry.getNames() == null)
                continue;

            for (String toolName : entry.getNames()) {
                if (existing.contains(toolName)) {
                    String msg = String.format(
                            "plugin tool name conflict (%s): %s",
                            entry.getPluginId(), toolName);
                    log.error(msg);
                    registry.addDiagnostic(PluginTypes.PluginDiagnostic.builder()
                            .pluginId(entry.getPluginId())
                            .level("error")
                            .message(msg)
                            .build());
                    continue;
                }

                // Check optional tool allowlist
                if (entry.isOptional() && !isOptionalToolAllowed(
                        toolName, entry.getPluginId(), allowlist)) {
                    continue;
                }

                existing.add(toolName);
                tools.add(ResolvedPluginTool.builder()
                        .name(toolName)
                        .pluginId(entry.getPluginId())
                        .optional(entry.isOptional())
                        .build());
            }
        }

        log.debug("Resolved {} plugin tools", tools.size());
        return tools;
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    static String normalizeToolName(String name) {
        if (name == null)
            return "";
        return name.trim().toLowerCase().replaceAll("[^a-z0-9]", "_");
    }

    static Set<String> normalizeAllowlist(List<String> list) {
        if (list == null || list.isEmpty())
            return Set.of();
        Set<String> normalized = new HashSet<>();
        for (String item : list) {
            String n = normalizeToolName(item);
            if (!n.isEmpty())
                normalized.add(n);
        }
        return normalized;
    }

    static boolean isOptionalToolAllowed(String toolName, String pluginId,
            Set<String> allowlist) {
        if (allowlist.isEmpty())
            return false;
        String normalizedTool = normalizeToolName(toolName);
        if (allowlist.contains(normalizedTool))
            return true;
        String normalizedPlugin = normalizeToolName(pluginId);
        if (allowlist.contains(normalizedPlugin))
            return true;
        return allowlist.contains("group:plugins");
    }
}
