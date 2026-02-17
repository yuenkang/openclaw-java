package com.openclaw.agent.plugins;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Plugin registry — central store for all plugin registrations.
 * Corresponds to TypeScript's plugins/registry.ts.
 */
@Slf4j
public final class PluginRegistry {

    // =========================================================================
    // Registration types
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginRecord {
        private String id;
        private String name;
        private String version;
        private String description;
        private PluginTypes.PluginKind kind;
        private String source;
        private PluginTypes.PluginOrigin origin;
        private String workspaceDir;
        private boolean enabled;
        @Builder.Default
        private String status = "loaded"; // "loaded", "disabled", "error"
        private String error;
        @Builder.Default
        private List<String> tools = List.of();
        @Builder.Default
        private List<String> channels = List.of();
        @Builder.Default
        private List<String> providers = List.of();
        @Builder.Default
        private List<String> commands = List.of();
        private int hookCount;
        private boolean configSchema;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginToolRegistration {
        private String pluginId;
        private List<String> names;
        private boolean optional;
        private String source;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginHookRegistration {
        private String pluginId;
        private List<String> events;
        private String source;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginCommandRegistration {
        private String pluginId;
        private String name;
        private String description;
        private String source;
    }

    // =========================================================================
    // Registry state
    // =========================================================================

    private final List<PluginRecord> plugins = new CopyOnWriteArrayList<>();
    private final List<PluginToolRegistration> tools = new CopyOnWriteArrayList<>();
    private final List<PluginHookRegistration> hooks = new CopyOnWriteArrayList<>();
    private final List<PluginCommandRegistration> commands = new CopyOnWriteArrayList<>();
    private final List<PluginTypes.PluginDiagnostic> diagnostics = new CopyOnWriteArrayList<>();
    private final Map<String, PluginRecord> pluginById = new ConcurrentHashMap<>();

    // =========================================================================
    // Registration
    // =========================================================================

    public void registerPlugin(PluginRecord record) {
        plugins.add(record);
        pluginById.put(record.getId(), record);
        log.debug("Registered plugin: {} ({})", record.getId(), record.getStatus());
    }

    public void registerTool(PluginToolRegistration registration) {
        tools.add(registration);
    }

    public void registerHook(PluginHookRegistration registration) {
        hooks.add(registration);
    }

    public void registerCommand(PluginCommandRegistration registration) {
        commands.add(registration);
    }

    public void addDiagnostic(PluginTypes.PluginDiagnostic diagnostic) {
        diagnostics.add(diagnostic);
    }

    // =========================================================================
    // Queries
    // =========================================================================

    public List<PluginRecord> getPlugins() {
        return List.copyOf(plugins);
    }

    public Optional<PluginRecord> getPlugin(String id) {
        return Optional.ofNullable(pluginById.get(id));
    }

    public List<PluginToolRegistration> getTools() {
        return List.copyOf(tools);
    }

    public List<PluginHookRegistration> getHooks() {
        return List.copyOf(hooks);
    }

    public List<PluginCommandRegistration> getCommands() {
        return List.copyOf(commands);
    }

    public List<PluginTypes.PluginDiagnostic> getDiagnostics() {
        return List.copyOf(diagnostics);
    }

    public List<PluginRecord> getEnabledPlugins() {
        return plugins.stream().filter(PluginRecord::isEnabled).toList();
    }

    public List<PluginRecord> getPluginsByKind(PluginTypes.PluginKind kind) {
        return plugins.stream()
                .filter(p -> p.getKind() == kind)
                .toList();
    }

    /**
     * Get plugin tool names for a given plugin.
     */
    public List<String> getToolNamesForPlugin(String pluginId) {
        return tools.stream()
                .filter(t -> pluginId.equals(t.getPluginId()))
                .flatMap(t -> t.getNames().stream())
                .toList();
    }

    // =========================================================================
    // Summary
    // =========================================================================

    public Map<String, Object> getSummary() {
        Map<String, Object> summary = new LinkedHashMap<>();
        summary.put("totalPlugins", plugins.size());
        summary.put("enabledPlugins", getEnabledPlugins().size());
        summary.put("totalTools", tools.size());
        summary.put("totalHooks", hooks.size());
        summary.put("totalCommands", commands.size());
        summary.put("diagnostics", diagnostics.size());
        return summary;
    }

    /**
     * Clear all registrations (useful for testing or reload).
     */
    public void clear() {
        plugins.clear();
        tools.clear();
        hooks.clear();
        commands.clear();
        diagnostics.clear();
        pluginById.clear();
    }
}
