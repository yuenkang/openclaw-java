package com.openclaw.agent.plugins;

import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Plugin configuration state — normalization and enable/disable resolution.
 * Corresponds to TypeScript's plugins/config-state.ts.
 */
@Slf4j
public final class PluginConfigState {

    private PluginConfigState() {
    }

    public static final Set<String> BUNDLED_ENABLED_BY_DEFAULT = new HashSet<>();

    // =========================================================================
    // Normalized config
    // =========================================================================

    public record NormalizedPluginsConfig(
            boolean enabled,
            List<String> allow,
            List<String> deny,
            List<String> loadPaths,
            Map<String, String> slots,
            Map<String, PluginEntryConfig> entries) {
    }

    public record PluginEntryConfig(Boolean enabled, Object config) {
    }

    // =========================================================================
    // Enable state
    // =========================================================================

    public record EnableState(boolean enabled, String reason) {
        public EnableState(boolean enabled) {
            this(enabled, null);
        }
    }

    // =========================================================================
    // Normalization
    // =========================================================================

    /**
     * Normalize raw plugin configuration into a structured form.
     */
    public static NormalizedPluginsConfig normalizePluginsConfig(
            Map<String, Object> pluginsConfig) {
        if (pluginsConfig == null) {
            return new NormalizedPluginsConfig(
                    true, List.of(), List.of(), List.of(),
                    Map.of("memory", ""), Map.of());
        }

        boolean enabled = !"false".equals(String.valueOf(pluginsConfig.get("enabled")));
        List<String> allow = normalizeList(pluginsConfig.get("allow"));
        List<String> deny = normalizeList(pluginsConfig.get("deny"));

        // loadPaths from config.plugins.load.paths
        List<String> loadPaths = List.of();
        Object loadObj = pluginsConfig.get("load");
        if (loadObj instanceof Map<?, ?> loadMap) {
            loadPaths = normalizeList(loadMap.get("paths"));
        }

        // Slots
        Map<String, String> slots = new LinkedHashMap<>();
        Object slotsObj = pluginsConfig.get("slots");
        if (slotsObj instanceof Map<?, ?> slotsMap) {
            for (var e : slotsMap.entrySet()) {
                if (e.getKey() instanceof String key && e.getValue() instanceof String val) {
                    String trimmed = val.trim();
                    slots.put(key, "none".equalsIgnoreCase(trimmed) ? "" : trimmed);
                }
            }
        }

        // Entries
        Map<String, PluginEntryConfig> entries = new LinkedHashMap<>();
        Object entriesObj = pluginsConfig.get("entries");
        if (entriesObj instanceof Map<?, ?> entriesMap) {
            for (var e : entriesMap.entrySet()) {
                if (!(e.getKey() instanceof String key) || key.isBlank())
                    continue;
                if (e.getValue() instanceof Map<?, ?> entryMap) {
                    Boolean entryEnabled = entryMap.get("enabled") instanceof Boolean b ? b : null;
                    Object config = entryMap.get("config");
                    entries.put(key, new PluginEntryConfig(entryEnabled, config));
                } else {
                    entries.put(key, new PluginEntryConfig(null, null));
                }
            }
        }

        return new NormalizedPluginsConfig(enabled, allow, deny, loadPaths, slots, entries);
    }

    // =========================================================================
    // Enable-state resolution
    // =========================================================================

    /**
     * Resolve whether a plugin should be enabled.
     */
    public static EnableState resolveEnableState(
            String id, PluginTypes.PluginOrigin origin,
            NormalizedPluginsConfig config) {
        if (!config.enabled()) {
            return new EnableState(false, "plugins disabled");
        }
        if (config.deny().contains(id)) {
            return new EnableState(false, "blocked by denylist");
        }
        if (!config.allow().isEmpty() && !config.allow().contains(id)) {
            return new EnableState(false, "not in allowlist");
        }
        // Memory slot match
        String memorySlot = config.slots().get("memory");
        if (id.equals(memorySlot)) {
            return new EnableState(true);
        }
        // Per-entry config
        PluginEntryConfig entry = config.entries().get(id);
        if (entry != null) {
            if (Boolean.TRUE.equals(entry.enabled())) {
                return new EnableState(true);
            }
            if (Boolean.FALSE.equals(entry.enabled())) {
                return new EnableState(false, "disabled in config");
            }
        }
        // Bundled defaults
        if (origin == PluginTypes.PluginOrigin.BUNDLED
                && BUNDLED_ENABLED_BY_DEFAULT.contains(id)) {
            return new EnableState(true);
        }
        if (origin == PluginTypes.PluginOrigin.BUNDLED) {
            return new EnableState(false, "bundled (disabled by default)");
        }
        return new EnableState(true);
    }

    // =========================================================================
    // Memory slot decision
    // =========================================================================

    public record MemorySlotDecision(boolean enabled, String reason, boolean selected) {
        public MemorySlotDecision(boolean enabled) {
            this(enabled, null, false);
        }
    }

    /**
     * Resolve whether a memory plugin should be enabled based on slot
     * configuration.
     */
    public static MemorySlotDecision resolveMemorySlotDecision(
            String id, String kind, String slot, String selectedId) {
        if (!"memory".equals(kind)) {
            return new MemorySlotDecision(true);
        }
        if (slot != null && slot.isEmpty()) {
            // slot == "" means "none" — memory disabled
            return new MemorySlotDecision(false, "memory slot disabled", false);
        }
        if (slot != null) {
            if (slot.equals(id)) {
                return new MemorySlotDecision(true, null, true);
            }
            return new MemorySlotDecision(false, "memory slot set to \"" + slot + "\"", false);
        }
        if (selectedId != null && !selectedId.equals(id)) {
            return new MemorySlotDecision(
                    false, "memory slot already filled by \"" + selectedId + "\"", false);
        }
        return new MemorySlotDecision(true, null, true);
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    static List<String> normalizeList(Object value) {
        if (!(value instanceof List<?> list)) {
            return List.of();
        }
        return list.stream()
                .filter(String.class::isInstance)
                .map(e -> ((String) e).trim())
                .filter(s -> !s.isEmpty())
                .toList();
    }
}
