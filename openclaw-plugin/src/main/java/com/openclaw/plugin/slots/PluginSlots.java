package com.openclaw.plugin.slots;

import com.openclaw.plugin.PluginTypes;
import com.openclaw.plugin.PluginTypes.PluginKind;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.*;

/**
 * Plugin slot management — handles exclusive slots where only one plugin
 * of a given kind can be active at a time (e.g. memory provider).
 * Corresponds to TypeScript's plugins/slots.ts.
 */
public final class PluginSlots {

    private PluginSlots() {
    }

    /** Currently the only exclusive slot is 'memory'. */
    private static final Map<PluginKind, String> SLOT_BY_KIND = Map.of(
            PluginKind.MEMORY, "memory");

    private static final Map<String, String> DEFAULT_SLOT_BY_KEY = Map.of(
            "memory", "memory-core");

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SlotSelectionResult {
        private Map<String, Object> pluginsConfig;
        private List<String> warnings;
        private boolean changed;
    }

    /**
     * Get the slot key for a given plugin kind.
     */
    public static String slotKeyForPluginKind(PluginKind kind) {
        return kind != null ? SLOT_BY_KIND.get(kind) : null;
    }

    /**
     * Get the default plugin id for a given slot key.
     */
    public static String defaultSlotIdForKey(String slotKey) {
        return DEFAULT_SLOT_BY_KEY.get(slotKey);
    }

    /**
     * Apply exclusive slot selection — when selecting a plugin for an exclusive
     * slot, disable all other plugins of the same kind.
     */
    @SuppressWarnings("unchecked")
    public static SlotSelectionResult applyExclusiveSlotSelection(
            Map<String, Object> pluginsConfig,
            String selectedId,
            PluginKind selectedKind,
            List<PluginRecord> registryPlugins) {

        String slotKey = slotKeyForPluginKind(selectedKind);
        if (slotKey == null) {
            return new SlotSelectionResult(pluginsConfig, List.of(), false);
        }

        List<String> warnings = new ArrayList<>();

        // Get current slots
        Map<String, Object> slots = pluginsConfig.get("slots") instanceof Map
                ? new HashMap<>((Map<String, Object>) pluginsConfig.get("slots"))
                : new HashMap<>();
        String prevSlot = slots.get(slotKey) instanceof String s ? s : null;
        slots.put(slotKey, selectedId);

        // Warn about switching
        String inferredPrev = prevSlot != null ? prevSlot : defaultSlotIdForKey(slotKey);
        if (inferredPrev != null && !inferredPrev.equals(selectedId)) {
            warnings.add(String.format(
                    "Exclusive slot \"%s\" switched from \"%s\" to \"%s\".",
                    slotKey, inferredPrev, selectedId));
        }

        // Disable other plugins of same kind
        Map<String, Object> entries = pluginsConfig.get("entries") instanceof Map
                ? new HashMap<>((Map<String, Object>) pluginsConfig.get("entries"))
                : new HashMap<>();
        List<String> disabledIds = new ArrayList<>();

        if (registryPlugins != null) {
            for (PluginRecord plugin : registryPlugins) {
                if (plugin.getId().equals(selectedId))
                    continue;
                if (plugin.getKind() != selectedKind)
                    continue;

                Map<String, Object> entry = entries.get(plugin.getId()) instanceof Map
                        ? new HashMap<>((Map<String, Object>) entries.get(plugin.getId()))
                        : new HashMap<>();
                if (!Boolean.FALSE.equals(entry.get("enabled"))) {
                    entry.put("enabled", false);
                    entries.put(plugin.getId(), entry);
                    disabledIds.add(plugin.getId());
                }
            }
        }

        if (!disabledIds.isEmpty()) {
            Collections.sort(disabledIds);
            warnings.add(String.format(
                    "Disabled other \"%s\" slot plugins: %s.",
                    slotKey, String.join(", ", disabledIds)));
        }

        boolean changed = !selectedId.equals(prevSlot) || !disabledIds.isEmpty();
        if (!changed) {
            return new SlotSelectionResult(pluginsConfig, List.of(), false);
        }

        Map<String, Object> newConfig = new HashMap<>(pluginsConfig);
        newConfig.put("slots", slots);
        newConfig.put("entries", entries);
        return new SlotSelectionResult(newConfig, warnings, true);
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginRecord {
        private String id;
        private PluginKind kind;
    }
}
