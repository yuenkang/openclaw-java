package com.openclaw.common.config;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Runtime config overrides: set/unset/get/reset/apply.
 * Corresponds to TypeScript's runtime-overrides.ts.
 */
public final class ConfigRuntimeOverrides {

    private ConfigRuntimeOverrides() {
    }

    // Mutable override tree
    private static volatile Map<String, Object> overrides = new LinkedHashMap<>();

    // =========================================================================
    // Types
    // =========================================================================

    public record SetResult(boolean ok, String error) {
    }

    public record UnsetResult(boolean ok, boolean removed, String error) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Get the current override tree.
     */
    public static Map<String, Object> getConfigOverrides() {
        return overrides;
    }

    /**
     * Reset all overrides.
     */
    public static void resetConfigOverrides() {
        overrides = new LinkedHashMap<>();
    }

    /**
     * Set a config override at the given dotted path.
     */
    public static SetResult setConfigOverride(String pathRaw, Object value) {
        var parsed = ConfigPaths.parseConfigPath(pathRaw);
        if (!parsed.ok() || parsed.path() == null) {
            return new SetResult(false, parsed.error() != null ? parsed.error() : "Invalid path.");
        }
        ConfigPaths.setConfigValueAtPath(overrides, parsed.path(), value);
        return new SetResult(true, null);
    }

    /**
     * Unset a config override at the given dotted path.
     */
    public static UnsetResult unsetConfigOverride(String pathRaw) {
        var parsed = ConfigPaths.parseConfigPath(pathRaw);
        if (!parsed.ok() || parsed.path() == null) {
            return new UnsetResult(false, false,
                    parsed.error() != null ? parsed.error() : "Invalid path.");
        }
        boolean removed = ConfigPaths.unsetConfigValueAtPath(overrides, parsed.path());
        return new UnsetResult(true, removed, null);
    }

    /**
     * Apply the current overrides on top of a config map.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> applyConfigOverrides(Map<String, Object> cfg) {
        if (overrides.isEmpty()) {
            return cfg;
        }
        return (Map<String, Object>) mergeOverrides(cfg, overrides);
    }

    // =========================================================================
    // Internal
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static Object mergeOverrides(Object base, Object override) {
        if (!isPlainMap(base) || !isPlainMap(override)) {
            return override;
        }
        Map<String, Object> baseMap = (Map<String, Object>) base;
        Map<String, Object> overrideMap = (Map<String, Object>) override;
        Map<String, Object> next = new LinkedHashMap<>(baseMap);
        for (var entry : overrideMap.entrySet()) {
            if (entry.getValue() == null) {
                continue;
            }
            next.put(entry.getKey(),
                    mergeOverrides(baseMap.get(entry.getKey()), entry.getValue()));
        }
        return next;
    }

    private static boolean isPlainMap(Object value) {
        return value instanceof Map;
    }
}
