package com.openclaw.common.config;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Runtime configuration overrides tree.
 * Supports set/unset/apply for in-memory config overrides.
 * Corresponds to TypeScript's runtime-overrides.ts.
 */
public final class ConfigOverrides {

    private ConfigOverrides() {
    }

    private static Map<String, Object> overrides = new LinkedHashMap<>();

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Get the current overrides tree.
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
     * Set a config override at a dot-notation path.
     */
    public static OperationResult setConfigOverride(String pathRaw, Object value) {
        var parsed = ConfigPaths.parseConfigPath(pathRaw);
        if (!parsed.ok()) {
            return new OperationResult(false, parsed.error());
        }
        ConfigPaths.setConfigValueAtPath(overrides, parsed.path(), value);
        return new OperationResult(true, null);
    }

    /**
     * Remove a config override at a dot-notation path.
     */
    public static UnsetResult unsetConfigOverride(String pathRaw) {
        var parsed = ConfigPaths.parseConfigPath(pathRaw);
        if (!parsed.ok()) {
            return new UnsetResult(false, false, parsed.error());
        }
        boolean removed = ConfigPaths.unsetConfigValueAtPath(overrides, parsed.path());
        return new UnsetResult(true, removed, null);
    }

    /**
     * Apply overrides on top of a config map.
     * Returns a new merged map (does not mutate the input).
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> applyConfigOverrides(Map<String, Object> cfg) {
        if (overrides.isEmpty()) {
            return cfg;
        }
        return (Map<String, Object>) mergeOverrides(cfg, overrides);
    }

    // =========================================================================
    // Result types
    // =========================================================================

    public record OperationResult(boolean ok, String error) {
    }

    public record UnsetResult(boolean ok, boolean removed, String error) {
    }

    // =========================================================================
    // Internal
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static Object mergeOverrides(Object base, Object override) {
        if (!(base instanceof Map) || !(override instanceof Map)) {
            return override;
        }
        Map<String, Object> baseMap = (Map<String, Object>) base;
        Map<String, Object> overrideMap = (Map<String, Object>) override;
        Map<String, Object> next = new LinkedHashMap<>(baseMap);
        for (var entry : overrideMap.entrySet()) {
            if (entry.getValue() == null) {
                continue;
            }
            Object baseChild = baseMap.get(entry.getKey());
            next.put(entry.getKey(), mergeOverrides(baseChild, entry.getValue()));
        }
        return next;
    }
}
