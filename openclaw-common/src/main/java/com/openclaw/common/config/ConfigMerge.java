package com.openclaw.common.config;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Config merge utilities — section merge and JSON merge-patch.
 * Corresponds to TypeScript's merge-config.ts + merge-patch.ts.
 */
public final class ConfigMerge {

    private ConfigMerge() {
    }

    // =========================================================================
    // Section merge (merge-config.ts)
    // =========================================================================

    /**
     * Merge a patch into a base configuration section.
     * Null values in patch are ignored unless the key is in unsetOnNull.
     *
     * @param base        base config section (may be null)
     * @param patch       patch values to apply
     * @param unsetOnNull keys to delete when patch value is null
     */
    public static Map<String, Object> mergeConfigSection(
            Map<String, Object> base,
            Map<String, Object> patch,
            List<String> unsetOnNull) {

        Map<String, Object> next = new LinkedHashMap<>();
        if (base != null)
            next.putAll(base);

        for (var entry : patch.entrySet()) {
            if (entry.getValue() == null) {
                if (unsetOnNull != null && unsetOnNull.contains(entry.getKey())) {
                    next.remove(entry.getKey());
                }
                continue;
            }
            next.put(entry.getKey(), entry.getValue());
        }
        return next;
    }

    public static Map<String, Object> mergeConfigSection(
            Map<String, Object> base, Map<String, Object> patch) {
        return mergeConfigSection(base, patch, null);
    }

    // =========================================================================
    // JSON Merge-Patch (RFC 7396) — merge-patch.ts
    // =========================================================================

    /**
     * Apply a JSON merge-patch to a base value.
     * - null patch values delete the key
     * - object patch values are recursively merged
     * - non-object patch replaces base entirely
     */
    @SuppressWarnings("unchecked")
    public static Object applyMergePatch(Object base, Object patch) {
        if (!isPlainObject(patch)) {
            return patch;
        }

        Map<String, Object> patchMap = (Map<String, Object>) patch;
        Map<String, Object> result;
        if (isPlainObject(base)) {
            result = new LinkedHashMap<>((Map<String, Object>) base);
        } else {
            result = new LinkedHashMap<>();
        }

        for (var entry : patchMap.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (value == null) {
                result.remove(key);
                continue;
            }
            if (isPlainObject(value)) {
                Object baseValue = result.get(key);
                result.put(key, applyMergePatch(
                        isPlainObject(baseValue) ? baseValue : new LinkedHashMap<>(), value));
                continue;
            }
            result.put(key, value);
        }

        return result;
    }

    private static boolean isPlainObject(Object value) {
        return value instanceof Map;
    }
}
