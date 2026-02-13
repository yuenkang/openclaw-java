package com.openclaw.common.config;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Normalize ~ paths in path-ish config fields.
 * Corresponds to TypeScript's normalize-paths.ts.
 */
public final class NormalizeConfigPaths {

    private NormalizeConfigPaths() {
    }

    private static final Pattern PATH_VALUE_RE = Pattern.compile("^~(?=$|[/\\\\])");
    private static final Pattern PATH_KEY_RE = Pattern.compile("(dir|path|paths|file|root|workspace)$",
            Pattern.CASE_INSENSITIVE);
    private static final Set<String> PATH_LIST_KEYS = Set.of("paths", "pathPrepend");

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Normalize "~" paths in path-ish config fields.
     * Mutates the map in-place and returns it.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> normalizeConfigPaths(Map<String, Object> cfg) {
        if (cfg == null) {
            return cfg;
        }
        normalizeAny(null, cfg);
        return cfg;
    }

    // =========================================================================
    // Internal
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static Object normalizeAny(String key, Object value) {
        if (value instanceof String s) {
            return normalizeStringValue(key, s);
        }
        if (value instanceof List<?> list) {
            boolean normalizeChildren = key != null && PATH_LIST_KEYS.contains(key);
            for (int i = 0; i < list.size(); i++) {
                Object entry = list.get(i);
                if (entry instanceof String s) {
                    if (normalizeChildren) {
                        Object replaced = normalizeStringValue(key, s);
                        if (replaced != entry) {
                            ((List<Object>) list).set(i, replaced);
                        }
                    }
                } else if (entry instanceof Map || entry instanceof List) {
                    normalizeAny(null, entry);
                }
            }
            return value;
        }
        if (value instanceof Map<?, ?> map) {
            for (var entry : ((Map<String, Object>) map).entrySet()) {
                Object next = normalizeAny(entry.getKey(), entry.getValue());
                if (next != entry.getValue()) {
                    entry.setValue(next);
                }
            }
            return value;
        }
        return value;
    }

    private static Object normalizeStringValue(String key, String value) {
        if (!PATH_VALUE_RE.matcher(value.trim()).find()) {
            return value;
        }
        if (key == null) {
            return value;
        }
        if (PATH_KEY_RE.matcher(key).find() || PATH_LIST_KEYS.contains(key)) {
            return resolveUserPath(value);
        }
        return value;
    }

    private static String resolveUserPath(String p) {
        if (p.startsWith("~")) {
            return System.getProperty("user.home") + p.substring(1);
        }
        return p;
    }
}
