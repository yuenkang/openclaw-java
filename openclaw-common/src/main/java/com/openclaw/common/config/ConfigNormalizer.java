package com.openclaw.common.config;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Normalize "~" paths in path-ish config fields.
 * Corresponds to TypeScript's normalize-paths.ts.
 */
public final class ConfigNormalizer {

    private ConfigNormalizer() {
    }

    /** Matches a value starting with ~ followed by / or end of string. */
    private static final Pattern PATH_VALUE_RE = Pattern.compile("^~(?=$|[/\\\\])");

    /** Matches keys that typically contain paths. */
    private static final Pattern PATH_KEY_RE = Pattern.compile("(dir|path|paths|file|root|workspace)$",
            Pattern.CASE_INSENSITIVE);

    /** Keys whose list values should also be path-normalized. */
    private static final Set<String> PATH_LIST_KEYS = Set.of("paths", "pathPrepend");

    /**
     * Normalize "~" paths in path-ish config fields throughout a config map.
     * Mutates the map in place and returns it.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> normalizeConfigPaths(Map<String, Object> cfg) {
        if (cfg == null)
            return cfg;
        normalizeAny(null, cfg);
        return cfg;
    }

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
                    Object normalized = normalizeChildren ? normalizeStringValue(key, s) : s;
                    if (!normalized.equals(entry)) {
                        ((List<Object>) list).set(i, normalized);
                    }
                } else if (entry instanceof Map) {
                    normalizeAny(null, entry);
                } else if (entry instanceof List) {
                    normalizeAny(null, entry);
                }
            }
            return list;
        }

        if (value instanceof Map<?, ?> map) {
            for (var entry : ((Map<String, Object>) map).entrySet()) {
                Object childVal = entry.getValue();
                Object normalized = normalizeAny(entry.getKey(), childVal);
                if (normalized != childVal) {
                    entry.setValue(normalized);
                }
            }
            return map;
        }

        return value;
    }

    private static Object normalizeStringValue(String key, String value) {
        String trimmed = value.trim();
        if (!PATH_VALUE_RE.matcher(trimmed).find()) {
            return value;
        }
        if (key == null) {
            return value;
        }
        if (PATH_KEY_RE.matcher(key).find() || PATH_LIST_KEYS.contains(key)) {
            return ConfigPaths.resolveUserPath(value).toString();
        }
        return value;
    }
}
