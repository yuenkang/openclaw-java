package com.openclaw.common.config;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Config redaction — removes sensitive values (tokens, passwords, API keys)
 * from config objects and raw JSON text.
 * Corresponds to TypeScript's redact-snapshot.ts.
 */
public final class ConfigRedactor {

    private ConfigRedactor() {
    }

    /** Sentinel value replacing redacted fields. */
    public static final String REDACTED_SENTINEL = "__OPENCLAW_REDACTED__";

    private static final Pattern[] SENSITIVE_KEY_PATTERNS = {
            Pattern.compile("token", Pattern.CASE_INSENSITIVE),
            Pattern.compile("password", Pattern.CASE_INSENSITIVE),
            Pattern.compile("secret", Pattern.CASE_INSENSITIVE),
            Pattern.compile("api.?key", Pattern.CASE_INSENSITIVE),
    };

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Deep-walk an object and replace sensitive values with the redaction sentinel.
     */
    @SuppressWarnings("unchecked")
    public static Object redactObject(Object obj) {
        if (obj == null)
            return null;
        if (obj instanceof List<?> list) {
            return list.stream().map(ConfigRedactor::redactObject).toList();
        }
        if (!(obj instanceof Map))
            return obj;

        Map<String, Object> map = (Map<String, Object>) obj;
        Map<String, Object> result = new LinkedHashMap<>();
        for (var entry : map.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();
            if (isSensitiveKey(key) && value != null) {
                result.put(key, REDACTED_SENTINEL);
            } else if (value instanceof Map || value instanceof List) {
                result.put(key, redactObject(value));
            } else {
                result.put(key, value);
            }
        }
        return result;
    }

    /**
     * Collect all sensitive string values from a config object.
     */
    @SuppressWarnings("unchecked")
    public static List<String> collectSensitiveValues(Object obj) {
        List<String> values = new ArrayList<>();
        if (obj == null)
            return values;

        if (obj instanceof List<?> list) {
            for (Object item : list) {
                values.addAll(collectSensitiveValues(item));
            }
            return values;
        }
        if (!(obj instanceof Map))
            return values;

        Map<String, Object> map = (Map<String, Object>) obj;
        for (var entry : map.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();
            if (isSensitiveKey(key) && value instanceof String s && !s.isEmpty()) {
                values.add(s);
            } else if (value instanceof Map || value instanceof List) {
                values.addAll(collectSensitiveValues(value));
            }
        }
        return values;
    }

    /**
     * Replace known sensitive values in raw JSON5 text with the redaction sentinel.
     * Values are replaced longest-first to avoid partial matches.
     */
    public static String redactRawText(String raw, Object config) {
        List<String> sensitiveValues = new ArrayList<>(collectSensitiveValues(config));
        sensitiveValues.sort((a, b) -> b.length() - a.length());

        String result = raw;
        for (String value : sensitiveValues) {
            result = result.replace(value, REDACTED_SENTINEL);
        }
        return result;
    }

    /**
     * Deep-walk incoming and replace REDACTED_SENTINEL values with originals.
     * Used to restore credentials after a Web UI round-trip.
     *
     * @throws IllegalArgumentException if a redacted key has no original value
     */
    @SuppressWarnings("unchecked")
    public static Object restoreRedactedValues(Object incoming, Object original) {
        if (incoming == null)
            return null;

        if (incoming instanceof List<?> inList) {
            List<Object> origList = original instanceof List ? (List<Object>) original : List.of();
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < inList.size(); i++) {
                result.add(restoreRedactedValues(inList.get(i),
                        i < origList.size() ? origList.get(i) : null));
            }
            return result;
        }

        if (!(incoming instanceof Map))
            return incoming;

        Map<String, Object> origMap = (original instanceof Map)
                ? (Map<String, Object>) original
                : Map.of();

        Map<String, Object> inMap = (Map<String, Object>) incoming;
        Map<String, Object> result = new LinkedHashMap<>();

        for (var entry : inMap.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (isSensitiveKey(key) && REDACTED_SENTINEL.equals(value)) {
                if (!origMap.containsKey(key)) {
                    throw new IllegalArgumentException(
                            "config write rejected: \"" + key +
                                    "\" is redacted; set an explicit value instead of " + REDACTED_SENTINEL);
                }
                result.put(key, origMap.get(key));
            } else if (value instanceof Map || value instanceof List) {
                result.put(key, restoreRedactedValues(value, origMap.get(key)));
            } else {
                result.put(key, value);
            }
        }
        return result;
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    /**
     * Check if a key name likely holds a sensitive value.
     */
    public static boolean isSensitiveKey(String key) {
        for (Pattern p : SENSITIVE_KEY_PATTERNS) {
            if (p.matcher(key).find())
                return true;
        }
        return false;
    }
}
