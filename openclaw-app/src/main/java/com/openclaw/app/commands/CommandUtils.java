package com.openclaw.app.commands;

import java.util.List;
import java.util.Map;

/**
 * Shared utility methods used across command handlers.
 */
public final class CommandUtils {

    private CommandUtils() {
    }

    /**
     * Extract text content from a message content field (String or List of content
     * parts).
     */
    public static String extractTextFromContent(Object content) {
        if (content instanceof String s) {
            return s;
        }
        if (content instanceof List<?> contentList) {
            for (Object item : contentList) {
                if (item instanceof Map<?, ?> contentItem && "text".equals(contentItem.get("type"))) {
                    return String.valueOf(contentItem.get("text"));
                }
            }
        }
        return "";
    }

    /** Parse a config value string to its appropriate type. */
    public static Object parseConfigValue(String value) {
        if ("true".equalsIgnoreCase(value))
            return true;
        if ("false".equalsIgnoreCase(value))
            return false;
        if ("null".equalsIgnoreCase(value))
            return null;
        try {
            return Long.parseLong(value);
        } catch (NumberFormatException ignored) {
        }
        try {
            return Double.parseDouble(value);
        } catch (NumberFormatException ignored) {
        }
        return value;
    }

    /** Format override map recursively for display. */
    @SuppressWarnings("unchecked")
    public static void formatOverrides(StringBuilder sb, Map<String, Object> map, String prefix) {
        for (var entry : map.entrySet()) {
            String key = prefix.isEmpty() ? entry.getKey() : prefix + "." + entry.getKey();
            if (entry.getValue() instanceof Map<?, ?> nested) {
                formatOverrides(sb, (Map<String, Object>) nested, key);
            } else {
                sb.append(String.format("`%s` = `%s`\n", key, entry.getValue()));
            }
        }
    }

    /** Resolve a dotted path to a value in a nested map. */
    @SuppressWarnings("unchecked")
    public static Object resolveNestedValue(Map<String, Object> map, String path) {
        String[] parts = path.split("\\.");
        Object current = map;
        for (String part : parts) {
            if (current instanceof Map<?, ?> m) {
                current = m.get(part);
            } else {
                return null;
            }
        }
        return current;
    }
}
