package com.openclaw.autoreply.reply;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.regex.Pattern;

/**
 * Parse raw config value strings into typed values
 * (booleans, numbers, JSON, quoted strings, or plain text).
 * Mirrors {@code auto-reply/reply/config-value.ts}.
 */
public final class ConfigValue {

    private ConfigValue() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final Pattern NUMBER_RE = Pattern.compile("^-?\\d+(\\.\\d+)?$");

    /** Result of parsing a config value. */
    public record ParseResult(Object value, String error) {
        public boolean hasError() {
            return error != null;
        }

        public boolean hasValue() {
            return value != null && error == null;
        }
    }

    /**
     * Parse a raw config value string into a typed Java object.
     * Returns one of: Boolean, Number, null, String, List, Map, or an error.
     */
    public static ParseResult parseConfigValue(String raw) {
        if (raw == null || raw.isBlank()) {
            return new ParseResult(null, "Missing value.");
        }
        String trimmed = raw.trim();

        // JSON object or array
        if (trimmed.startsWith("{") || trimmed.startsWith("[")) {
            try {
                Object parsed = MAPPER.readValue(trimmed, Object.class);
                return new ParseResult(parsed, null);
            } catch (Exception e) {
                return new ParseResult(null, "Invalid JSON: " + e.getMessage());
            }
        }

        // Booleans
        if ("true".equals(trimmed))
            return new ParseResult(true, null);
        if ("false".equals(trimmed))
            return new ParseResult(false, null);

        // Null
        if ("null".equals(trimmed))
            return new ParseResult(null, null);

        // Numbers
        if (NUMBER_RE.matcher(trimmed).matches()) {
            try {
                double num = Double.parseDouble(trimmed);
                if (Double.isFinite(num)) {
                    // Return int if whole number
                    if (num == Math.floor(num) && !trimmed.contains(".")) {
                        return new ParseResult((long) num, null);
                    }
                    return new ParseResult(num, null);
                }
            } catch (NumberFormatException ignored) {
                // fall through
            }
        }

        // Quoted strings
        if ((trimmed.startsWith("\"") && trimmed.endsWith("\"")) ||
                (trimmed.startsWith("'") && trimmed.endsWith("'"))) {
            try {
                String unquoted = MAPPER.readValue(trimmed, String.class);
                return new ParseResult(unquoted, null);
            } catch (Exception e) {
                // Strip quotes manually
                String unquoted = trimmed.substring(1, trimmed.length() - 1);
                return new ParseResult(unquoted, null);
            }
        }

        // Plain string
        return new ParseResult(trimmed, null);
    }
}
