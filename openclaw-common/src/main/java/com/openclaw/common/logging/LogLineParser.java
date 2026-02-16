package com.openclaw.common.logging;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.Map;

/**
 * Parser for JSON-formatted log lines (as written by the TS tslog file
 * transport).
 * Translates TS logging/parse-log-line.ts.
 *
 */
public final class LogLineParser {

    private LogLineParser() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final TypeReference<Map<String, Object>> MAP_TYPE = new TypeReference<>() {
    };

    /**
     * Parsed representation of a single JSON log line.
     */
    public record ParsedLogLine(
            String time,
            String level,
            String subsystem,
            String module,
            String message,
            String raw) {
    }

    /**
     * Parse a single JSON log line.
     *
     * @param raw the raw JSON string
     * @return parsed log line, or null if parsing fails
     */
    public static ParsedLogLine parse(String raw) {
        if (raw == null || raw.isBlank()) {
            return null;
        }
        try {
            Map<String, Object> parsed = MAPPER.readValue(raw, MAP_TYPE);
            Map<String, Object> meta = asMap(parsed.get("_meta"));

            // Extract time
            String time = asString(parsed.get("time"));
            if (time == null && meta != null) {
                time = asString(meta.get("date"));
            }

            // Extract level
            String level = null;
            if (meta != null) {
                String levelRaw = asString(meta.get("logLevelName"));
                if (levelRaw != null) {
                    level = levelRaw.toLowerCase();
                }
            }

            // Extract subsystem/module from _meta.name (JSON-encoded object)
            String subsystem = null;
            String module = null;
            if (meta != null) {
                var nameMeta = parseMetaName(meta.get("name"));
                subsystem = nameMeta[0];
                module = nameMeta[1];
            }

            // Extract message from numeric keys (tslog convention)
            String message = extractMessage(parsed);

            return new ParsedLogLine(time, level, subsystem, module, message, raw);
        } catch (Exception e) {
            return null;
        }
    }

    // -----------------------------------------------------------------------
    // Internals
    // -----------------------------------------------------------------------

    private static String extractMessage(Map<String, Object> value) {
        StringBuilder parts = new StringBuilder();
        for (var entry : value.entrySet()) {
            if (!entry.getKey().matches("\\d+")) {
                continue;
            }
            if (parts.length() > 0) {
                parts.append(" ");
            }
            Object item = entry.getValue();
            if (item instanceof String s) {
                parts.append(s);
            } else if (item != null) {
                try {
                    parts.append(MAPPER.writeValueAsString(item));
                } catch (Exception e) {
                    parts.append(item);
                }
            }
        }
        return parts.toString();
    }

    private static String[] parseMetaName(Object raw) {
        String[] result = { null, null }; // [subsystem, module]
        if (!(raw instanceof String nameStr)) {
            return result;
        }
        try {
            Map<String, Object> parsed = MAPPER.readValue(nameStr, MAP_TYPE);
            if (parsed.get("subsystem") instanceof String s)
                result[0] = s;
            if (parsed.get("module") instanceof String m)
                result[1] = m;
        } catch (Exception ignored) {
            // not JSON — ignore
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> asMap(Object value) {
        if (value instanceof Map<?, ?> m) {
            return (Map<String, Object>) m;
        }
        return null;
    }

    private static String asString(Object value) {
        if (value instanceof String s) {
            return s;
        }
        return null;
    }
}
