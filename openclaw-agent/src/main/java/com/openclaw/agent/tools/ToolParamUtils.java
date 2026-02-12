package com.openclaw.agent.tools;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.ArrayList;
import java.util.List;

/**
 * Utility functions for reading tool parameters from JSON input.
 * Corresponds to TypeScript's tools/common.ts (readStringParam,
 * readNumberParam, readStringArrayParam, jsonResult).
 */
public final class ToolParamUtils {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private ToolParamUtils() {
    }

    // --- String param ---

    /**
     * Read a string parameter from the JSON params node.
     *
     * @param params   JSON parameter node
     * @param key      parameter key
     * @param required if true, throws when missing
     * @return trimmed string value, or null if absent and not required
     */
    public static String readStringParam(JsonNode params, String key, boolean required) {
        if (params == null || !params.has(key)) {
            if (required)
                throw new IllegalArgumentException(key + " required");
            return null;
        }
        JsonNode node = params.get(key);
        if (!node.isTextual()) {
            if (required)
                throw new IllegalArgumentException(key + " required");
            return null;
        }
        String value = node.asText().trim();
        if (value.isEmpty()) {
            if (required)
                throw new IllegalArgumentException(key + " required");
            return null;
        }
        return value;
    }

    public static String readStringParam(JsonNode params, String key) {
        return readStringParam(params, key, false);
    }

    // --- Number param ---

    /**
     * Read a numeric parameter (integer or double).
     */
    public static Number readNumberParam(JsonNode params, String key, boolean required) {
        if (params == null || !params.has(key)) {
            if (required)
                throw new IllegalArgumentException(key + " required");
            return null;
        }
        JsonNode node = params.get(key);
        if (node.isNumber()) {
            return node.isInt() || node.isLong() ? node.asLong() : node.asDouble();
        }
        if (node.isTextual()) {
            String trimmed = node.asText().trim();
            if (!trimmed.isEmpty()) {
                try {
                    return Double.parseDouble(trimmed);
                } catch (NumberFormatException ignored) {
                }
            }
        }
        if (required)
            throw new IllegalArgumentException(key + " required");
        return null;
    }

    public static Number readNumberParam(JsonNode params, String key) {
        return readNumberParam(params, key, false);
    }

    public static int readIntParam(JsonNode params, String key, int defaultValue) {
        Number n = readNumberParam(params, key, false);
        return n != null ? n.intValue() : defaultValue;
    }

    // --- String array param ---

    /**
     * Read a string-array parameter. Accepts both JSON array and single string.
     */
    public static List<String> readStringArrayParam(JsonNode params, String key, boolean required) {
        if (params == null || !params.has(key)) {
            if (required)
                throw new IllegalArgumentException(key + " required");
            return null;
        }
        JsonNode node = params.get(key);
        if (node.isArray()) {
            List<String> values = new ArrayList<>();
            for (JsonNode item : node) {
                if (item.isTextual()) {
                    String v = item.asText().trim();
                    if (!v.isEmpty())
                        values.add(v);
                }
            }
            if (values.isEmpty()) {
                if (required)
                    throw new IllegalArgumentException(key + " required");
                return null;
            }
            return values;
        }
        if (node.isTextual()) {
            String value = node.asText().trim();
            if (value.isEmpty()) {
                if (required)
                    throw new IllegalArgumentException(key + " required");
                return null;
            }
            return List.of(value);
        }
        if (required)
            throw new IllegalArgumentException(key + " required");
        return null;
    }

    public static List<String> readStringArrayParam(JsonNode params, String key) {
        return readStringArrayParam(params, key, false);
    }

    // --- Boolean param ---

    public static boolean readBoolParam(JsonNode params, String key, boolean defaultValue) {
        if (params == null || !params.has(key))
            return defaultValue;
        JsonNode node = params.get(key);
        if (node.isBoolean())
            return node.asBoolean();
        if (node.isTextual())
            return "true".equalsIgnoreCase(node.asText().trim());
        return defaultValue;
    }

    // --- JSON result builder ---

    /**
     * Build a standard JSON tool result (corresponds to TS jsonResult).
     */
    public static AgentTool.ToolResult jsonResult(Object payload) {
        try {
            String json = MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(payload);
            return AgentTool.ToolResult.ok(json, payload);
        } catch (JsonProcessingException e) {
            return AgentTool.ToolResult.ok(String.valueOf(payload));
        }
    }

    /**
     * Build a JSON result from key-value pairs.
     */
    public static AgentTool.ToolResult jsonResult(String... kvPairs) {
        ObjectNode node = MAPPER.createObjectNode();
        for (int i = 0; i + 1 < kvPairs.length; i += 2) {
            node.put(kvPairs[i], kvPairs[i + 1]);
        }
        return AgentTool.ToolResult.ok(node.toPrettyString(), node);
    }

    /**
     * Build an error JSON result.
     */
    public static AgentTool.ToolResult errorResult(String error) {
        ObjectNode node = MAPPER.createObjectNode();
        node.put("status", "error");
        node.put("error", error);
        return AgentTool.ToolResult.fail(node.toPrettyString());
    }

    /**
     * Serialize an object to a pretty-printed JSON string.
     */
    public static String toJsonString(Object value) {
        try {
            return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(value);
        } catch (JsonProcessingException e) {
            return String.valueOf(value);
        }
    }

    /**
     * Read an integer parameter, returning null if absent.
     */
    public static Integer readIntegerParam(JsonNode params, String key) {
        Number n = readNumberParam(params, key, false);
        return n != null ? n.intValue() : null;
    }
}
