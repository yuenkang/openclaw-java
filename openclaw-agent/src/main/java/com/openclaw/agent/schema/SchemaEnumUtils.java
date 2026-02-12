package com.openclaw.agent.schema;

import java.util.List;
import java.util.Map;

/**
 * JSON Schema enum helpers for tool parameter definitions.
 * Provides safe string-enum schema construction that avoids anyOf/oneOf
 * patterns which some providers reject.
 * Mirrors agents/schema/typebox.ts.
 */
public final class SchemaEnumUtils {

    private SchemaEnumUtils() {
    }

    /**
     * Build a required string-enum schema node.
     * Generates { "type": "string", "enum": [...values], "description": ... }.
     */
    public static Map<String, Object> stringEnum(List<String> values, String description) {
        if (description != null && !description.isBlank()) {
            return Map.of("type", "string", "enum", List.copyOf(values),
                    "description", description);
        }
        return Map.of("type", "string", "enum", List.copyOf(values));
    }

    /**
     * Build a required string-enum schema node without description.
     */
    public static Map<String, Object> stringEnum(List<String> values) {
        return stringEnum(values, null);
    }

    /**
     * Build a channel-target schema node (string type with description).
     */
    public static Map<String, Object> channelTargetSchema(String description) {
        String desc = description != null && !description.isBlank()
                ? description
                : "Channel target identifier (e.g. telegram:12345 or discord:guildId:channelId)";
        return Map.of("type", "string", "description", desc);
    }

    /**
     * Build a channel-targets array schema node.
     */
    public static Map<String, Object> channelTargetsSchema(String description) {
        return Map.of("type", "array", "items", channelTargetSchema(description));
    }
}
