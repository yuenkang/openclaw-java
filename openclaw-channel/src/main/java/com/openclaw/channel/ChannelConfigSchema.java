package com.openclaw.channel;

import java.util.*;

/**
 * Channel config schema builder.
 * Builds JSON Schema objects from channel config definitions.
 * Corresponds to TypeScript's channels/plugins/config-schema.ts.
 */
public final class ChannelConfigSchema {

    private ChannelConfigSchema() {
    }

    /**
     * Build a JSON Schema for a channel's config.
     *
     * @param properties map of property name to property schema definitions
     * @return a JSON Schema object (as a Map)
     */
    public static Map<String, Object> buildChannelConfigSchema(
            Map<String, Map<String, Object>> properties) {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("$schema", "http://json-schema.org/draft-07/schema#");
        schema.put("type", "object");
        schema.put("properties", properties);
        return schema;
    }

    /**
     * Create a string property schema.
     */
    public static Map<String, Object> stringProperty(String description) {
        Map<String, Object> prop = new LinkedHashMap<>();
        prop.put("type", "string");
        if (description != null)
            prop.put("description", description);
        return prop;
    }

    /**
     * Create a boolean property schema.
     */
    public static Map<String, Object> booleanProperty(String description) {
        Map<String, Object> prop = new LinkedHashMap<>();
        prop.put("type", "boolean");
        if (description != null)
            prop.put("description", description);
        return prop;
    }

    /**
     * Create a string array property schema.
     */
    public static Map<String, Object> stringArrayProperty(String description) {
        Map<String, Object> prop = new LinkedHashMap<>();
        prop.put("type", "array");
        prop.put("items", Map.of("type", "string"));
        if (description != null)
            prop.put("description", description);
        return prop;
    }

    /**
     * Create an enum property schema.
     */
    public static Map<String, Object> enumProperty(String description, List<String> values) {
        Map<String, Object> prop = new LinkedHashMap<>();
        prop.put("type", "string");
        prop.put("enum", values);
        if (description != null)
            prop.put("description", description);
        return prop;
    }
}
