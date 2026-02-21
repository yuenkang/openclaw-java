package com.openclaw.plugin.config;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Plugin JSON schema validator — validates plugin config against
 * JSON Schema definitions using Jackson.
 * Corresponds to TypeScript's plugins/schema-validator.ts (which uses AJV).
 */
@Slf4j
public final class PluginSchemaValidator {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final Map<String, JsonNode> schemaCache = new ConcurrentHashMap<>();

    private PluginSchemaValidator() {
    }

    public sealed interface ValidationResult {
        record Ok() implements ValidationResult {
        }

        record Fail(List<String> errors) implements ValidationResult {
        }
    }

    /**
     * Validate a value against a JSON Schema.
     *
     * @param schema   JSON Schema as a Map
     * @param cacheKey unique key for caching the compiled schema
     * @param value    value to validate
     */
    @SuppressWarnings("unchecked")
    public static ValidationResult validate(
            Map<String, Object> schema, String cacheKey, Object value) {
        if (schema == null || schema.isEmpty()) {
            return new ValidationResult.Ok();
        }

        try {
            JsonNode schemaNode = schemaCache.computeIfAbsent(cacheKey,
                    k -> MAPPER.valueToTree(schema));

            // Basic type validation based on JSON Schema "type" field
            String type = schema.get("type") instanceof String s ? s : null;
            List<String> errors = new ArrayList<>();

            if ("object".equals(type) && value != null) {
                if (!(value instanceof Map)) {
                    errors.add("<root>: expected object");
                    return new ValidationResult.Fail(errors);
                }

                Map<String, Object> valueMap = (Map<String, Object>) value;
                Map<String, Object> properties = schema.get("properties") instanceof Map
                        ? (Map<String, Object>) schema.get("properties")
                        : Map.of();

                // Check required fields
                if (schema.get("required") instanceof List<?> required) {
                    for (Object r : required) {
                        if (r instanceof String key && !valueMap.containsKey(key)) {
                            errors.add(key + ": required");
                        }
                    }
                }

                // Check additionalProperties
                if (Boolean.FALSE.equals(schema.get("additionalProperties"))) {
                    for (String key : valueMap.keySet()) {
                        if (!properties.containsKey(key)) {
                            errors.add(key + ": unexpected property");
                        }
                    }
                }
            }

            return errors.isEmpty() ? new ValidationResult.Ok()
                    : new ValidationResult.Fail(errors);
        } catch (Exception e) {
            log.warn("Schema validation error: {}", e.getMessage());
            return new ValidationResult.Fail(List.of("schema validation error: " + e.getMessage()));
        }
    }
}
