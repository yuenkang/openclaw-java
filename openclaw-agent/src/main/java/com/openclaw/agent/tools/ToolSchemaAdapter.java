package com.openclaw.agent.tools;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Normalize tool parameter JSON schemas for cross-provider compatibility.
 * Corresponds to TypeScript's pi-tools.schema.ts.
 *
 * <p>
 * Key responsibilities:
 * </p>
 * <ul>
 * <li>Flatten union schemas (anyOf/oneOf) into a single {@code type: "object"}
 * schema</li>
 * <li>Clean schemas of keywords unsupported by Gemini</li>
 * <li>Ensure top-level {@code type: "object"} for OpenAI compatibility</li>
 * </ul>
 */
@Slf4j
public final class ToolSchemaAdapter {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * JSON Schema keywords that Gemini does not support and should be removed.
     */
    private static final Set<String> GEMINI_UNSUPPORTED = Set.of(
            "$schema", "$id", "$ref", "$defs", "definitions",
            "if", "then", "else", "not", "dependentSchemas",
            "dependentRequired", "unevaluatedProperties",
            "unevaluatedItems", "contentMediaType", "contentEncoding",
            "examples", "deprecated", "readOnly", "writeOnly",
            "externalDocs", "xml", "discriminator");

    private ToolSchemaAdapter() {
    }

    /**
     * Normalize a tool's parameter schema for cross-provider compatibility.
     * Handles union flattening and Gemini cleanup.
     */
    public static JsonNode normalizeToolParameters(JsonNode schema) {
        if (schema == null || !schema.isObject()) {
            return schema;
        }
        ObjectNode obj = (ObjectNode) schema.deepCopy();

        // If already type:object with properties (no top-level anyOf) — just clean
        if (obj.has("type") && obj.has("properties") && !obj.has("anyOf")) {
            return cleanSchemaForGemini(obj);
        }

        // If missing type but has object-ish fields, force type:object
        if (!obj.has("type")
                && (obj.has("properties") || obj.has("required"))
                && !obj.has("anyOf") && !obj.has("oneOf")) {
            obj.put("type", "object");
            return cleanSchemaForGemini(obj);
        }

        // Flatten anyOf/oneOf union
        String variantKey = obj.has("anyOf") ? "anyOf" : (obj.has("oneOf") ? "oneOf" : null);
        if (variantKey == null) {
            return cleanSchemaForGemini(obj);
        }

        JsonNode variants = obj.get(variantKey);
        if (!variants.isArray()) {
            return cleanSchemaForGemini(obj);
        }

        ObjectNode mergedProperties = MAPPER.createObjectNode();
        Map<String, Integer> requiredCounts = new LinkedHashMap<>();
        int objectVariants = 0;

        for (JsonNode entry : variants) {
            if (!entry.isObject() || !entry.has("properties")) {
                continue;
            }
            objectVariants++;
            ObjectNode props = (ObjectNode) entry.get("properties");
            Iterator<String> fieldNames = props.fieldNames();
            while (fieldNames.hasNext()) {
                String key = fieldNames.next();
                if (!mergedProperties.has(key)) {
                    mergedProperties.set(key, props.get(key).deepCopy());
                } else {
                    mergedProperties.set(key,
                            mergePropertySchemas(mergedProperties.get(key), props.get(key)));
                }
            }
            if (entry.has("required") && entry.get("required").isArray()) {
                for (JsonNode req : entry.get("required")) {
                    if (req.isTextual()) {
                        requiredCounts.merge(req.asText(), 1, Integer::sum);
                    }
                }
            }
        }

        // Build merged required
        ArrayNode mergedRequired = MAPPER.createArrayNode();
        if (obj.has("required") && obj.get("required").isArray()) {
            for (JsonNode req : obj.get("required")) {
                if (req.isTextual())
                    mergedRequired.add(req.asText());
            }
        } else if (objectVariants > 0) {
            int ov = objectVariants;
            requiredCounts.entrySet().stream()
                    .filter(e -> e.getValue() == ov)
                    .forEach(e -> mergedRequired.add(e.getKey()));
        }

        // Build flattened schema
        ObjectNode result = MAPPER.createObjectNode();
        result.put("type", "object");
        if (obj.has("title"))
            result.set("title", obj.get("title"));
        if (obj.has("description"))
            result.set("description", obj.get("description"));
        if (mergedProperties.size() > 0) {
            result.set("properties", mergedProperties);
        } else if (obj.has("properties")) {
            result.set("properties", obj.get("properties"));
        }
        if (mergedRequired.size() > 0) {
            result.set("required", mergedRequired);
        }
        if (obj.has("additionalProperties")) {
            result.set("additionalProperties", obj.get("additionalProperties"));
        } else {
            result.put("additionalProperties", true);
        }

        return cleanSchemaForGemini(result);
    }

    /**
     * Remove JSON Schema keywords unsupported by Gemini.
     */
    public static JsonNode cleanSchemaForGemini(JsonNode schema) {
        if (schema == null || !schema.isObject()) {
            return schema;
        }
        ObjectNode obj = (ObjectNode) schema.deepCopy();
        GEMINI_UNSUPPORTED.forEach(obj::remove);

        // Recursively clean properties
        if (obj.has("properties") && obj.get("properties").isObject()) {
            ObjectNode props = (ObjectNode) obj.get("properties");
            Iterator<String> names = props.fieldNames();
            List<String> keys = new ArrayList<>();
            names.forEachRemaining(keys::add);
            for (String key : keys) {
                props.set(key, cleanSchemaForGemini(props.get(key)));
            }
        }

        // Recursively clean items
        if (obj.has("items") && obj.get("items").isObject()) {
            obj.set("items", cleanSchemaForGemini(obj.get("items")));
        }

        // Recursively clean anyOf/oneOf/allOf if still present
        for (String arrayKey : List.of("anyOf", "oneOf", "allOf")) {
            if (obj.has(arrayKey) && obj.get(arrayKey).isArray()) {
                ArrayNode cleaned = MAPPER.createArrayNode();
                for (JsonNode item : obj.get(arrayKey)) {
                    cleaned.add(cleanSchemaForGemini(item));
                }
                obj.set(arrayKey, cleaned);
            }
        }

        return obj;
    }

    /**
     * Merge two property schemas, combining enum values.
     */
    static JsonNode mergePropertySchemas(JsonNode existing, JsonNode incoming) {
        if (existing == null)
            return incoming;
        if (incoming == null)
            return existing;

        List<String> existingEnum = extractEnumValues(existing);
        List<String> incomingEnum = extractEnumValues(incoming);

        if (existingEnum != null || incomingEnum != null) {
            ObjectNode merged = MAPPER.createObjectNode();
            // Copy metadata from existing/incoming
            for (JsonNode source : List.of(existing, incoming)) {
                if (source.isObject()) {
                    for (String metaKey : List.of("title", "description", "default")) {
                        if (!merged.has(metaKey) && source.has(metaKey)) {
                            merged.set(metaKey, source.get(metaKey));
                        }
                    }
                }
            }
            // Merge enum values
            Set<String> values = new LinkedHashSet<>();
            if (existingEnum != null)
                values.addAll(existingEnum);
            if (incomingEnum != null)
                values.addAll(incomingEnum);

            // Determine type from values
            if (!values.isEmpty()) {
                merged.put("type", "string");
            }
            ArrayNode enumArray = MAPPER.createArrayNode();
            values.forEach(enumArray::add);
            merged.set("enum", enumArray);
            return merged;
        }

        return existing;
    }

    /**
     * Extract enum values from a schema node.
     */
    static List<String> extractEnumValues(JsonNode schema) {
        if (schema == null || !schema.isObject())
            return null;

        if (schema.has("enum") && schema.get("enum").isArray()) {
            List<String> values = new ArrayList<>();
            for (JsonNode v : schema.get("enum")) {
                values.add(v.asText());
            }
            return values;
        }
        if (schema.has("const")) {
            return List.of(schema.get("const").asText());
        }

        // Check anyOf/oneOf variants
        JsonNode variants = schema.has("anyOf") ? schema.get("anyOf")
                : schema.has("oneOf") ? schema.get("oneOf") : null;
        if (variants != null && variants.isArray()) {
            List<String> values = new ArrayList<>();
            for (JsonNode variant : variants) {
                List<String> extracted = extractEnumValues(variant);
                if (extracted != null)
                    values.addAll(extracted);
            }
            return values.isEmpty() ? null : values;
        }

        return null;
    }
}
