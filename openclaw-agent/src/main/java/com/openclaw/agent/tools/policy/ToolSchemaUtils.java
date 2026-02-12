package com.openclaw.agent.tools.policy;

import java.util.*;

/**
 * JSON Schema normalization for tool parameters.
 * Corresponds to TypeScript pi-tools.schema.ts.
 */
public final class ToolSchemaUtils {

    private ToolSchemaUtils() {
    }

    /**
     * Normalize a tool's JSON Schema parameters:
     * - Flatten anyOf/oneOf unions into a merged object schema (for OpenAI
     * compatibility).
     * - Force `type: "object"` if missing but properties/required present.
     * - Clean for Gemini compatibility.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> normalizeToolParameters(Map<String, Object> schema) {
        if (schema == null)
            return null;

        // Already has type + properties and no top-level anyOf → just clean
        if (schema.containsKey("type") && schema.containsKey("properties")
                && !(schema.get("anyOf") instanceof List)) {
            return cleanSchemaForGemini(schema);
        }

        // Missing type, but has object-ish fields → force type:object
        if (!schema.containsKey("type")
                && (schema.get("properties") instanceof Map || schema.get("required") instanceof List)
                && !(schema.get("anyOf") instanceof List)
                && !(schema.get("oneOf") instanceof List)) {
            Map<String, Object> copy = new LinkedHashMap<>(schema);
            copy.put("type", "object");
            return cleanSchemaForGemini(copy);
        }

        // Find anyOf or oneOf
        String variantKey = schema.get("anyOf") instanceof List ? "anyOf"
                : schema.get("oneOf") instanceof List ? "oneOf" : null;
        if (variantKey == null)
            return schema;

        List<?> variants = (List<?>) schema.get(variantKey);
        Map<String, Object> mergedProperties = new LinkedHashMap<>();
        Map<String, Integer> requiredCounts = new LinkedHashMap<>();
        int objectVariants = 0;

        for (Object entry : variants) {
            if (!(entry instanceof Map<?, ?> entryMap))
                continue;
            Object props = entryMap.get("properties");
            if (!(props instanceof Map<?, ?> propsMap))
                continue;
            objectVariants++;
            for (Map.Entry<?, ?> propEntry : propsMap.entrySet()) {
                String key = String.valueOf(propEntry.getKey());
                Object value = propEntry.getValue();
                mergedProperties.merge(key, value, (existing, incoming) -> mergePropertySchemas(existing, incoming));
            }
            Object required = entryMap.get("required");
            if (required instanceof List<?> reqList) {
                for (Object req : reqList) {
                    if (req instanceof String s) {
                        requiredCounts.merge(s, 1, Integer::sum);
                    }
                }
            }
        }

        List<String> baseRequired = schema.get("required") instanceof List<?> l
                ? l.stream().filter(k -> k instanceof String).map(k -> (String) k).toList()
                : null;

        List<String> mergedRequired;
        if (baseRequired != null && !baseRequired.isEmpty()) {
            mergedRequired = baseRequired;
        } else if (objectVariants > 0) {
            int ov = objectVariants;
            mergedRequired = requiredCounts.entrySet().stream()
                    .filter(e -> e.getValue() == ov)
                    .map(Map.Entry::getKey)
                    .toList();
        } else {
            mergedRequired = null;
        }

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("type", "object");
        if (schema.get("title") instanceof String title)
            result.put("title", title);
        if (schema.get("description") instanceof String desc)
            result.put("description", desc);
        result.put("properties", mergedProperties.isEmpty()
                ? (schema.getOrDefault("properties", Map.of()))
                : mergedProperties);
        if (mergedRequired != null && !mergedRequired.isEmpty())
            result.put("required", mergedRequired);
        result.put("additionalProperties",
                schema.containsKey("additionalProperties") ? schema.get("additionalProperties") : true);

        return cleanSchemaForGemini(result);
    }

    /**
     * Clean a JSON Schema for Gemini compatibility — remove unsupported keywords.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> cleanSchemaForGemini(Map<String, Object> schema) {
        if (schema == null)
            return null;
        Map<String, Object> cleaned = new LinkedHashMap<>();
        Set<String> geminiUnsupported = Set.of(
                "$schema", "$id", "$ref", "$defs", "definitions",
                "examples", "default", "const", "if", "then", "else",
                "not", "additionalItems", "contains", "patternProperties",
                "dependencies", "propertyNames", "contentMediaType", "contentEncoding");
        for (Map.Entry<String, Object> entry : schema.entrySet()) {
            String key = entry.getKey();
            if (geminiUnsupported.contains(key))
                continue;
            Object value = entry.getValue();
            if (value instanceof Map<?, ?> nested) {
                cleaned.put(key, cleanSchemaForGemini((Map<String, Object>) nested));
            } else if (value instanceof List<?> list) {
                List<Object> cleanedList = new ArrayList<>();
                for (Object item : list) {
                    if (item instanceof Map<?, ?> nestedItem) {
                        cleanedList.add(cleanSchemaForGemini((Map<String, Object>) nestedItem));
                    } else {
                        cleanedList.add(item);
                    }
                }
                cleaned.put(key, cleanedList);
            } else {
                cleaned.put(key, value);
            }
        }
        return cleaned;
    }

    // ── Internal helpers ────────────────────────────────────────────

    @SuppressWarnings("unchecked")
    private static Object mergePropertySchemas(Object existing, Object incoming) {
        if (existing == null)
            return incoming;
        if (incoming == null)
            return existing;

        List<?> existingEnum = extractEnumValues(existing);
        List<?> incomingEnum = extractEnumValues(incoming);
        if (existingEnum != null || incomingEnum != null) {
            Set<Object> values = new LinkedHashSet<>();
            if (existingEnum != null)
                values.addAll(existingEnum);
            if (incomingEnum != null)
                values.addAll(incomingEnum);
            Map<String, Object> merged = new LinkedHashMap<>();
            for (Object source : List.of(existing, incoming)) {
                if (!(source instanceof Map<?, ?> srcMap))
                    continue;
                for (String meta : List.of("title", "description", "default")) {
                    if (!merged.containsKey(meta) && srcMap.containsKey(meta)) {
                        merged.put(meta, srcMap.get(meta));
                    }
                }
            }
            Set<String> types = new HashSet<>();
            for (Object v : values)
                types.add(v == null ? "null" : v.getClass().getSimpleName().toLowerCase());
            if (types.size() == 1) {
                String javaType = types.iterator().next();
                String jsonType = switch (javaType) {
                    case "string" -> "string";
                    case "integer", "long" -> "integer";
                    case "double", "float" -> "number";
                    case "boolean" -> "boolean";
                    default -> "string";
                };
                merged.put("type", jsonType);
            }
            merged.put("enum", new ArrayList<>(values));
            return merged;
        }
        return existing;
    }

    @SuppressWarnings("unchecked")
    private static List<?> extractEnumValues(Object schema) {
        if (!(schema instanceof Map<?, ?> map))
            return null;
        if (map.get("enum") instanceof List<?> e)
            return e;
        if (map.containsKey("const"))
            return List.of(map.get("const"));
        List<?> variants = map.get("anyOf") instanceof List<?> l ? l
                : (map.get("oneOf") instanceof List<?> l2 ? l2 : null);
        if (variants != null) {
            List<Object> values = new ArrayList<>();
            for (Object v : variants) {
                List<?> extracted = extractEnumValues(v);
                if (extracted != null)
                    values.addAll(extracted);
            }
            return values.isEmpty() ? null : values;
        }
        return null;
    }
}
