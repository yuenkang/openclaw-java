package com.openclaw.agent.schema;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Scrubs JSON-Schema objects to comply with Gemini's restricted subset.
 * Removes unsupported keywords, resolves $ref, flattens anyOf/oneOf literal
 * unions to enum,
 * strips null variants, and converts const → enum.
 * Mirrors agents/schema/clean-for-gemini.ts.
 */
public final class GeminiSchemaCleanser {

    private GeminiSchemaCleanser() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper();

    public static final Set<String> UNSUPPORTED_KEYWORDS = Set.of(
            "patternProperties", "additionalProperties",
            "$schema", "$id", "$ref", "$defs", "definitions", "examples",
            "minLength", "maxLength", "minimum", "maximum", "multipleOf",
            "pattern", "format", "minItems", "maxItems", "uniqueItems",
            "minProperties", "maxProperties");

    private static final Set<String> PRESERVE_KEYS = Set.of("description", "title", "default");
    private static final Pattern REF_PATTERN = Pattern.compile("^#/(?:\\$defs|definitions)/(.+)$");

    /**
     * Clean a JSON-Schema node for Gemini compatibility.
     */
    public static JsonNode clean(JsonNode schema) {
        if (schema == null || schema.isNull() || schema.isMissingNode())
            return schema;
        if (schema.isArray()) {
            ArrayNode arr = JsonNodeFactory.instance.arrayNode();
            for (JsonNode item : schema)
                arr.add(clean(item));
            return arr;
        }
        if (!schema.isObject())
            return schema;

        Map<String, JsonNode> defs = extractDefs(schema);
        return cleanWithDefs(schema, defs, new HashSet<>());
    }

    // --- Internal ---

    private static Map<String, JsonNode> extractDefs(JsonNode schema) {
        Map<String, JsonNode> defs = new HashMap<>();
        addDefsFrom(defs, schema.get("$defs"));
        addDefsFrom(defs, schema.get("definitions"));
        return defs;
    }

    private static void addDefsFrom(Map<String, JsonNode> defs, JsonNode node) {
        if (node != null && node.isObject()) {
            node.fields().forEachRemaining(e -> defs.put(e.getKey(), e.getValue()));
        }
    }

    private static Map<String, JsonNode> mergeDefs(Map<String, JsonNode> parent, JsonNode schema) {
        Map<String, JsonNode> next = new HashMap<>(parent);
        addDefsFrom(next, schema.get("$defs"));
        addDefsFrom(next, schema.get("definitions"));
        return next;
    }

    private static String decodeJsonPointerSegment(String s) {
        return s.replace("~1", "/").replace("~0", "~");
    }

    private static JsonNode tryResolveRef(String ref, Map<String, JsonNode> defs) {
        Matcher m = REF_PATTERN.matcher(ref);
        if (!m.matches())
            return null;
        String name = decodeJsonPointerSegment(m.group(1));
        return name.isEmpty() ? null : defs.get(name);
    }

    private static boolean isNullSchema(JsonNode variant) {
        if (variant == null || !variant.isObject())
            return false;
        // { const: null }
        if (variant.has("const") && variant.get("const").isNull())
            return true;
        // { enum: [null] }
        JsonNode en = variant.get("enum");
        if (en != null && en.isArray() && en.size() == 1 && en.get(0).isNull())
            return true;
        // { type: "null" }
        JsonNode typeNode = variant.get("type");
        if (typeNode != null) {
            if (typeNode.isTextual() && "null".equals(typeNode.textValue()))
                return true;
            if (typeNode.isArray() && typeNode.size() == 1
                    && typeNode.get(0).isTextual() && "null".equals(typeNode.get(0).textValue()))
                return true;
        }
        return false;
    }

    private static JsonNode tryFlattenLiteralAnyOf(List<JsonNode> variants) {
        if (variants.isEmpty())
            return null;
        String commonType = null;
        ArrayNode values = JsonNodeFactory.instance.arrayNode();

        for (JsonNode v : variants) {
            if (!v.isObject())
                return null;
            JsonNode literalValue = null;
            if (v.has("const")) {
                literalValue = v.get("const");
            } else if (v.has("enum") && v.get("enum").isArray() && v.get("enum").size() == 1) {
                literalValue = v.get("enum").get(0);
            } else {
                return null;
            }
            JsonNode typeNode = v.get("type");
            if (typeNode == null || !typeNode.isTextual())
                return null;
            String variantType = typeNode.textValue();
            if (commonType == null)
                commonType = variantType;
            else if (!commonType.equals(variantType))
                return null;
            values.add(literalValue);
        }

        if (commonType != null && values.size() > 0) {
            ObjectNode result = JsonNodeFactory.instance.objectNode();
            result.put("type", commonType);
            result.set("enum", values);
            return result;
        }
        return null;
    }

    private static JsonNode cleanWithDefs(JsonNode schema, Map<String, JsonNode> defs, Set<String> refStack) {
        if (schema == null || !schema.isObject())
            return schema;
        if (schema.isArray()) {
            ArrayNode arr = JsonNodeFactory.instance.arrayNode();
            for (JsonNode item : schema)
                arr.add(cleanWithDefs(item, defs, refStack));
            return arr;
        }

        Map<String, JsonNode> nextDefs = mergeDefs(defs, schema);

        // Handle $ref
        JsonNode refNode = schema.get("$ref");
        if (refNode != null && refNode.isTextual()) {
            String refValue = refNode.textValue();
            if (refStack.contains(refValue))
                return JsonNodeFactory.instance.objectNode();

            JsonNode resolved = tryResolveRef(refValue, nextDefs);
            if (resolved != null) {
                Set<String> nextStack = new HashSet<>(refStack);
                nextStack.add(refValue);
                JsonNode cleaned = cleanWithDefs(resolved, nextDefs, nextStack);
                if (cleaned != null && cleaned.isObject()) {
                    ObjectNode result = ((ObjectNode) cleaned).deepCopy();
                    for (String key : PRESERVE_KEYS) {
                        if (schema.has(key))
                            result.set(key, schema.get(key));
                    }
                    return result;
                }
                return cleaned;
            }

            ObjectNode result = JsonNodeFactory.instance.objectNode();
            for (String key : PRESERVE_KEYS) {
                if (schema.has(key))
                    result.set(key, schema.get(key));
            }
            return result;
        }

        boolean hasAnyOf = schema.has("anyOf") && schema.get("anyOf").isArray();
        boolean hasOneOf = schema.has("oneOf") && schema.get("oneOf").isArray();

        // Process anyOf/oneOf variants
        List<JsonNode> cleanedAnyOf = hasAnyOf ? cleanVariants(schema.get("anyOf"), nextDefs, refStack) : null;
        List<JsonNode> cleanedOneOf = hasOneOf ? cleanVariants(schema.get("oneOf"), nextDefs, refStack) : null;

        // Try flattening anyOf
        if (hasAnyOf && cleanedAnyOf != null) {
            JsonNode result = trySimplify(schema, cleanedAnyOf);
            if (result != null)
                return result;
        }

        // Try flattening oneOf
        if (hasOneOf && cleanedOneOf != null) {
            JsonNode result = trySimplify(schema, cleanedOneOf);
            if (result != null)
                return result;
        }

        // Generic clean pass
        ObjectNode cleaned = JsonNodeFactory.instance.objectNode();
        Iterator<Map.Entry<String, JsonNode>> fields = schema.fields();
        while (fields.hasNext()) {
            Map.Entry<String, JsonNode> entry = fields.next();
            String key = entry.getKey();
            JsonNode value = entry.getValue();

            if (UNSUPPORTED_KEYWORDS.contains(key))
                continue;

            if ("const".equals(key)) {
                ArrayNode arr = JsonNodeFactory.instance.arrayNode();
                arr.add(value);
                cleaned.set("enum", arr);
                continue;
            }

            if ("type".equals(key) && (hasAnyOf || hasOneOf))
                continue;
            if ("type".equals(key) && value.isArray()) {
                ArrayNode types = JsonNodeFactory.instance.arrayNode();
                for (JsonNode t : value) {
                    if (t.isTextual() && !"null".equals(t.textValue()))
                        types.add(t);
                }
                cleaned.set("type", types.size() == 1 ? types.get(0) : types);
                continue;
            }

            if ("properties".equals(key) && value.isObject()) {
                ObjectNode props = JsonNodeFactory.instance.objectNode();
                value.fields()
                        .forEachRemaining(e -> props.set(e.getKey(), cleanWithDefs(e.getValue(), nextDefs, refStack)));
                cleaned.set(key, props);
            } else if ("items".equals(key)) {
                if (value.isArray()) {
                    ArrayNode arr = JsonNodeFactory.instance.arrayNode();
                    for (JsonNode item : value)
                        arr.add(cleanWithDefs(item, nextDefs, refStack));
                    cleaned.set(key, arr);
                } else if (value.isObject()) {
                    cleaned.set(key, cleanWithDefs(value, nextDefs, refStack));
                } else {
                    cleaned.set(key, value);
                }
            } else if (("anyOf".equals(key) || "oneOf".equals(key) || "allOf".equals(key)) && value.isArray()) {
                List<JsonNode> source = "anyOf".equals(key) && cleanedAnyOf != null ? cleanedAnyOf
                        : "oneOf".equals(key) && cleanedOneOf != null ? cleanedOneOf
                                : null;
                if (source != null) {
                    ArrayNode arr = JsonNodeFactory.instance.arrayNode();
                    source.forEach(arr::add);
                    cleaned.set(key, arr);
                } else {
                    ArrayNode arr = JsonNodeFactory.instance.arrayNode();
                    for (JsonNode v : value)
                        arr.add(cleanWithDefs(v, nextDefs, refStack));
                    cleaned.set(key, arr);
                }
            } else {
                cleaned.set(key, value);
            }
        }

        return cleaned;
    }

    private static List<JsonNode> cleanVariants(JsonNode variants, Map<String, JsonNode> defs, Set<String> refStack) {
        List<JsonNode> result = new ArrayList<>();
        for (JsonNode v : variants)
            result.add(cleanWithDefs(v, defs, refStack));
        return result;
    }

    private static JsonNode trySimplify(JsonNode original, List<JsonNode> cleaned) {
        // Strip null variants
        List<JsonNode> nonNull = cleaned.stream().filter(v -> !isNullSchema(v)).toList();
        boolean stripped = nonNull.size() != cleaned.size();

        // Try flatten to enum
        JsonNode flattened = tryFlattenLiteralAnyOf(nonNull);
        if (flattened != null && flattened.isObject()) {
            ObjectNode result = ((ObjectNode) flattened).deepCopy();
            for (String key : PRESERVE_KEYS) {
                if (original.has(key))
                    result.set(key, original.get(key));
            }
            return result;
        }

        // Unwrap single remaining variant
        if (stripped && nonNull.size() == 1) {
            JsonNode lone = nonNull.get(0);
            if (lone.isObject()) {
                ObjectNode result = ((ObjectNode) lone).deepCopy();
                for (String key : PRESERVE_KEYS) {
                    if (original.has(key))
                        result.set(key, original.get(key));
                }
                return result;
            }
            return lone;
        }

        return null;
    }
}
