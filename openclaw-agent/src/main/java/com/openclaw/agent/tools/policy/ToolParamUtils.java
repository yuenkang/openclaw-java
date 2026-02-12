package com.openclaw.agent.tools.policy;

import java.util.*;

/**
 * Tool parameter normalization — Claude Code param aliasing and required-param
 * validation.
 * Corresponds to TypeScript pi-tools.read.ts (param normalization parts).
 */
public final class ToolParamUtils {

    private ToolParamUtils() {
    }

    /** A required-parameter group: at least one key must be present. */
    public record RequiredParamGroup(List<String> keys, boolean allowEmpty, String label) {
        public RequiredParamGroup(List<String> keys, String label) {
            this(keys, false, label);
        }
    }

    /** Claude Code → pi-coding-agent param alias groups. */
    public static final Map<String, List<RequiredParamGroup>> CLAUDE_PARAM_GROUPS;
    static {
        Map<String, List<RequiredParamGroup>> m = new LinkedHashMap<>();
        m.put("read", List.of(
                new RequiredParamGroup(List.of("path", "file_path"), "path (path or file_path)")));
        m.put("write", List.of(
                new RequiredParamGroup(List.of("path", "file_path"), "path (path or file_path)")));
        m.put("edit", List.of(
                new RequiredParamGroup(List.of("path", "file_path"), "path (path or file_path)"),
                new RequiredParamGroup(List.of("oldText", "old_string"), "oldText (oldText or old_string)"),
                new RequiredParamGroup(List.of("newText", "new_string"), "newText (newText or new_string)")));
        CLAUDE_PARAM_GROUPS = Collections.unmodifiableMap(m);
    }

    /**
     * Normalize Claude Code params to pi-coding-agent conventions:
     * file_path → path, old_string → oldText, new_string → newText.
     */
    public static Map<String, Object> normalizeToolParams(Map<String, Object> params) {
        if (params == null)
            return null;
        Map<String, Object> normalized = new LinkedHashMap<>(params);

        if (normalized.containsKey("file_path") && !normalized.containsKey("path")) {
            normalized.put("path", normalized.remove("file_path"));
        }
        if (normalized.containsKey("old_string") && !normalized.containsKey("oldText")) {
            normalized.put("oldText", normalized.remove("old_string"));
        }
        if (normalized.containsKey("new_string") && !normalized.containsKey("newText")) {
            normalized.put("newText", normalized.remove("new_string"));
        }
        return normalized;
    }

    /**
     * Assert that required parameter groups are satisfied.
     * 
     * @throws IllegalArgumentException if a required group is missing
     */
    public static void assertRequiredParams(
            Map<String, Object> params,
            List<RequiredParamGroup> groups,
            String toolName) {
        if (params == null) {
            throw new IllegalArgumentException("Missing parameters for " + toolName);
        }
        for (RequiredParamGroup group : groups) {
            boolean satisfied = group.keys().stream().anyMatch(key -> {
                Object value = params.get(key);
                if (!(value instanceof String s))
                    return false;
                return group.allowEmpty() || !s.trim().isEmpty();
            });
            if (!satisfied) {
                String label = group.label() != null ? group.label()
                        : String.join(" or ", group.keys());
                throw new IllegalArgumentException("Missing required parameter: " + label);
            }
        }
    }

    /**
     * Patch a tool schema to add Claude Code style aliases (file_path, old_string,
     * new_string).
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> patchSchemaForClaudeCompatibility(Map<String, Object> schema) {
        if (schema == null)
            return null;
        Object propsObj = schema.get("properties");
        if (!(propsObj instanceof Map<?, ?> propsMap))
            return schema;

        Map<String, Object> properties = new LinkedHashMap<>((Map<String, Object>) propsMap);
        List<String> required = schema.get("required") instanceof List<?> l
                ? new ArrayList<>(l.stream().filter(k -> k instanceof String).map(k -> (String) k).toList())
                : new ArrayList<>();
        boolean changed = false;

        record Alias(String original, String alias) {
        }
        for (Alias pair : List.of(
                new Alias("path", "file_path"),
                new Alias("oldText", "old_string"),
                new Alias("newText", "new_string"))) {
            if (!properties.containsKey(pair.original()))
                continue;
            if (!properties.containsKey(pair.alias())) {
                properties.put(pair.alias(), properties.get(pair.original()));
                changed = true;
            }
            int idx = required.indexOf(pair.original());
            if (idx >= 0) {
                required.remove(idx);
                changed = true;
            }
        }

        if (!changed)
            return schema;
        Map<String, Object> result = new LinkedHashMap<>(schema);
        result.put("properties", properties);
        result.put("required", required);
        return result;
    }
}
