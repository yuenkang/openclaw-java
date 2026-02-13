package com.openclaw.common.config;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

/**
 * Config include processor — resolves $include directives in config objects.
 * Supports single file, array of files, circular detection, and depth limits.
 * Corresponds to TypeScript's includes.ts.
 */
public final class ConfigIncludes {

    private ConfigIncludes() {
    }

    public static final String INCLUDE_KEY = "$include";
    public static final int MAX_INCLUDE_DEPTH = 10;

    // =========================================================================
    // Types
    // =========================================================================

    /**
     * Pluggable file reader and JSON parser for include resolution.
     */
    public interface IncludeResolver {
        String readFile(String path);

        Object parseJson(String raw);
    }

    // =========================================================================
    // Errors
    // =========================================================================

    public static class ConfigIncludeError extends RuntimeException {
        private final String includePath;

        public ConfigIncludeError(String message, String includePath) {
            super(message);
            this.includePath = includePath;
        }

        public ConfigIncludeError(String message, String includePath, Throwable cause) {
            super(message, cause);
            this.includePath = includePath;
        }

        public String getIncludePath() {
            return includePath;
        }
    }

    public static class CircularIncludeError extends ConfigIncludeError {
        private final List<String> chain;

        public CircularIncludeError(List<String> chain) {
            super("Circular include detected: " + String.join(" -> ", chain),
                    chain.get(chain.size() - 1));
            this.chain = chain;
        }

        public List<String> getChain() {
            return chain;
        }
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Resolves all $include directives in a parsed config object.
     */
    public static Object resolveConfigIncludes(Object obj, String configPath, IncludeResolver resolver) {
        return new Processor(configPath, resolver).process(obj);
    }

    // =========================================================================
    // Deep merge (arrays concatenate, objects merge recursively)
    // =========================================================================

    @SuppressWarnings("unchecked")
    public static Object deepMerge(Object target, Object source) {
        if (target instanceof List && source instanceof List) {
            List<Object> merged = new ArrayList<>((List<Object>) target);
            merged.addAll((List<Object>) source);
            return merged;
        }
        if (isPlainObject(target) && isPlainObject(source)) {
            Map<String, Object> result = new LinkedHashMap<>((Map<String, Object>) target);
            Map<String, Object> srcMap = (Map<String, Object>) source;
            for (var entry : srcMap.entrySet()) {
                result.merge(entry.getKey(), entry.getValue(),
                        (old, nw) -> deepMerge(old, nw));
            }
            return result;
        }
        return source;
    }

    // =========================================================================
    // Include Processor
    // =========================================================================

    private static class Processor {
        private Set<String> visited;
        private int depth;
        private final String basePath;
        private final IncludeResolver resolver;

        Processor(String basePath, IncludeResolver resolver) {
            this.basePath = basePath;
            this.resolver = resolver;
            this.visited = new HashSet<>();
            this.visited.add(Path.of(basePath).normalize().toString());
            this.depth = 0;
        }

        @SuppressWarnings("unchecked")
        Object process(Object obj) {
            if (obj instanceof List) {
                return ((List<Object>) obj).stream()
                        .map(this::process)
                        .toList();
            }
            if (!isPlainObject(obj))
                return obj;

            Map<String, Object> map = (Map<String, Object>) obj;
            if (!map.containsKey(INCLUDE_KEY)) {
                return processObject(map);
            }
            return processInclude(map);
        }

        @SuppressWarnings("unchecked")
        private Map<String, Object> processObject(Map<String, Object> obj) {
            Map<String, Object> result = new LinkedHashMap<>();
            for (var entry : obj.entrySet()) {
                result.put(entry.getKey(), process(entry.getValue()));
            }
            return result;
        }

        @SuppressWarnings("unchecked")
        private Object processInclude(Map<String, Object> obj) {
            Object includeValue = obj.get(INCLUDE_KEY);
            List<String> otherKeys = obj.keySet().stream()
                    .filter(k -> !INCLUDE_KEY.equals(k)).toList();
            Object included = resolveInclude(includeValue);

            if (otherKeys.isEmpty())
                return included;

            if (!isPlainObject(included)) {
                throw new ConfigIncludeError(
                        "Sibling keys require included content to be an object",
                        includeValue instanceof String s ? s : INCLUDE_KEY);
            }

            Map<String, Object> rest = new LinkedHashMap<>();
            for (String key : otherKeys) {
                rest.put(key, process(obj.get(key)));
            }
            return deepMerge(included, rest);
        }

        @SuppressWarnings("unchecked")
        private Object resolveInclude(Object value) {
            if (value instanceof String s) {
                return loadFile(s);
            }
            if (value instanceof List<?> list) {
                Object merged = new LinkedHashMap<String, Object>();
                for (Object item : list) {
                    if (!(item instanceof String s)) {
                        throw new ConfigIncludeError(
                                "Invalid $include array item: expected string, got "
                                        + (item == null ? "null" : item.getClass().getSimpleName()),
                                String.valueOf(item));
                    }
                    merged = deepMerge(merged, loadFile(s));
                }
                return merged;
            }
            throw new ConfigIncludeError(
                    "Invalid $include value: expected string or array of strings, got "
                            + (value == null ? "null" : value.getClass().getSimpleName()),
                    String.valueOf(value));
        }

        private Object loadFile(String includePath) {
            String resolvedPath = resolvePath(includePath);
            checkCircular(resolvedPath);
            checkDepth(includePath);

            String raw;
            try {
                raw = resolver.readFile(resolvedPath);
            } catch (Exception e) {
                throw new ConfigIncludeError(
                        "Failed to read include file: " + includePath + " (resolved: " + resolvedPath + ")",
                        includePath, e);
            }

            Object parsed;
            try {
                parsed = resolver.parseJson(raw);
            } catch (Exception e) {
                throw new ConfigIncludeError(
                        "Failed to parse include file: " + includePath + " (resolved: " + resolvedPath + ")",
                        includePath, e);
            }

            return processNested(resolvedPath, parsed);
        }

        private String resolvePath(String includePath) {
            Path p = Path.of(includePath);
            if (p.isAbsolute())
                return p.normalize().toString();
            return Path.of(basePath).getParent().resolve(includePath).normalize().toString();
        }

        private void checkCircular(String resolvedPath) {
            if (visited.contains(resolvedPath)) {
                List<String> chain = new ArrayList<>(visited);
                chain.add(resolvedPath);
                throw new CircularIncludeError(chain);
            }
        }

        private void checkDepth(String includePath) {
            if (depth >= MAX_INCLUDE_DEPTH) {
                throw new ConfigIncludeError(
                        "Maximum include depth (" + MAX_INCLUDE_DEPTH + ") exceeded at: " + includePath,
                        includePath);
            }
        }

        private Object processNested(String resolvedPath, Object parsed) {
            Processor nested = new Processor(resolvedPath, resolver);
            nested.visited = new HashSet<>(visited);
            nested.visited.add(resolvedPath);
            nested.depth = depth + 1;
            return nested.process(parsed);
        }
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private static boolean isPlainObject(Object value) {
        return value instanceof Map;
    }
}
