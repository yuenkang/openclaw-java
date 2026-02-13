package com.openclaw.common.config;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Environment variable substitution for config values.
 * Supports ${VAR_NAME} syntax in string values.
 * Corresponds to TypeScript's env-substitution.ts + env-vars.ts.
 */
public final class EnvSubstitution {

    private EnvSubstitution() {
    }

    /** Pattern for valid uppercase env var names. */
    private static final Pattern ENV_VAR_NAME_PATTERN = Pattern.compile("^[A-Z_][A-Z0-9_]*$");

    // =========================================================================
    // MissingEnvVarError
    // =========================================================================

    /**
     * Thrown when a referenced environment variable is not set or is empty.
     */
    public static class MissingEnvVarException extends RuntimeException {
        private final String varName;
        private final String configPath;

        public MissingEnvVarException(String varName, String configPath) {
            super("Missing env var \"" + varName + "\" referenced at config path: " + configPath);
            this.varName = varName;
            this.configPath = configPath;
        }

        public String getVarName() {
            return varName;
        }

        public String getConfigPath() {
            return configPath;
        }
    }

    // =========================================================================
    // resolveConfigEnvVars — main entry point
    // =========================================================================

    /**
     * Resolve ${VAR_NAME} references in all string values throughout a config map.
     *
     * @param obj The parsed config as a nested Map (from JSON parse)
     * @param env Environment variables to use for substitution
     * @return The config with env vars substituted
     * @throws MissingEnvVarException if a referenced env var is not set or empty
     */
    public static Object resolveConfigEnvVars(Object obj, Map<String, String> env) {
        return substituteAny(obj, env, "");
    }

    /**
     * Resolve using System.getenv().
     */
    public static Object resolveConfigEnvVars(Object obj) {
        return resolveConfigEnvVars(obj, System.getenv());
    }

    // =========================================================================
    // collectConfigEnvVars — from env-vars.ts
    // =========================================================================

    /**
     * Collect env vars defined in config.env section.
     */
    public static Map<String, String> collectConfigEnvVars(OpenClawConfig cfg) {
        if (cfg == null || cfg.getEnv() == null) {
            return Map.of();
        }

        Map<String, String> entries = new LinkedHashMap<>();
        var envConfig = cfg.getEnv();

        // Explicit vars map
        if (envConfig.getVars() != null) {
            for (var entry : envConfig.getVars().entrySet()) {
                String value = entry.getValue();
                if (value != null && !value.isEmpty()) {
                    entries.put(entry.getKey(), value);
                }
            }
        }

        return entries;
    }

    // =========================================================================
    // Internal substitution logic
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static Object substituteAny(Object value, Map<String, String> env, String path) {
        if (value instanceof String s) {
            return substituteString(s, env, path);
        }

        if (value instanceof List<?> list) {
            List<Object> result = new ArrayList<>(list.size());
            for (int i = 0; i < list.size(); i++) {
                result.add(substituteAny(list.get(i), env, path + "[" + i + "]"));
            }
            return result;
        }

        if (value instanceof Map<?, ?> map) {
            Map<String, Object> result = new LinkedHashMap<>();
            for (var entry : ((Map<String, Object>) map).entrySet()) {
                String childPath = path.isEmpty() ? entry.getKey() : path + "." + entry.getKey();
                result.put(entry.getKey(), substituteAny(entry.getValue(), env, childPath));
            }
            return result;
        }

        // Primitives pass through
        return value;
    }

    private static String substituteString(String value, Map<String, String> env, String configPath) {
        if (!value.contains("$")) {
            return value;
        }

        StringBuilder sb = new StringBuilder(value.length());

        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
            if (ch != '$') {
                sb.append(ch);
                continue;
            }

            char next = (i + 1 < value.length()) ? value.charAt(i + 1) : 0;
            char afterNext = (i + 2 < value.length()) ? value.charAt(i + 2) : 0;

            // Escaped: $${VAR} -> ${VAR}
            if (next == '$' && afterNext == '{') {
                int end = value.indexOf('}', i + 3);
                if (end != -1) {
                    String name = value.substring(i + 3, end);
                    if (ENV_VAR_NAME_PATTERN.matcher(name).matches()) {
                        sb.append("${").append(name).append('}');
                        i = end;
                        continue;
                    }
                }
            }

            // Substitution: ${VAR} -> value
            if (next == '{') {
                int end = value.indexOf('}', i + 2);
                if (end != -1) {
                    String name = value.substring(i + 2, end);
                    if (ENV_VAR_NAME_PATTERN.matcher(name).matches()) {
                        String envValue = env.get(name);
                        if (envValue == null || envValue.isEmpty()) {
                            throw new MissingEnvVarException(name, configPath);
                        }
                        sb.append(envValue);
                        i = end;
                        continue;
                    }
                }
            }

            // Leave untouched
            sb.append(ch);
        }

        return sb.toString();
    }
}
