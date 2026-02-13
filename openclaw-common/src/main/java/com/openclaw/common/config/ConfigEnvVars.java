package com.openclaw.common.config;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Collect environment variables declared in config.
 * Corresponds to TypeScript's env-vars.ts.
 */
public final class ConfigEnvVars {

    private ConfigEnvVars() {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Collect environment variables from the config's env section.
     *
     * @param cfg the OpenClaw configuration (may be null)
     * @return a map of env var name -> value
     */
    public static Map<String, String> collectConfigEnvVars(OpenClawConfig cfg) {
        if (cfg == null) {
            return Map.of();
        }
        var envConfig = cfg.getEnv();
        if (envConfig == null) {
            return Map.of();
        }

        Map<String, String> entries = new LinkedHashMap<>();

        // Collect from env.vars
        var vars = envConfig.getVars();
        if (vars != null) {
            for (var entry : vars.entrySet()) {
                if (entry.getValue() != null && !entry.getValue().isEmpty()) {
                    entries.put(entry.getKey(), entry.getValue());
                }
            }
        }

        return entries;
    }
}
