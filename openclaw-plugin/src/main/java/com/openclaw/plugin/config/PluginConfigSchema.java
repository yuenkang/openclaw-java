package com.openclaw.plugin.config;

import java.util.Map;

/**
 * Plugin config schema factory — provides empty and basic schema validators.
 * Corresponds to TypeScript's plugins/config-schema.ts.
 */
public final class PluginConfigSchema {

    private PluginConfigSchema() {
    }

    public sealed interface SafeParseResult {
        record Success(Object data) implements SafeParseResult {
        }

        record Failure(String message) implements SafeParseResult {
        }
    }

    @FunctionalInterface
    public interface ConfigSchema {
        SafeParseResult safeParse(Object value);
    }

    /**
     * Returns a schema that accepts only empty config objects.
     */
    public static ConfigSchema emptyPluginConfigSchema() {
        return value -> {
            if (value == null) {
                return new SafeParseResult.Success(null);
            }
            if (!(value instanceof Map<?, ?> map)) {
                return new SafeParseResult.Failure("expected config object");
            }
            if (!map.isEmpty()) {
                return new SafeParseResult.Failure("config must be empty");
            }
            return new SafeParseResult.Success(value);
        };
    }

    /**
     * Returns a schema that accepts any Map as config.
     */
    public static ConfigSchema anyObjectConfigSchema() {
        return value -> {
            if (value == null) {
                return new SafeParseResult.Success(null);
            }
            if (!(value instanceof Map)) {
                return new SafeParseResult.Failure("expected config object");
            }
            return new SafeParseResult.Success(value);
        };
    }
}
