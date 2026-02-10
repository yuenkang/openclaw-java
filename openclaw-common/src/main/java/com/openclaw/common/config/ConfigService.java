package com.openclaw.common.config;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Loads and caches OpenClaw configuration.
 * Corresponds to TypeScript's io.ts createConfigIO / loadConfig.
 */
@Slf4j
public class ConfigService {

    private static final Duration DEFAULT_CACHE_TTL = Duration.ofMillis(200);
    private static final Pattern ENV_VAR_PATTERN = Pattern.compile("\\$\\{([^}:]+)(?::-(.*?))?}");

    private final ObjectMapper objectMapper;
    private final Cache<String, OpenClawConfig> cache;
    private final Path configPath;

    public ConfigService(Path configPath) {
        this(configPath, DEFAULT_CACHE_TTL);
    }

    public ConfigService(Path configPath, Duration cacheTtl) {
        this.configPath = configPath;
        this.objectMapper = new ObjectMapper()
                .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        this.cache = Caffeine.newBuilder()
                .expireAfterWrite(cacheTtl)
                .maximumSize(1)
                .build();
    }

    /**
     * Load config with caching.
     */
    public OpenClawConfig loadConfig() {
        return cache.get(configPath.toString(), key -> doLoadConfig());
    }

    /**
     * Force reload config, bypassing cache.
     */
    public OpenClawConfig reloadConfig() {
        cache.invalidateAll();
        return loadConfig();
    }

    private OpenClawConfig doLoadConfig() {
        try {
            if (!Files.exists(configPath)) {
                log.warn("Config file not found: {}, using defaults", configPath);
                return applyDefaults(new OpenClawConfig());
            }

            String raw = Files.readString(configPath);

            // Environment variable substitution
            raw = substituteEnvVars(raw);

            // Parse JSON
            OpenClawConfig config = objectMapper.readValue(raw, OpenClawConfig.class);

            // Apply defaults
            config = applyDefaults(config);

            log.info("Config loaded from: {}", configPath);
            return config;

        } catch (IOException e) {
            log.error("Failed to load config from: {}", configPath, e);
            return applyDefaults(new OpenClawConfig());
        }
    }

    /**
     * Substitute ${VAR} and ${VAR:-default} patterns.
     * Corresponds to TypeScript's env-substitution.ts.
     */
    String substituteEnvVars(String raw) {
        Map<String, String> env = System.getenv();
        Matcher matcher = ENV_VAR_PATTERN.matcher(raw);
        StringBuilder result = new StringBuilder();

        while (matcher.find()) {
            String varName = matcher.group(1);
            String defaultValue = matcher.group(2);
            String value = env.getOrDefault(varName,
                    defaultValue != null ? defaultValue : "");
            matcher.appendReplacement(result, Matcher.quoteReplacement(value));
        }
        matcher.appendTail(result);
        return result.toString();
    }

    /**
     * Apply default values to missing config fields.
     * Corresponds to TypeScript's defaults.ts.
     */
    OpenClawConfig applyDefaults(OpenClawConfig config) {
        if (config.getGateway() == null) {
            config.setGateway(new OpenClawConfig.GatewayConfig());
        }
        if (config.getAuth() == null) {
            config.setAuth(new OpenClawConfig.AuthConfig());
        }
        if (config.getCron() == null) {
            config.setCron(new OpenClawConfig.CronConfig());
        }
        if (config.getLogging() == null) {
            config.setLogging(new OpenClawConfig.LoggingConfig());
        }
        if (config.getAgents() == null) {
            config.setAgents(new OpenClawConfig.AgentsConfig());
        }
        if (config.getAgents().getDefaults() == null) {
            config.getAgents().setDefaults(new OpenClawConfig.AgentDefaults());
        }
        return config;
    }
}
