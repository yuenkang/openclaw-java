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
import java.util.LinkedHashMap;
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
        // Expand ~ to user home directory
        String pathStr = configPath.toString();
        if (pathStr.startsWith("~")) {
            pathStr = System.getProperty("user.home") + pathStr.substring(1);
            configPath = Path.of(pathStr);
        }
        this.configPath = configPath;
        this.objectMapper = new ObjectMapper()
                .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        this.cache = Caffeine.newBuilder()
                .expireAfterWrite(cacheTtl)
                .maximumSize(1)
                .build();

        // Invalidate cache when runtime overrides change so next loadConfig() picks up
        // new values
        ConfigRuntimeOverrides.setOnOverrideChanged(() -> cache.invalidateAll());
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

    /**
     * Save config changes to disk, preserving original file structure.
     * Only applies runtime overrides on top of the original raw JSON;
     * all other fields (including nulls and empty objects) remain untouched.
     */
    @SuppressWarnings("unchecked")
    public void saveConfig(OpenClawConfig config) throws IOException {
        Files.createDirectories(configPath.getParent());

        // Read the original file as a raw LinkedHashMap to preserve key order and all
        // values
        Map<String, Object> original = new LinkedHashMap<>();
        if (Files.exists(configPath)) {
            try {
                String raw = Files.readString(configPath);
                original = objectMapper.readValue(raw, LinkedHashMap.class);
            } catch (Exception e) {
                log.warn("Could not read original config: {}", e.getMessage());
                // Fallback: serialize the full config object
                original = objectMapper.convertValue(config, LinkedHashMap.class);
            }
        }

        // Apply only runtime overrides on top of the original
        Map<String, Object> overrides = ConfigRuntimeOverrides.getConfigOverrides();
        Map<String, Object> merged = overrides.isEmpty()
                ? original
                : deepMerge(original, overrides);

        String json = objectMapper.writerWithDefaultPrettyPrinter()
                .writeValueAsString(merged);
        Files.writeString(configPath, json);
        cache.invalidateAll();
        log.info("Config saved to: {}", configPath);
    }

    /**
     * Deep-merge override values into the base map.
     * Base map key order is preserved; new keys from overrides are appended.
     */
    @SuppressWarnings("unchecked")
    private Map<String, Object> deepMerge(
            Map<String, Object> base, Map<String, Object> overrides) {
        Map<String, Object> result = new LinkedHashMap<>(base);

        for (var entry : overrides.entrySet()) {
            String key = entry.getKey();
            Object overrideVal = entry.getValue();
            Object baseVal = result.get(key);

            if (baseVal instanceof Map && overrideVal instanceof Map) {
                result.put(key, deepMerge(
                        (Map<String, Object>) baseVal, (Map<String, Object>) overrideVal));
            } else {
                result.put(key, overrideVal);
            }
        }

        return result;
    }

    /**
     * Get the config file path.
     */
    public Path getConfigPath() {
        return configPath;
    }

    @SuppressWarnings("unchecked")
    private OpenClawConfig doLoadConfig() {
        try {
            OpenClawConfig config;
            if (!Files.exists(configPath)) {
                log.warn("Config file not found: {}, using defaults", configPath);
                config = applyDefaults(new OpenClawConfig());
            } else {
                String raw = Files.readString(configPath);

                // Environment variable substitution
                raw = substituteEnvVars(raw);

                // Parse JSON
                config = objectMapper.readValue(raw, OpenClawConfig.class);

                // Apply defaults
                config = applyDefaults(config);

                log.info("Config loaded from: {}", configPath);
            }

            // Apply runtime overrides (mirrors TypeScript's applyConfigOverrides at end of
            // loadConfig)
            Map<String, Object> overrides = ConfigRuntimeOverrides.getConfigOverrides();
            if (!overrides.isEmpty()) {
                Map<String, Object> configMap = objectMapper.convertValue(config, Map.class);
                Map<String, Object> merged = ConfigRuntimeOverrides.applyConfigOverrides(configMap);
                config = objectMapper.convertValue(merged, OpenClawConfig.class);
                log.debug("Applied {} runtime override(s)", overrides.size());
            }

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
            config.getAgents().setDefaults(new OpenClawConfig.AgentDefaultsConfig());
        }
        return config;
    }
}
