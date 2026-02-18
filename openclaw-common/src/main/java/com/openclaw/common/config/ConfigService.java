package com.openclaw.common.config;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.openclaw.common.infra.ShellEnv;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
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

    /**
     * Expected env keys to look for via login shell fallback (mirrors TS
     * SHELL_ENV_EXPECTED_KEYS).
     */
    private static final List<String> SHELL_ENV_EXPECTED_KEYS = List.of(
            "OPENAI_API_KEY", "ANTHROPIC_API_KEY", "ANTHROPIC_OAUTH_TOKEN",
            "GEMINI_API_KEY", "ZAI_API_KEY", "OPENROUTER_API_KEY",
            "AI_GATEWAY_API_KEY", "MINIMAX_API_KEY", "SYNTHETIC_API_KEY",
            "ELEVENLABS_API_KEY", "TELEGRAM_BOT_TOKEN", "DISCORD_BOT_TOKEN",
            "SLACK_BOT_TOKEN", "SLACK_APP_TOKEN",
            "OPENCLAW_GATEWAY_TOKEN", "OPENCLAW_GATEWAY_PASSWORD");

    @SuppressWarnings("unchecked")
    private OpenClawConfig doLoadConfig() {
        try {
            OpenClawConfig config;
            if (!Files.exists(configPath)) {
                log.warn("Config file not found: {}, using defaults", configPath);
                // Load shell env fallback when config is missing
                // (mirrors TS config/io.ts behavior)
                loadShellEnvIfNeeded(null);
                config = applyDefaults(new OpenClawConfig());
            } else {
                String raw = Files.readString(configPath);

                // Environment variable substitution
                raw = substituteEnvVars(raw);

                // Parse JSON
                config = objectMapper.readValue(raw, OpenClawConfig.class);

                // Apply defaults
                config = applyDefaults(config);

                // Load shell env fallback after config is loaded
                // (mirrors TS config/io.ts: loadShellEnvFallback after applyDefaults)
                loadShellEnvIfNeeded(config);

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
     * Load environment variables from the user's login shell if needed.
     * Mirrors TypeScript's loadShellEnvFallback in config/io.ts.
     */
    private void loadShellEnvIfNeeded(OpenClawConfig config) {
        try {
            // Check if shell env fallback is enabled via environment
            String envFlag = System.getenv("OPENCLAW_SHELL_ENV");
            boolean enabled = "1".equals(envFlag) || "true".equalsIgnoreCase(envFlag);

            // Also check config for env.shellEnv.enabled
            if (!enabled && config != null) {
                // Config-based enable check — if config has shellEnv settings
                // this is a simplified check; full config schema TBD
                enabled = false; // Default off unless explicitly enabled
            }

            if (!enabled) {
                return;
            }

            Map<String, String> target = new HashMap<>(System.getenv());
            ShellEnv.ShellEnvResult result = ShellEnv.loadShellEnvFallback(
                    true, target, SHELL_ENV_EXPECTED_KEYS, null);

            if (result.ok() && result.applied() != null && !result.applied().isEmpty()) {
                // Apply discovered keys as system properties
                for (String key : result.applied()) {
                    String value = target.get(key);
                    if (value != null && System.getenv(key) == null) {
                        System.setProperty(key, value);
                    }
                }
                log.info("Shell env fallback applied {} key(s): {}",
                        result.applied().size(), result.applied());
            }
        } catch (Exception e) {
            log.debug("Shell env fallback skipped: {}", e.getMessage());
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
