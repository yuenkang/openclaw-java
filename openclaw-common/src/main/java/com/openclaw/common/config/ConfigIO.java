package com.openclaw.common.config;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;

/**
 * Config file I/O utility functions.
 * Extracted from TypeScript's io.ts — self-contained helpers only.
 * The full config loading pipeline (createConfigIO / loadConfig) lives
 * in ConfigService.java.
 */
public final class ConfigIO {

    private static final Logger log = LoggerFactory.getLogger(ConfigIO.class);
    private static final ObjectMapper MAPPER = new ObjectMapper();

    private ConfigIO() {
    }

    // =========================================================================
    // Constants
    // =========================================================================

    public static final int CONFIG_BACKUP_COUNT = 5;

    public static final List<String> SHELL_ENV_EXPECTED_KEYS = List.of(
            "OPENAI_API_KEY",
            "ANTHROPIC_API_KEY",
            "ANTHROPIC_OAUTH_TOKEN",
            "GEMINI_API_KEY",
            "ZAI_API_KEY",
            "OPENROUTER_API_KEY",
            "AI_GATEWAY_API_KEY",
            "MINIMAX_API_KEY",
            "SYNTHETIC_API_KEY",
            "ELEVENLABS_API_KEY",
            "TELEGRAM_BOT_TOKEN",
            "DISCORD_BOT_TOKEN",
            "SLACK_BOT_TOKEN",
            "SLACK_APP_TOKEN",
            "OPENCLAW_GATEWAY_TOKEN",
            "OPENCLAW_GATEWAY_PASSWORD");

    // =========================================================================
    // Types
    // =========================================================================

    /** Result of JSON/JSON5 parsing. */
    public sealed interface ParseResult permits ParseResult.Ok, ParseResult.Fail {
        record Ok(Object parsed) implements ParseResult {
        }

        record Fail(String error) implements ParseResult {
        }
    }

    // =========================================================================
    // Hashing
    // =========================================================================

    /**
     * Compute SHA-256 hex hash of raw config text.
     */
    public static String hashConfigRaw(String raw) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest((raw != null ? raw : "").getBytes());
            return HexFormat.of().formatHex(hash);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("SHA-256 not available", e);
        }
    }

    /**
     * Resolve the hash from a snapshot, using a precomputed hash if available.
     */
    public static String resolveConfigSnapshotHash(String hash, String raw) {
        if (hash != null) {
            String trimmed = hash.trim();
            if (!trimmed.isEmpty()) {
                return trimmed;
            }
        }
        if (raw == null) {
            return null;
        }
        return hashConfigRaw(raw);
    }

    // =========================================================================
    // Coerce / Parse
    // =========================================================================

    /**
     * Coerce an unknown object to an OpenClawConfig (returns empty if invalid).
     */
    public static OpenClawConfig coerceConfig(Object value) {
        if (value == null) {
            return new OpenClawConfig();
        }
        if (value instanceof OpenClawConfig cfg) {
            return cfg;
        }
        if (value instanceof Map) {
            try {
                String json = MAPPER.writeValueAsString(value);
                return MAPPER.readValue(json, OpenClawConfig.class);
            } catch (JsonProcessingException e) {
                return new OpenClawConfig();
            }
        }
        return new OpenClawConfig();
    }

    /**
     * Parse a JSON string. Returns ParseResult.Ok or ParseResult.Fail.
     */
    public static ParseResult parseConfigJson(String raw) {
        try {
            Object parsed = MAPPER.readValue(raw, Object.class);
            return new ParseResult.Ok(parsed);
        } catch (JsonProcessingException e) {
            return new ParseResult.Fail(e.getMessage());
        }
    }

    // =========================================================================
    // Version stamping
    // =========================================================================

    /**
     * Stamp the config with the current version and timestamp in meta.
     */
    public static void stampConfigVersion(OpenClawConfig cfg, String version) {
        if (cfg.getMeta() == null) {
            cfg.setMeta(new OpenClawConfig.MetaConfig());
        }
        cfg.getMeta().setLastTouchedVersion(version);
        cfg.getMeta().setLastTouchedAt(java.time.Instant.now().toString());
    }

    /**
     * Warn if the config was last written by a newer version.
     */
    public static void warnIfConfigFromFuture(OpenClawConfig cfg, String currentVersion) {
        if (cfg == null || cfg.getMeta() == null) {
            return;
        }
        String touched = cfg.getMeta().getLastTouchedVersion();
        if (touched == null || touched.isBlank()) {
            return;
        }
        Integer cmp = OpenClawVersion.compare(currentVersion, touched);
        if (cmp != null && cmp < 0) {
            log.warn("Config was last written by a newer OpenClaw ({}); current version is {}.",
                    touched, currentVersion);
        }
    }

    // =========================================================================
    // Config warnings
    // =========================================================================

    /**
     * Warn on common config miskeys.
     */
    @SuppressWarnings("unchecked")
    public static void warnOnConfigMiskeys(Object raw) {
        if (!(raw instanceof Map<?, ?> map)) {
            return;
        }
        Object gateway = map.get("gateway");
        if (!(gateway instanceof Map<?, ?> gatewayMap)) {
            return;
        }
        if (gatewayMap.containsKey("token")) {
            log.warn("Config uses \"gateway.token\". This key is ignored; "
                    + "use \"gateway.auth.token\" instead.");
        }
    }

    // =========================================================================
    // Backup rotation
    // =========================================================================

    /**
     * Rotate config backup files (configPath.bak, .bak.1, .bak.2, ...).
     */
    public static void rotateConfigBackups(Path configPath) {
        if (CONFIG_BACKUP_COUNT <= 1) {
            return;
        }
        String base = configPath.toString() + ".bak";
        int maxIndex = CONFIG_BACKUP_COUNT - 1;

        // Delete oldest
        try {
            Files.deleteIfExists(Path.of(base + "." + maxIndex));
        } catch (IOException ignored) {
        }

        // Shift down
        for (int i = maxIndex - 1; i >= 1; i--) {
            try {
                Path src = Path.of(base + "." + i);
                Path dst = Path.of(base + "." + (i + 1));
                if (Files.exists(src)) {
                    Files.move(src, dst, StandardCopyOption.REPLACE_EXISTING);
                }
            } catch (IOException ignored) {
            }
        }

        // Move .bak -> .bak.1
        try {
            Path bakPath = Path.of(base);
            if (Files.exists(bakPath)) {
                Files.move(bakPath, Path.of(base + ".1"), StandardCopyOption.REPLACE_EXISTING);
            }
        } catch (IOException ignored) {
        }
    }

    // =========================================================================
    // Config cache helpers
    // =========================================================================

    /**
     * Resolve config cache TTL from environment.
     */
    public static int resolveConfigCacheMs() {
        return resolveConfigCacheMs(System.getenv());
    }

    /**
     * Resolve config cache TTL from the given environment map.
     */
    public static int resolveConfigCacheMs(Map<String, String> env) {
        String raw = env != null ? env.get("OPENCLAW_CONFIG_CACHE_MS") : null;
        if (raw != null) {
            raw = raw.trim();
        }
        if (raw == null || raw.isEmpty()) {
            return 200; // DEFAULT_CONFIG_CACHE_MS
        }
        if ("0".equals(raw)) {
            return 0;
        }
        try {
            int parsed = Integer.parseInt(raw);
            return Math.max(0, parsed);
        } catch (NumberFormatException e) {
            return 200;
        }
    }

    /**
     * Check whether config caching should be used.
     */
    public static boolean shouldUseConfigCache(Map<String, String> env) {
        String disable = env != null ? env.get("OPENCLAW_DISABLE_CONFIG_CACHE") : null;
        if (disable != null && !disable.trim().isEmpty()) {
            return false;
        }
        return resolveConfigCacheMs(env) > 0;
    }
}
