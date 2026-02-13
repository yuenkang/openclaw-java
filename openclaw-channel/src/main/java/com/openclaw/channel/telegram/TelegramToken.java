package com.openclaw.channel.telegram;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Telegram bot token resolution.
 * Corresponds to TypeScript's telegram/token.ts.
 */
public final class TelegramToken {

    private TelegramToken() {
    }

    public static final String DEFAULT_ACCOUNT_ID = "default";

    public enum Source {
        ENV, TOKEN_FILE, CONFIG, NONE
    }

    public record Resolution(String token, Source source) {
    }

    /**
     * Resolve a Telegram bot token.
     * Priority: account tokenFile → account botToken → base tokenFile → base
     * botToken → env.
     */
    @SuppressWarnings("unchecked")
    public static Resolution resolve(Map<String, Object> channelsConfig,
            String accountId,
            String envToken,
            Consumer<String> logMissingFile) {
        String effectiveId = normalizeAccountId(accountId);
        Map<String, Object> telegramCfg = getMap(channelsConfig, "telegram");

        // Account-level config
        if (telegramCfg != null) {
            Map<String, Object> accountCfg = resolveAccountCfg(telegramCfg, effectiveId);
            if (accountCfg != null) {
                // Account tokenFile
                String tokenFile = asStringTrimmed(accountCfg.get("tokenFile"));
                if (tokenFile != null) {
                    Resolution fileRes = readTokenFile(tokenFile,
                            "channels.telegram.accounts." + effectiveId + ".tokenFile", logMissingFile);
                    return fileRes;
                }
                // Account botToken
                String botToken = asStringTrimmed(accountCfg.get("botToken"));
                if (botToken != null)
                    return new Resolution(botToken, Source.CONFIG);
            }
        }

        boolean allowEnv = DEFAULT_ACCOUNT_ID.equals(effectiveId);

        // Base tokenFile
        if (allowEnv && telegramCfg != null) {
            String tokenFile = asStringTrimmed(telegramCfg.get("tokenFile"));
            if (tokenFile != null) {
                return readTokenFile(tokenFile, "channels.telegram.tokenFile", logMissingFile);
            }
        }

        // Base botToken
        if (allowEnv && telegramCfg != null) {
            String configToken = asStringTrimmed(telegramCfg.get("botToken"));
            if (configToken != null)
                return new Resolution(configToken, Source.CONFIG);
        }

        // Environment variable
        if (allowEnv) {
            String env = envToken != null ? envToken : System.getenv("TELEGRAM_BOT_TOKEN");
            if (env != null && !env.isBlank()) {
                return new Resolution(env.trim(), Source.ENV);
            }
        }

        return new Resolution("", Source.NONE);
    }

    public static Resolution resolve(Map<String, Object> channelsConfig, String accountId) {
        return resolve(channelsConfig, accountId, null, null);
    }

    // =========================================================================
    // Internal helpers
    // =========================================================================

    private static Resolution readTokenFile(String path, String label, Consumer<String> log) {
        Path tokenPath = Path.of(path);
        if (!Files.exists(tokenPath)) {
            if (log != null)
                log.accept(label + " not found: " + path);
            return new Resolution("", Source.NONE);
        }
        try {
            String token = Files.readString(tokenPath).trim();
            return token.isEmpty()
                    ? new Resolution("", Source.NONE)
                    : new Resolution(token, Source.TOKEN_FILE);
        } catch (IOException e) {
            if (log != null)
                log.accept(label + " read failed: " + e.getMessage());
            return new Resolution("", Source.NONE);
        }
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> resolveAccountCfg(Map<String, Object> telegramCfg, String accountId) {
        Map<String, Object> accounts = getMap(telegramCfg, "accounts");
        if (accounts == null)
            return null;

        // Direct match
        Object direct = accounts.get(accountId);
        if (direct instanceof Map)
            return (Map<String, Object>) direct;

        // Normalized key fallback
        for (Map.Entry<String, Object> entry : accounts.entrySet()) {
            if (normalizeAccountId(entry.getKey()).equals(accountId) && entry.getValue() instanceof Map) {
                return (Map<String, Object>) entry.getValue();
            }
        }
        return null;
    }

    static String normalizeAccountId(String accountId) {
        if (accountId == null || accountId.isBlank())
            return DEFAULT_ACCOUNT_ID;
        return accountId.trim().toLowerCase();
    }

    @SuppressWarnings("unchecked")
    static Map<String, Object> getMap(Map<String, Object> map, String key) {
        if (map == null)
            return null;
        Object val = map.get(key);
        return val instanceof Map ? (Map<String, Object>) val : null;
    }

    private static String asStringTrimmed(Object val) {
        if (val instanceof String s) {
            String trimmed = s.trim();
            return trimmed.isEmpty() ? null : trimmed;
        }
        return null;
    }
}
