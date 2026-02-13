package com.openclaw.channel.discord;

import java.util.Map;

/**
 * Discord bot token resolution.
 * Corresponds to TypeScript's discord/token.ts.
 */
public final class DiscordToken {

    private DiscordToken() {
    }

    public static final String DEFAULT_ACCOUNT_ID = "default";

    public enum Source {
        ENV, CONFIG, NONE
    }

    public record Resolution(String token, Source source) {
    }

    /**
     * Normalize a Discord bot token: trim, strip leading "Bot " prefix.
     */
    public static String normalize(String raw) {
        if (raw == null || raw.isBlank())
            return null;
        String trimmed = raw.trim();
        return trimmed.replaceFirst("(?i)^Bot\\s+", "");
    }

    /**
     * Resolve a Discord bot token from config and environment.
     * Priority: account config token → base config token → env DISCORD_BOT_TOKEN.
     */
    @SuppressWarnings("unchecked")
    public static Resolution resolve(Map<String, Object> channelsConfig, String accountId, String envToken) {
        String effectiveAccountId = normalizeAccountId(accountId);
        Map<String, Object> discordCfg = getMap(channelsConfig, "discord");

        // Account-level token
        if (discordCfg != null) {
            Map<String, Object> accounts = getMap(discordCfg, "accounts");
            if (accounts != null) {
                Map<String, Object> accountCfg = getMap(accounts, effectiveAccountId);
                if (accountCfg == null && DEFAULT_ACCOUNT_ID.equals(effectiveAccountId)) {
                    accountCfg = getMap(accounts, DEFAULT_ACCOUNT_ID);
                }
                if (accountCfg != null) {
                    String token = normalize(asString(accountCfg.get("token")));
                    if (token != null)
                        return new Resolution(token, Source.CONFIG);
                }
            }
        }

        boolean allowEnv = DEFAULT_ACCOUNT_ID.equals(effectiveAccountId);

        // Base config token
        if (allowEnv && discordCfg != null) {
            String configToken = normalize(asString(discordCfg.get("token")));
            if (configToken != null)
                return new Resolution(configToken, Source.CONFIG);
        }

        // Environment variable
        if (allowEnv) {
            String env = envToken != null ? envToken : System.getenv("DISCORD_BOT_TOKEN");
            String normalizedEnv = normalize(env);
            if (normalizedEnv != null)
                return new Resolution(normalizedEnv, Source.ENV);
        }

        return new Resolution("", Source.NONE);
    }

    public static Resolution resolve(Map<String, Object> channelsConfig, String accountId) {
        return resolve(channelsConfig, accountId, null);
    }

    // =========================================================================
    // Internal helpers
    // =========================================================================

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

    static String asString(Object val) {
        return val instanceof String s ? s : null;
    }
}
