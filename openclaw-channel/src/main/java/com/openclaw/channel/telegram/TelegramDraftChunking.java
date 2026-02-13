package com.openclaw.channel.telegram;

import java.util.Map;

/**
 * Telegram draft streaming chunk configuration.
 * Corresponds to TypeScript's telegram/draft-chunking.ts.
 */
public final class TelegramDraftChunking {

    private TelegramDraftChunking() {
    }

    public static final int DEFAULT_MIN = 200;
    public static final int DEFAULT_MAX = 800;

    public enum BreakPreference {
        PARAGRAPH, NEWLINE, SENTENCE
    }

    /**
     * Resolved draft streaming chunk configuration.
     */
    public record Config(int minChars, int maxChars, BreakPreference breakPreference) {
    }

    /**
     * Resolve Telegram draft streaming chunking parameters from config.
     *
     * @param channelsConfig the "channels" section of config
     * @param accountId      account ID
     * @param textChunkLimit overall text chunk limit (from channel dock or config)
     */
    @SuppressWarnings("unchecked")
    public static Config resolve(Map<String, Object> channelsConfig,
            String accountId,
            int textChunkLimit) {
        String effectiveId = TelegramToken.normalizeAccountId(accountId);
        Map<String, Object> telegramCfg = TelegramToken.getMap(channelsConfig, "telegram");

        // Look for draftChunk config at account level, then base level
        Map<String, Object> draftCfg = null;
        if (telegramCfg != null) {
            Map<String, Object> accounts = TelegramToken.getMap(telegramCfg, "accounts");
            if (accounts != null) {
                Map<String, Object> accountCfg = accounts.get(effectiveId) instanceof Map
                        ? (Map<String, Object>) accounts.get(effectiveId)
                        : null;
                if (accountCfg != null) {
                    draftCfg = accountCfg.get("draftChunk") instanceof Map
                            ? (Map<String, Object>) accountCfg.get("draftChunk")
                            : null;
                }
            }
            if (draftCfg == null) {
                draftCfg = telegramCfg.get("draftChunk") instanceof Map
                        ? (Map<String, Object>) telegramCfg.get("draftChunk")
                        : null;
            }
        }

        int maxRequested = Math.max(1, getInt(draftCfg, "maxChars", DEFAULT_MAX));
        int maxChars = Math.max(1, Math.min(maxRequested, textChunkLimit > 0 ? textChunkLimit : maxRequested));
        int minRequested = Math.max(1, getInt(draftCfg, "minChars", DEFAULT_MIN));
        int minChars = Math.min(minRequested, maxChars);

        BreakPreference breakPref = BreakPreference.PARAGRAPH;
        if (draftCfg != null) {
            Object bp = draftCfg.get("breakPreference");
            if ("newline".equals(bp))
                breakPref = BreakPreference.NEWLINE;
            else if ("sentence".equals(bp))
                breakPref = BreakPreference.SENTENCE;
        }

        return new Config(minChars, maxChars, breakPref);
    }

    private static int getInt(Map<String, Object> map, String key, int defaultValue) {
        if (map == null)
            return defaultValue;
        Object val = map.get(key);
        if (val instanceof Number n)
            return n.intValue();
        return defaultValue;
    }
}
