package com.openclaw.channel.normalize;

import java.util.regex.Pattern;

/**
 * Telegram messaging target normalization.
 * Corresponds to TypeScript's channels/plugins/normalize/telegram.ts.
 */
public final class TelegramNormalize {

    private TelegramNormalize() {
    }

    private static final Pattern TME_URL = Pattern.compile(
            "^https?://t\\.me/([A-Za-z0-9_]+)$", Pattern.CASE_INSENSITIVE);
    private static final Pattern TME_SHORT = Pattern.compile(
            "^t\\.me/([A-Za-z0-9_]+)$", Pattern.CASE_INSENSITIVE);
    private static final Pattern TG_PREFIX = Pattern.compile(
            "^(telegram|tg):", Pattern.CASE_INSENSITIVE);
    private static final Pattern NUMERIC_ID = Pattern.compile("^-?\\d{6,}$");

    /**
     * Normalize a raw Telegram messaging target into a canonical form.
     *
     * @return normalized target like "telegram:@username" or "telegram:-12345", or
     *         null
     */
    public static String normalizeMessagingTarget(String raw) {
        if (raw == null)
            return null;
        String trimmed = raw.trim();
        if (trimmed.isEmpty())
            return null;

        String normalized = trimmed;
        if (normalized.toLowerCase().startsWith("telegram:")) {
            normalized = normalized.substring("telegram:".length()).trim();
        } else if (normalized.toLowerCase().startsWith("tg:")) {
            normalized = normalized.substring("tg:".length()).trim();
        }
        if (normalized.isEmpty())
            return null;

        // Check t.me URL
        var matcher = TME_URL.matcher(normalized);
        if (matcher.matches()) {
            normalized = "@" + matcher.group(1);
        } else {
            matcher = TME_SHORT.matcher(normalized);
            if (matcher.matches()) {
                normalized = "@" + matcher.group(1);
            }
        }
        if (normalized.isEmpty())
            return null;

        return ("telegram:" + normalized).toLowerCase();
    }

    /**
     * Check if a raw string looks like a Telegram target ID.
     */
    public static boolean looksLikeTargetId(String raw) {
        if (raw == null)
            return false;
        String trimmed = raw.trim();
        if (trimmed.isEmpty())
            return false;

        if (TG_PREFIX.matcher(trimmed).find())
            return true;
        if (trimmed.startsWith("@"))
            return true;
        return NUMERIC_ID.matcher(trimmed).matches();
    }
}
