package com.openclaw.channel.normalize;

import java.util.regex.Pattern;

/**
 * Discord messaging target detection.
 * Corresponds to TypeScript's channels/plugins/normalize/discord.ts.
 * Note: normalizeDiscordMessagingTarget depends on external parseDiscordTarget;
 * only the self-contained looksLikeDiscordTargetId is fully implemented here.
 */
public final class DiscordNormalize {

    private DiscordNormalize() {
    }

    private static final Pattern USER_MENTION = Pattern.compile("^<@!?\\d+>$");
    private static final Pattern PREFIX = Pattern.compile(
            "^(user|channel|discord):", Pattern.CASE_INSENSITIVE);
    private static final Pattern SNOWFLAKE = Pattern.compile("^\\d{6,}$");

    /**
     * Check if a raw string looks like a Discord target ID.
     */
    public static boolean looksLikeTargetId(String raw) {
        if (raw == null)
            return false;
        String trimmed = raw.trim();
        if (trimmed.isEmpty())
            return false;

        if (USER_MENTION.matcher(trimmed).matches())
            return true;
        if (PREFIX.matcher(trimmed).find())
            return true;
        return SNOWFLAKE.matcher(trimmed).matches();
    }
}
