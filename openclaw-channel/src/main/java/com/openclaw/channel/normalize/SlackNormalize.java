package com.openclaw.channel.normalize;

import java.util.regex.Pattern;

/**
 * Slack messaging target detection.
 * Corresponds to TypeScript's channels/plugins/normalize/slack.ts.
 * Note: normalizeSlackMessagingTarget depends on external parseSlackTarget;
 * only the self-contained looksLikeSlackTargetId is fully implemented here.
 */
public final class SlackNormalize {

    private SlackNormalize() {
    }

    private static final Pattern USER_MENTION = Pattern.compile(
            "^<@[A-Z0-9]+>$", Pattern.CASE_INSENSITIVE);
    private static final Pattern PREFIX = Pattern.compile(
            "^(user|channel):", Pattern.CASE_INSENSITIVE);
    private static final Pattern SLACK_PREFIX = Pattern.compile(
            "^slack:", Pattern.CASE_INSENSITIVE);
    private static final Pattern SLACK_ID = Pattern.compile(
            "^[CUWGD][A-Z0-9]{8,}$", Pattern.CASE_INSENSITIVE);

    /**
     * Check if a raw string looks like a Slack target ID.
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
        if (SLACK_PREFIX.matcher(trimmed).find())
            return true;
        if (trimmed.startsWith("@") || trimmed.startsWith("#"))
            return true;
        return SLACK_ID.matcher(trimmed).matches();
    }
}
