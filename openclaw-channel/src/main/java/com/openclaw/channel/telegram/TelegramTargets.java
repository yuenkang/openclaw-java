package com.openclaw.channel.telegram;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Telegram delivery target parsing.
 * Corresponds to TypeScript's telegram/targets.ts.
 */
public final class TelegramTargets {

    private TelegramTargets() {
    }

    private static final Pattern TG_PREFIX = Pattern.compile("^(telegram|tg):", Pattern.CASE_INSENSITIVE);
    private static final Pattern GROUP_PREFIX = Pattern.compile("^group:", Pattern.CASE_INSENSITIVE);
    private static final Pattern TOPIC_EXPLICIT = Pattern.compile("^(.+?):topic:(\\d+)$");
    private static final Pattern TOPIC_NUMERIC = Pattern.compile("^(.+):(\\d+)$");

    /**
     * A parsed Telegram delivery target.
     */
    public record Target(String chatId, Integer messageThreadId) {
        public Target(String chatId) {
            this(chatId, null);
        }
    }

    /**
     * Strip internal Telegram prefixes (telegram:, tg:, group:).
     */
    public static String stripInternalPrefixes(String to) {
        String trimmed = to.trim();
        boolean strippedTelegramPrefix = false;
        while (true) {
            String next;
            if (TG_PREFIX.matcher(trimmed).find()) {
                strippedTelegramPrefix = true;
                next = TG_PREFIX.matcher(trimmed).replaceFirst("").trim();
            } else if (strippedTelegramPrefix && GROUP_PREFIX.matcher(trimmed).find()) {
                next = GROUP_PREFIX.matcher(trimmed).replaceFirst("").trim();
            } else {
                next = trimmed;
            }
            if (next.equals(trimmed))
                return trimmed;
            trimmed = next;
        }
    }

    /**
     * Parse a Telegram delivery target into chatId and optional topic/thread ID.
     * <p>
     * Supported formats:
     * <ul>
     * <li>{@code chatId} — plain chat ID, t.me link, @username, or internal
     * prefixes</li>
     * <li>{@code chatId:topicId} — numeric topic/thread ID</li>
     * <li>{@code chatId:topic:topicId} — explicit topic marker (preferred)</li>
     * </ul>
     */
    public static Target parse(String to) {
        String normalized = stripInternalPrefixes(to);

        // chatId:topic:123 — explicit topic marker
        Matcher topicMatch = TOPIC_EXPLICIT.matcher(normalized);
        if (topicMatch.matches()) {
            return new Target(topicMatch.group(1),
                    Integer.parseInt(topicMatch.group(2)));
        }

        // chatId:123 — implicit topic
        Matcher colonMatch = TOPIC_NUMERIC.matcher(normalized);
        if (colonMatch.matches()) {
            return new Target(colonMatch.group(1),
                    Integer.parseInt(colonMatch.group(2)));
        }

        return new Target(normalized);
    }
}
