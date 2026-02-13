package com.openclaw.channel.discord;

import java.util.regex.Pattern;

/**
 * Discord threading types and pure utility functions.
 * Corresponds to the non-async parts of TypeScript's
 * discord/monitor/threading.ts.
 */
public final class DiscordThreading {

    private DiscordThreading() {
    }

    // Discord channel type IDs
    public static final int PUBLIC_THREAD = 11;
    public static final int PRIVATE_THREAD = 12;
    public static final int ANNOUNCEMENT_THREAD = 10;
    public static final int GUILD_FORUM = 15;
    public static final int GUILD_MEDIA = 16;

    // =========================================================================
    // Types
    // =========================================================================

    public record ThreadChannel(String id, String name, String parentId,
            String parentName, String ownerId) {
    }

    public record ThreadStarter(String text, String author, Long timestamp) {
    }

    public record AutoThreadContext(String createdThreadId, String from, String to,
            String originatingTo, String sessionKey,
            String parentSessionKey) {
    }

    // =========================================================================
    // Utility functions
    // =========================================================================

    public static boolean isThreadType(Integer type) {
        if (type == null)
            return false;
        return type == PUBLIC_THREAD || type == PRIVATE_THREAD || type == ANNOUNCEMENT_THREAD;
    }

    public static boolean isForumParentType(Integer type) {
        if (type == null)
            return false;
        return type == GUILD_FORUM || type == GUILD_MEDIA;
    }

    private static final Pattern USER_MENTION = Pattern.compile("<@!?\\d+>");
    private static final Pattern ROLE_MENTION = Pattern.compile("<@&\\d+>");
    private static final Pattern CHANNEL_MENTION = Pattern.compile("<#\\d+>");
    private static final Pattern WHITESPACE = Pattern.compile("\\s+");

    /**
     * Sanitize a thread name by removing Discord mentions and truncating.
     */
    public static String sanitizeThreadName(String rawName, String fallbackId) {
        String cleaned = USER_MENTION.matcher(rawName).replaceAll("");
        cleaned = ROLE_MENTION.matcher(cleaned).replaceAll("");
        cleaned = CHANNEL_MENTION.matcher(cleaned).replaceAll("");
        cleaned = WHITESPACE.matcher(cleaned).replaceAll(" ").trim();
        String base = cleaned.isEmpty() ? "Thread " + fallbackId : cleaned;
        if (base.length() > 80)
            base = base.substring(0, 80);
        if (base.length() > 100)
            base = base.substring(0, 100);
        return base.isEmpty() ? "Thread " + fallbackId : base;
    }

    /**
     * Resolve reply target based on reply-to mode.
     */
    public static String resolveReplyTarget(String replyToMode, String replyToId,
            boolean hasReplied) {
        if ("off".equals(replyToMode))
            return null;
        String id = replyToId != null ? replyToId.trim() : null;
        if (id == null || id.isEmpty())
            return null;
        if ("all".equals(replyToMode))
            return id;
        // "first" mode: only reply if we haven't replied yet
        return hasReplied ? null : id;
    }
}
