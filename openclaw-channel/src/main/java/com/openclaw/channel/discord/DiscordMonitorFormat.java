package com.openclaw.channel.discord;

import java.time.Instant;

/**
 * Discord monitor formatting utilities.
 * Corresponds to TypeScript's discord/monitor/format.ts.
 */
public final class DiscordMonitorFormat {

    private DiscordMonitorFormat() {
    }

    /**
     * Resolve a human-readable system location for a Discord message context.
     */
    public static String resolveSystemLocation(boolean isDirectMessage, boolean isGroupDm,
            String guildName, String channelName) {
        if (isDirectMessage)
            return "DM";
        if (isGroupDm)
            return "Group DM #" + channelName;
        return guildName != null && !guildName.isEmpty()
                ? guildName + " #" + channelName
                : "#" + channelName;
    }

    /**
     * Format a reaction emoji for display.
     */
    public static String formatReactionEmoji(String emojiId, String emojiName) {
        if (emojiId != null && !emojiId.isEmpty() && emojiName != null && !emojiName.isEmpty()) {
            return emojiName + ":" + emojiId;
        }
        return emojiName != null ? emojiName : "emoji";
    }

    /**
     * Format a Discord user tag: username#discriminator or just username.
     */
    public static String formatUserTag(String username, String discriminator, String userId) {
        if (discriminator != null && !discriminator.isBlank() && !"0".equals(discriminator.trim())) {
            return username + "#" + discriminator.trim();
        }
        return username != null ? username : userId;
    }

    /**
     * Parse an ISO timestamp string to epoch millis, or null if invalid.
     */
    public static Long resolveTimestampMs(String timestamp) {
        if (timestamp == null || timestamp.isBlank())
            return null;
        try {
            return Instant.parse(timestamp).toEpochMilli();
        } catch (Exception e) {
            return null;
        }
    }
}
