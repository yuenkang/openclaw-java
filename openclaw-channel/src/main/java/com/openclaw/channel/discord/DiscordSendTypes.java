package com.openclaw.channel.discord;

import java.util.List;

/**
 * Discord send operation types and constants.
 * Corresponds to TypeScript's discord/send.types.ts.
 */
public final class DiscordSendTypes {

    private DiscordSendTypes() {
    }

    // =========================================================================
    // Constants
    // =========================================================================

    public static final int TEXT_LIMIT = 2000;
    public static final int MAX_EMOJI_BYTES = 256 * 1024;
    public static final int MAX_STICKER_BYTES = 512 * 1024;
    public static final int MAX_STICKERS = 3;
    public static final int POLL_MAX_ANSWERS = 10;
    public static final int POLL_MAX_DURATION_HOURS = 32 * 24;
    public static final int MISSING_PERMISSIONS_CODE = 50013;
    public static final int CANNOT_DM_CODE = 50007;

    // =========================================================================
    // Error
    // =========================================================================

    public static class SendError extends RuntimeException {
        private String kind; // "missing-permissions" | "dm-blocked"
        private String channelId;
        private List<String> missingPermissions;

        public SendError(String message) {
            super(message);
        }

        public SendError(String message, String kind, String channelId) {
            super(message);
            this.kind = kind;
            this.channelId = channelId;
        }

        public String getKind() {
            return kind;
        }

        public void setKind(String kind) {
            this.kind = kind;
        }

        public String getChannelId() {
            return channelId;
        }

        public void setChannelId(String channelId) {
            this.channelId = channelId;
        }

        public List<String> getMissingPermissions() {
            return missingPermissions;
        }

        public void setMissingPermissions(List<String> p) {
            this.missingPermissions = p;
        }

        @Override
        public String toString() {
            return getMessage();
        }
    }

    // =========================================================================
    // Result types
    // =========================================================================

    public record SendResult(String messageId, String channelId) {
    }

    public record Recipient(String kind, String id) {
    }

    // =========================================================================
    // Option / query types
    // =========================================================================

    public record ReactOpts(String token, String accountId, boolean verbose) {
    }

    public record ReactionUser(String id, String username, String tag) {
    }

    public record ReactionEmoji(String id, String name, String raw) {
    }

    public record ReactionSummary(ReactionEmoji emoji, int count, List<ReactionUser> users) {
    }

    public record PermissionsSummary(String channelId, String guildId,
            List<String> permissions, String raw,
            boolean isDm, Integer channelType) {
    }

    public record MessageQuery(Integer limit, String before, String after, String around) {
    }

    public record MessageEdit(String content) {
    }

    public record ThreadCreate(String messageId, String name, Integer autoArchiveMinutes) {
    }

    public record ThreadList(String guildId, String channelId,
            boolean includeArchived, String before, Integer limit) {
    }

    public record SearchQuery(String guildId, String content,
            List<String> channelIds, List<String> authorIds, Integer limit) {
    }

    public record RoleChange(String guildId, String userId, String roleId) {
    }

    public record ModerationTarget(String guildId, String userId, String reason) {
    }

    public record TimeoutTarget(String guildId, String userId, String reason,
            String until, Integer durationMinutes) {
    }

    public record EmojiUpload(String guildId, String name, String mediaUrl, List<String> roleIds) {
    }

    public record StickerUpload(String guildId, String name, String description,
            String tags, String mediaUrl) {
    }

    public record ChannelCreate(String guildId, String name, Integer type,
            String parentId, String topic, Integer position, Boolean nsfw) {
    }

    public record ChannelEdit(String channelId, String name, String topic,
            Integer position, String parentId, Boolean nsfw,
            Integer rateLimitPerUser) {
    }

    public record ChannelMove(String guildId, String channelId, String parentId, Integer position) {
    }

    public record ChannelPermissionSet(String channelId, String targetId,
            int targetType, String allow, String deny) {
    }
}
