package com.openclaw.channel.discord;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Discord send utilities: emoji normalization, recipient parsing, and
 * permission helpers.
 * Corresponds to shared logic from TypeScript's discord/send.shared.ts and
 * send.permissions.ts.
 */
public final class DiscordSendUtils {

    private DiscordSendUtils() {
    }

    // =========================================================================
    // Emoji / Reaction helpers
    // =========================================================================

    private static final Pattern CUSTOM_EMOJI = Pattern.compile("^<a?:([a-zA-Z0-9_]+):(\\d+)>$");
    private static final Pattern ANIMATED_PREFIX = Pattern.compile("^a:");

    /**
     * Normalize a reaction emoji for use in Discord API URLs.
     * Custom emojis → name:id, standard emojis → URL-encoded.
     */
    public static String normalizeReactionEmoji(String raw) {
        String trimmed = raw.trim();
        Matcher custom = CUSTOM_EMOJI.matcher(trimmed);
        if (custom.matches()) {
            return custom.group(1) + ":" + custom.group(2);
        }
        String stripped = ANIMATED_PREFIX.matcher(trimmed).replaceFirst("");
        try {
            return URLEncoder.encode(stripped, StandardCharsets.UTF_8.name());
        } catch (UnsupportedEncodingException e) {
            return stripped;
        }
    }

    /**
     * Build a reaction identifier string from emoji components.
     */
    public static String buildReactionIdentifier(String emojiId, String emojiName) {
        if (emojiId != null && !emojiId.isBlank()) {
            String name = emojiName != null && !emojiName.isBlank() ? emojiName : "_";
            return name + ":" + emojiId;
        }
        return emojiName;
    }

    /**
     * Format an emoji for display.
     */
    public static String formatReactionEmoji(String emojiId, String emojiName) {
        if (emojiId != null && !emojiId.isBlank()) {
            String name = emojiName != null && !emojiName.isBlank() ? emojiName : "_";
            return "<:" + name + ":" + emojiId + ">";
        }
        return emojiName != null ? emojiName : "?";
    }

    /**
     * Parse a raw recipient string into a Recipient (user or channel).
     */
    public static DiscordSendTypes.Recipient parseRecipient(String raw) {
        var target = DiscordTargets.parse(raw, null, null);
        if (target == null) {
            throw new IllegalArgumentException("Invalid Discord recipient: " + raw);
        }
        return new DiscordSendTypes.Recipient(target.kind(), target.id());
    }

    /**
     * Normalize sticker IDs: trim, deduplicate, limit to 3.
     */
    public static List<String> normalizeStickerIds(List<String> ids) {
        if (ids == null || ids.isEmpty())
            return List.of();
        List<String> result = new ArrayList<>();
        for (String id : ids) {
            String trimmed = id != null ? id.trim() : "";
            if (!trimmed.isEmpty() && !result.contains(trimmed)) {
                result.add(trimmed);
            }
            if (result.size() >= DiscordSendTypes.MAX_STICKERS)
                break;
        }
        return result;
    }

    /**
     * Normalize an emoji name for upload (alphanumeric + underscores, 2-32 chars).
     */
    public static String normalizeEmojiName(String raw, String label) {
        String name = raw.trim().replaceAll("[^a-zA-Z0-9_]", "_");
        if (name.length() < 2 || name.length() > 32) {
            throw new IllegalArgumentException(
                    label + " must be 2-32 alphanumeric/underscore characters");
        }
        return name;
    }

    // =========================================================================
    // Permission helpers
    // =========================================================================

    /** Thread channel type IDs (public, private, news threads). */
    public static boolean isThreadChannelType(Integer channelType) {
        if (channelType == null)
            return false;
        return channelType == 11 || channelType == 12 || channelType == 10;
    }
}
