package com.openclaw.channel;

import java.util.Map;
import java.util.regex.Pattern;

/**
 * Group mention policies per channel — resolves require‑mention and tool
 * policies
 * for Telegram, Discord, Slack, WhatsApp, iMessage, Google Chat, BlueBubbles.
 * Corresponds to TypeScript's channels/plugins/group-mentions.ts.
 */
public final class GroupMentions {

    private GroupMentions() {
    }

    // =========================================================================
    // Slug normalization (shared by Discord / Slack)
    // =========================================================================

    private static final Pattern DISCORD_STRIP_PREFIX = Pattern.compile("^[@#]+");
    private static final Pattern DISCORD_SPACE_TO_DASH = Pattern.compile("[\\s_]+");
    private static final Pattern DISCORD_INVALID_CHARS = Pattern.compile("[^a-z0-9-]+");
    private static final Pattern MULTI_DASH = Pattern.compile("-{2,}");
    private static final Pattern TRIM_DASHES = Pattern.compile("^-+|-+$");

    public static String normalizeDiscordSlug(String value) {
        if (value == null || value.isBlank()) {
            return "";
        }
        String text = value.trim().toLowerCase();
        text = DISCORD_STRIP_PREFIX.matcher(text).replaceAll("");
        text = DISCORD_SPACE_TO_DASH.matcher(text).replaceAll("-");
        text = DISCORD_INVALID_CHARS.matcher(text).replaceAll("-");
        text = MULTI_DASH.matcher(text).replaceAll("-");
        text = TRIM_DASHES.matcher(text).replaceAll("");
        return text;
    }

    private static final Pattern SLACK_SPACE = Pattern.compile("\\s+");
    private static final Pattern SLACK_INVALID = Pattern.compile("[^a-z0-9#@._+\\-]+");
    private static final Pattern SLACK_TRIM = Pattern.compile("^[-.]+|[-.]+$");

    public static String normalizeSlackSlug(String raw) {
        if (raw == null || raw.isBlank()) {
            return "";
        }
        String text = raw.trim().toLowerCase();
        text = SLACK_SPACE.matcher(text).replaceAll("-");
        text = SLACK_INVALID.matcher(text).replaceAll("-");
        text = MULTI_DASH.matcher(text).replaceAll("-");
        text = SLACK_TRIM.matcher(text).replaceAll("");
        return text;
    }

    // =========================================================================
    // Telegram group ID parsing
    // =========================================================================

    public record TelegramGroupId(String chatId, String topicId) {
    }

    private static final Pattern DIGITS = Pattern.compile("^-?\\d+$");
    private static final Pattern POSITIVE_DIGITS = Pattern.compile("^\\d+$");

    public static TelegramGroupId parseTelegramGroupId(String value) {
        if (value == null || value.isBlank()) {
            return new TelegramGroupId(null, null);
        }
        String raw = value.trim();
        String[] parts = raw.split(":");
        // Filter empty parts
        String[] nonEmpty = java.util.Arrays.stream(parts)
                .filter(p -> !p.isEmpty())
                .toArray(String[]::new);

        if (nonEmpty.length >= 3
                && "topic".equals(nonEmpty[1])
                && DIGITS.matcher(nonEmpty[0]).matches()
                && POSITIVE_DIGITS.matcher(nonEmpty[2]).matches()) {
            return new TelegramGroupId(nonEmpty[0], nonEmpty[2]);
        }
        if (nonEmpty.length >= 2
                && DIGITS.matcher(nonEmpty[0]).matches()
                && POSITIVE_DIGITS.matcher(nonEmpty[1]).matches()) {
            return new TelegramGroupId(nonEmpty[0], nonEmpty[1]);
        }
        return new TelegramGroupId(raw, null);
    }

    // =========================================================================
    // Per-channel require-mention resolution
    // =========================================================================

    /**
     * Resolve require-mention for Telegram groups.
     * Checks topic → group → wildcard → channel-level group policy.
     */
    @SuppressWarnings("unchecked")
    public static Boolean resolveTelegramGroupRequireMention(
            Map<String, Object> channelsConfig,
            String groupId, String accountId) {
        TelegramGroupId parsed = parseTelegramGroupId(groupId);
        if (parsed.chatId() == null) {
            return null;
        }
        Map<String, Object> telegram = getMap(channelsConfig, "telegram");
        if (telegram == null) {
            return null;
        }
        Map<String, Object> groups = getMap(telegram, "groups");
        if (groups != null) {
            // Specific group config
            Map<String, Object> groupConfig = getMap(groups, parsed.chatId());
            Map<String, Object> wildcardConfig = getMap(groups, "*");

            // Check topic-level
            if (parsed.topicId() != null) {
                Boolean topicResult = getTopicRequireMention(groupConfig, parsed.topicId());
                if (topicResult != null)
                    return topicResult;
                topicResult = getTopicRequireMention(wildcardConfig, parsed.topicId());
                if (topicResult != null)
                    return topicResult;
            }

            // Check group-level
            Boolean groupResult = getBooleanField(groupConfig, "requireMention");
            if (groupResult != null)
                return groupResult;
            groupResult = getBooleanField(wildcardConfig, "requireMention");
            if (groupResult != null)
                return groupResult;
        }
        return null;
    }

    /**
     * Resolve require-mention for Discord groups using guild → channel hierarchy.
     */
    @SuppressWarnings("unchecked")
    public static boolean resolveDiscordGroupRequireMention(
            Map<String, Object> channelsConfig,
            String groupId, String groupChannel, String groupSpace) {
        Map<String, Object> discord = getMap(channelsConfig, "discord");
        Map<String, Object> guilds = discord != null ? getMap(discord, "guilds") : null;
        Map<String, Object> guildEntry = resolveDiscordGuildEntry(guilds, groupSpace);

        if (guildEntry != null) {
            Map<String, Object> channels = getMap(guildEntry, "channels");
            if (channels != null && !channels.isEmpty()) {
                String channelSlug = normalizeDiscordSlug(groupChannel);
                Map<String, Object> entry = findChannelEntry(channels, groupId, channelSlug, groupChannel);
                if (entry != null) {
                    Boolean val = getBooleanField(entry, "requireMention");
                    if (val != null)
                        return val;
                }
            }
            Boolean guildVal = getBooleanField(guildEntry, "requireMention");
            if (guildVal != null)
                return guildVal;
        }
        return true; // default for Discord
    }

    /**
     * Resolve require-mention for Slack groups.
     */
    @SuppressWarnings("unchecked")
    public static boolean resolveSlackGroupRequireMention(
            Map<String, Object> slackAccountConfig,
            String groupId, String groupChannel) {
        Map<String, Object> channels = getMap(slackAccountConfig, "channels");
        if (channels == null || channels.isEmpty()) {
            return true;
        }
        Map<String, Object> matched = findSlackChannelEntry(channels, groupId, groupChannel);
        Map<String, Object> fallback = getMap(channels, "*");
        Map<String, Object> resolved = matched != null ? matched : fallback;
        if (resolved != null) {
            Boolean val = getBooleanField(resolved, "requireMention");
            if (val != null)
                return val;
        }
        return true;
    }

    // =========================================================================
    // Internal helpers
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static Map<String, Object> getMap(Map<String, Object> parent, String key) {
        if (parent == null)
            return null;
        Object val = parent.get(key);
        return val instanceof Map ? (Map<String, Object>) val : null;
    }

    private static Boolean getBooleanField(Map<String, Object> map, String key) {
        if (map == null)
            return null;
        Object val = map.get(key);
        return val instanceof Boolean ? (Boolean) val : null;
    }

    @SuppressWarnings("unchecked")
    private static Boolean getTopicRequireMention(Map<String, Object> groupConfig, String topicId) {
        if (groupConfig == null)
            return null;
        Map<String, Object> topics = getMap(groupConfig, "topics");
        if (topics == null)
            return null;
        Map<String, Object> topicConfig = getMap(topics, topicId);
        return getBooleanField(topicConfig, "requireMention");
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> resolveDiscordGuildEntry(
            Map<String, Object> guilds, String groupSpace) {
        if (guilds == null || guilds.isEmpty())
            return null;
        String space = groupSpace != null ? groupSpace.trim() : "";
        if (!space.isEmpty() && guilds.containsKey(space)) {
            return getMap(guilds, space);
        }
        String normalized = normalizeDiscordSlug(space);
        if (!normalized.isEmpty() && guilds.containsKey(normalized)) {
            return getMap(guilds, normalized);
        }
        // Check slug field match
        if (!normalized.isEmpty()) {
            for (Object entryVal : guilds.values()) {
                if (entryVal instanceof Map) {
                    Map<String, Object> entry = (Map<String, Object>) entryVal;
                    String slug = entry.get("slug") != null ? entry.get("slug").toString() : null;
                    if (normalized.equals(normalizeDiscordSlug(slug))) {
                        return entry;
                    }
                }
            }
        }
        return getMap(guilds, "*");
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> findChannelEntry(Map<String, Object> channels,
            String groupId, String channelSlug,
            String groupChannel) {
        if (groupId != null && channels.containsKey(groupId)) {
            return getMap(channels, groupId);
        }
        if (channelSlug != null && !channelSlug.isEmpty()) {
            if (channels.containsKey(channelSlug)) {
                return getMap(channels, channelSlug);
            }
            if (channels.containsKey("#" + channelSlug)) {
                return getMap(channels, "#" + channelSlug);
            }
        }
        if (groupChannel != null) {
            String alt = normalizeDiscordSlug(groupChannel);
            if (!alt.isEmpty() && channels.containsKey(alt)) {
                return getMap(channels, alt);
            }
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> findSlackChannelEntry(Map<String, Object> channels,
            String groupId, String groupChannel) {
        String channelId = groupId != null ? groupId.trim() : null;
        String channelName = groupChannel != null ? groupChannel.replaceFirst("^#", "") : null;
        String normalized = normalizeSlackSlug(channelName);

        String[] candidates = new String[] {
                channelId,
                channelName != null ? "#" + channelName : null,
                channelName,
                normalized.isEmpty() ? null : normalized
        };

        for (String candidate : candidates) {
            if (candidate != null && !candidate.isEmpty() && channels.containsKey(candidate)) {
                return getMap(channels, candidate);
            }
        }
        return null;
    }
}
