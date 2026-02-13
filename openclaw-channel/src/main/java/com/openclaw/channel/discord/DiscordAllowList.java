package com.openclaw.channel.discord;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Discord allow-list normalization, matching, and policy resolution.
 * Corresponds to TypeScript's discord/monitor/allow-list.ts.
 */
public final class DiscordAllowList {

    private DiscordAllowList() {
    }

    private static final Pattern MENTION_PREFIX = Pattern.compile("^<@!?");
    private static final Pattern MENTION_SUFFIX = Pattern.compile(">$");
    private static final Pattern SLUG_CLEANUP = Pattern.compile("[^a-z0-9]+");
    private static final Pattern SLUG_TRIM = Pattern.compile("^-+|-+$");

    // =========================================================================
    // Types
    // =========================================================================

    public record AllowList(boolean allowAll, Set<String> ids, Set<String> names) {
    }

    public record AllowListMatch(boolean allowed, String matchKey, String matchSource) {
    }

    public record GuildEntryResolved(
            String id, String slug, boolean requireMention,
            String reactionNotifications, // "off" | "own" | "all" | "allowlist"
            List<String> users) {
    }

    public record ChannelConfigResolved(
            boolean allowed, Boolean requireMention, List<String> skills,
            Boolean enabled, List<String> users, String systemPrompt,
            Boolean includeThreadStarter, Boolean autoThread,
            String matchKey, String matchSource) {
    }

    // =========================================================================
    // Slug normalization
    // =========================================================================

    public static String normalizeSlug(String value) {
        String lower = value.trim().toLowerCase();
        if (lower.startsWith("#"))
            lower = lower.substring(1);
        lower = SLUG_CLEANUP.matcher(lower).replaceAll("-");
        lower = SLUG_TRIM.matcher(lower).replaceAll("");
        return lower;
    }

    // =========================================================================
    // Allow list normalization
    // =========================================================================

    public static AllowList normalize(List<String> raw, List<String> prefixes) {
        if (raw == null || raw.isEmpty())
            return null;
        Set<String> ids = new HashSet<>();
        Set<String> names = new HashSet<>();
        boolean allowAll = raw.stream().anyMatch(e -> "*".equals(e.trim()));

        for (String entry : raw) {
            String text = entry.trim();
            if (text.isEmpty() || "*".equals(text))
                continue;

            // Try to extract numeric ID from mention format
            String maybeId = MENTION_PREFIX.matcher(text).replaceFirst("");
            maybeId = MENTION_SUFFIX.matcher(maybeId).replaceFirst("");
            if (maybeId.matches("\\d+")) {
                ids.add(maybeId);
                continue;
            }

            // Check for known prefixes
            boolean matched = false;
            for (String prefix : prefixes) {
                if (text.startsWith(prefix)) {
                    String candidate = text.substring(prefix.length());
                    if (!candidate.isEmpty())
                        ids.add(candidate);
                    matched = true;
                    break;
                }
            }
            if (matched)
                continue;

            String slug = normalizeSlug(text);
            if (!slug.isEmpty())
                names.add(slug);
        }
        return new AllowList(allowAll, ids, names);
    }

    // =========================================================================
    // Matching
    // =========================================================================

    public static boolean matches(AllowList list, String id, String name, String tag) {
        if (list == null)
            return true;
        if (list.allowAll())
            return true;
        if (id != null && list.ids().contains(id))
            return true;
        String slug = name != null ? normalizeSlug(name) : "";
        if (!slug.isEmpty() && list.names().contains(slug))
            return true;
        if (tag != null) {
            String tagSlug = normalizeSlug(tag);
            if (!tagSlug.isEmpty() && list.names().contains(tagSlug))
                return true;
        }
        return false;
    }

    public static AllowListMatch resolveMatch(AllowList list, String id, String name, String tag) {
        if (list.allowAll())
            return new AllowListMatch(true, "*", "wildcard");
        if (id != null && list.ids().contains(id))
            return new AllowListMatch(true, id, "id");
        String slug = name != null ? normalizeSlug(name) : "";
        if (!slug.isEmpty() && list.names().contains(slug))
            return new AllowListMatch(true, slug, "name");
        if (tag != null) {
            String tagSlug = normalizeSlug(tag);
            if (!tagSlug.isEmpty() && list.names().contains(tagSlug))
                return new AllowListMatch(true, tagSlug, "tag");
        }
        return new AllowListMatch(false, null, null);
    }

    // =========================================================================
    // Policy helpers
    // =========================================================================

    /**
     * Resolves whether a group is allowed based on the group policy.
     */
    public static boolean isGroupAllowedByPolicy(String groupPolicy,
            boolean guildAllowlisted,
            boolean channelAllowlistConfigured,
            boolean channelAllowed) {
        if ("disabled".equals(groupPolicy))
            return false;
        if ("open".equals(groupPolicy))
            return true;
        // "allowlist"
        if (!guildAllowlisted)
            return false;
        if (!channelAllowlistConfigured)
            return true;
        return channelAllowed;
    }

    /**
     * Resolves whether a mention is required for a guild message.
     */
    public static boolean shouldRequireMention(boolean isGuildMessage, boolean isThread,
            Boolean channelRequireMention,
            Boolean guildRequireMention,
            boolean isAutoThreadOwnedByBot) {
        if (!isGuildMessage)
            return false;
        if (isAutoThreadOwnedByBot)
            return false;
        if (channelRequireMention != null)
            return channelRequireMention;
        if (guildRequireMention != null)
            return guildRequireMention;
        return true;
    }

    /**
     * Check if a thread is an auto-created thread owned by the bot.
     */
    public static boolean isAutoThreadOwnedByBot(boolean isThread, boolean autoThread,
            String botId, String threadOwnerId) {
        if (!isThread || !autoThread)
            return false;
        if (botId == null || botId.isBlank())
            return false;
        if (threadOwnerId == null || threadOwnerId.isBlank())
            return false;
        return botId.trim().equals(threadOwnerId.trim());
    }

    /**
     * Check if a reaction notification should be emitted.
     */
    public static boolean shouldEmitReactionNotification(String mode, String botId,
            String messageAuthorId,
            String userId, String userName,
            String userTag,
            List<String> allowlist) {
        if (mode == null)
            mode = "own";
        return switch (mode) {
            case "off" -> false;
            case "all" -> true;
            case "own" -> botId != null && botId.equals(messageAuthorId);
            case "allowlist" -> {
                AllowList list = normalize(allowlist, List.of("discord:", "user:", "pk:"));
                yield list != null && matches(list, userId, userName, userTag);
            }
            default -> false;
        };
    }
}
