package com.openclaw.channel.discord;

import com.openclaw.channel.MessagingTarget;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Discord messaging target parsing and resolution.
 * Corresponds to TypeScript's discord/targets.ts.
 */
public final class DiscordTargets {

    private DiscordTargets() {
    }

    private static final Pattern USER_MENTION = Pattern.compile("^<@!?(\\d+)>$");
    private static final Pattern SNOWFLAKE = Pattern.compile("^\\d+$");
    private static final Pattern KNOWN_FORMAT = Pattern.compile(
            "^(user:|channel:|discord:|@|<@!?)|[\\d]+$");

    /**
     * Parse a raw Discord target into a normalized MessagingTarget.Target.
     *
     * @param raw              the raw input (mention, prefix:id, @user, bare ID,
     *                         channel name)
     * @param defaultKind      the default kind for ambiguous numeric IDs (null to
     *                         error)
     * @param ambiguousMessage custom error for ambiguous targets
     * @return parsed target, or null if empty
     */
    public static MessagingTarget.Target parse(String raw, String defaultKind, String ambiguousMessage) {
        if (raw == null)
            return null;
        String trimmed = raw.trim();
        if (trimmed.isEmpty())
            return null;

        // <@123> or <@!123>
        Matcher mentionMatch = USER_MENTION.matcher(trimmed);
        if (mentionMatch.matches()) {
            return MessagingTarget.build("user", mentionMatch.group(1), trimmed);
        }

        // user:123
        if (trimmed.startsWith("user:")) {
            String id = trimmed.substring("user:".length()).trim();
            return id.isEmpty() ? null : MessagingTarget.build("user", id, trimmed);
        }
        // channel:123
        if (trimmed.startsWith("channel:")) {
            String id = trimmed.substring("channel:".length()).trim();
            return id.isEmpty() ? null : MessagingTarget.build("channel", id, trimmed);
        }
        // discord:123
        if (trimmed.startsWith("discord:")) {
            String id = trimmed.substring("discord:".length()).trim();
            return id.isEmpty() ? null : MessagingTarget.build("user", id, trimmed);
        }
        // @user
        if (trimmed.startsWith("@")) {
            String candidate = trimmed.substring(1).trim();
            if (!SNOWFLAKE.matcher(candidate).matches()) {
                throw new IllegalArgumentException(
                        "Discord DMs require a user id (use user:<id> or a <@id> mention)");
            }
            return MessagingTarget.build("user", candidate, trimmed);
        }
        // Bare numeric ID
        if (SNOWFLAKE.matcher(trimmed).matches()) {
            if (defaultKind != null) {
                return MessagingTarget.build(defaultKind, trimmed, trimmed);
            }
            throw new IllegalArgumentException(ambiguousMessage != null ? ambiguousMessage
                    : "Ambiguous Discord recipient \"" + trimmed +
                            "\". Use \"user:" + trimmed + "\" for DMs or \"channel:" +
                            trimmed + "\" for channel messages.");
        }
        // Default to channel
        return MessagingTarget.build("channel", trimmed, trimmed);
    }

    public static MessagingTarget.Target parse(String raw, String defaultKind) {
        return parse(raw, defaultKind, null);
    }

    public static MessagingTarget.Target parse(String raw) {
        return parse(raw, null, null);
    }

    /**
     * Resolve a raw string as a Discord channel ID.
     */
    public static String resolveChannelId(String raw) {
        MessagingTarget.Target target = parse(raw, "channel");
        return MessagingTarget.requireTargetKind("Discord", target, "channel");
    }

    /**
     * Check if a string likely represents a Discord username (not a mention,
     * prefix, or bare ID).
     */
    public static boolean isLikelyUsername(String input) {
        if (input == null || input.isBlank())
            return false;
        return !KNOWN_FORMAT.matcher(input).find();
    }

    /**
     * Check if the input is an explicit user lookup (mention, user: prefix, @, or
     * numeric + defaultKind=user).
     */
    public static boolean isExplicitUserLookup(String input, String defaultKind) {
        if (input == null || input.isBlank())
            return false;
        if (USER_MENTION.matcher(input).matches())
            return true;
        if (input.startsWith("user:") || input.startsWith("discord:"))
            return true;
        if (input.startsWith("@"))
            return true;
        if (SNOWFLAKE.matcher(input).matches())
            return "user".equals(defaultKind);
        return false;
    }
}
