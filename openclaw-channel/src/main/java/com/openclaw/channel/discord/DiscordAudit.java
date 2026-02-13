package com.openclaw.channel.discord;

import java.util.List;

/**
 * Discord channel permissions audit types.
 * Corresponds to TypeScript's discord/audit.ts.
 */
public final class DiscordAudit {

    private DiscordAudit() {
    }

    public static final List<String> REQUIRED_CHANNEL_PERMISSIONS = List.of(
            "ViewChannel", "SendMessages");

    public record ChannelPermissionsEntry(
            String channelId,
            boolean ok,
            List<String> missing,
            String error,
            String matchKey,
            String matchSource) {
    }

    public record ChannelPermissionsAudit(
            boolean ok,
            int checkedChannels,
            int unresolvedChannels,
            List<ChannelPermissionsEntry> channels,
            long elapsedMs) {
    }
}
