package com.openclaw.channel.discord;

/**
 * PluralKit integration types for Discord message attribution.
 * Corresponds to TypeScript's discord/pluralkit.ts.
 */
public final class DiscordPluralKit {

    private DiscordPluralKit() {
    }

    public static final String API_BASE = "https://api.pluralkit.me/v2";

    // =========================================================================
    // Types
    // =========================================================================

    public record Config(boolean enabled, String token) {
    }

    public record SystemInfo(String id, String name, String tag) {
    }

    public record MemberInfo(String id, String name, String displayName) {
    }

    public record MessageInfo(String id, String original, String sender,
            SystemInfo system, MemberInfo member) {
    }
}
