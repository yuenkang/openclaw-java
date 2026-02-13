package com.openclaw.channel.discord;

/**
 * Discord sender identity resolution types and helpers.
 * Corresponds to TypeScript's discord/monitor/sender-identity.ts.
 */
public final class DiscordSenderIdentity {

    private DiscordSenderIdentity() {
    }

    public record PluralKitSenderInfo(String memberId, String memberName,
            String systemId, String systemName) {
    }

    public record Identity(String id, String name, String tag, String label,
            boolean isPluralKit, PluralKitSenderInfo pluralkit) {
    }

    /**
     * Resolve a webhook ID from a message, normalizing empty/null values.
     */
    public static String resolveWebhookId(String webhookId) {
        if (webhookId == null)
            return null;
        String trimmed = webhookId.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    /**
     * Build a sender identity from PluralKit info.
     */
    public static Identity fromPluralKit(String memberId, String memberName,
            String systemId, String systemName) {
        String label = systemName != null && !systemName.isEmpty()
                ? memberName + " (PK:" + systemName + ")"
                : memberName + " (PK)";
        return new Identity(memberId, memberName, null, label, true,
                new PluralKitSenderInfo(memberId, memberName, systemId, systemName));
    }

    /**
     * Build a sender identity from a Discord user.
     */
    public static Identity fromUser(String userId, String username, String discriminator,
            String globalName, String nickname) {
        String tag = DiscordMonitorFormat.formatUserTag(username, discriminator, userId);
        String display = nickname != null ? nickname : (globalName != null ? globalName : username);
        String label;
        if (display != null && tag != null && !display.equals(tag)) {
            label = display + " (" + tag + ")";
        } else {
            label = display != null ? display : (tag != null ? tag : userId);
        }
        return new Identity(userId, username, tag, label, false, null);
    }
}
