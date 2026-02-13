package com.openclaw.gateway.agent;

/**
 * Resolves assistant identity (name, avatar, emoji) for display.
 * Corresponds to TypeScript's assistant-identity.ts.
 */
public final class AssistantIdentityResolver {

    private AssistantIdentityResolver() {
    }

    private static final int MAX_ASSISTANT_NAME = 50;
    private static final int MAX_ASSISTANT_AVATAR = 200;
    private static final int MAX_ASSISTANT_EMOJI = 16;

    public static final AssistantIdentity DEFAULT = new AssistantIdentity(
            "main", "Assistant", "A", null);

    // =========================================================================
    // Types
    // =========================================================================

    public record AssistantIdentity(
            String agentId,
            String name,
            String avatar,
            String emoji) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Resolve assistant identity from agent config values.
     *
     * @param agentId      agent identifier (nullable, defaults to "main")
     * @param configName   name from config ui.assistant.name
     * @param configAvatar avatar from config ui.assistant.avatar
     * @param agentName    name from agent identity
     * @param agentAvatar  avatar from agent identity
     * @param agentEmoji   emoji from agent identity
     */
    public static AssistantIdentity resolve(
            String agentId,
            String configName, String configAvatar,
            String agentName, String agentAvatar, String agentEmoji) {

        String resolvedId = agentId != null && !agentId.isBlank() ? agentId.trim() : "main";

        String name = firstNonNull(
                coerce(configName, MAX_ASSISTANT_NAME),
                coerce(agentName, MAX_ASSISTANT_NAME),
                DEFAULT.name());

        String avatar = firstNonNull(
                normalizeAvatarValue(coerce(configAvatar, MAX_ASSISTANT_AVATAR)),
                normalizeAvatarValue(coerce(agentAvatar, MAX_ASSISTANT_AVATAR)),
                normalizeAvatarValue(coerce(agentEmoji, MAX_ASSISTANT_AVATAR)),
                DEFAULT.avatar());

        String emoji = firstNonNull(
                normalizeEmojiValue(coerce(agentEmoji, MAX_ASSISTANT_EMOJI)),
                normalizeEmojiValue(coerce(agentAvatar, MAX_ASSISTANT_EMOJI)));

        return new AssistantIdentity(resolvedId, name, avatar, emoji);
    }

    // =========================================================================
    // Internal helpers
    // =========================================================================

    static String coerce(String value, int maxLength) {
        if (value == null)
            return null;
        String trimmed = value.trim();
        if (trimmed.isEmpty())
            return null;
        return trimmed.length() <= maxLength ? trimmed : trimmed.substring(0, maxLength);
    }

    static boolean isAvatarUrl(String value) {
        return value.matches("(?i)^https?://.*") || value.matches("(?i)^data:image/.*");
    }

    static boolean looksLikeAvatarPath(String value) {
        if (value.contains("/") || value.contains("\\"))
            return true;
        return value.matches("(?i).*\\.(png|jpe?g|gif|webp|svg|ico)$");
    }

    static String normalizeAvatarValue(String value) {
        if (value == null || value.isBlank())
            return null;
        String trimmed = value.trim();
        if (isAvatarUrl(trimmed))
            return trimmed;
        if (looksLikeAvatarPath(trimmed))
            return trimmed;
        // Single short chars (emoji) are valid
        if (!trimmed.contains(" ") && trimmed.length() <= 4)
            return trimmed;
        return null;
    }

    static String normalizeEmojiValue(String value) {
        if (value == null || value.isBlank())
            return null;
        String trimmed = value.trim();
        if (trimmed.length() > MAX_ASSISTANT_EMOJI)
            return null;
        // Must contain at least one non-ASCII char
        boolean hasNonAscii = false;
        for (int i = 0; i < trimmed.length(); i++) {
            if (trimmed.charAt(i) > 127) {
                hasNonAscii = true;
                break;
            }
        }
        if (!hasNonAscii)
            return null;
        if (isAvatarUrl(trimmed) || looksLikeAvatarPath(trimmed))
            return null;
        return trimmed;
    }

    @SafeVarargs
    private static <T> T firstNonNull(T... values) {
        for (T v : values) {
            if (v != null)
                return v;
        }
        return null;
    }
}
