package com.openclaw.autoreply.reply;

/**
 * Auth profile resolution for directive handling — mask API keys,
 * resolve auth labels and profile overrides.
 * Mirrors {@code auto-reply/reply/directive-handling.auth.ts}.
 */
public final class DirectiveHandlingAuth {

    private DirectiveHandlingAuth() {
    }

    /** Detail mode for auth label display. */
    public enum ModelAuthDetailMode {
        COMPACT,
        VERBOSE
    }

    /** Resolved auth label. */
    public record AuthLabel(String label, String source) {
    }

    /** Profile override resolution result. */
    public record ProfileOverrideResult(String profileId, String error) {
        public static ProfileOverrideResult success(String profileId) {
            return new ProfileOverrideResult(profileId, null);
        }

        public static ProfileOverrideResult failure(String error) {
            return new ProfileOverrideResult(null, error);
        }

        public static ProfileOverrideResult empty() {
            return new ProfileOverrideResult(null, null);
        }
    }

    /**
     * Mask an API key for display (first 8 + last 8 chars).
     */
    public static String maskApiKey(String value) {
        if (value == null)
            return "missing";
        String trimmed = value.trim();
        if (trimmed.isEmpty())
            return "missing";
        if (trimmed.length() <= 16)
            return trimmed;
        return trimmed.substring(0, 8) + "..." + trimmed.substring(trimmed.length() - 8);
    }

    /**
     * Format an auth label for display.
     */
    public static String formatAuthLabel(AuthLabel auth) {
        if (auth.source() == null || auth.source().isEmpty()
                || auth.source().equals(auth.label()) || "missing".equals(auth.source())) {
            return auth.label();
        }
        return auth.label() + " (" + auth.source() + ")";
    }

    /**
     * Resolve a profile override from a raw profile name.
     *
     * @param rawProfile the raw profile name (may be null/empty)
     * @param provider   the expected provider
     * @return result with profileId or error
     */
    public static ProfileOverrideResult resolveProfileOverride(
            String rawProfile, String provider) {

        if (rawProfile == null || rawProfile.isBlank()) {
            return ProfileOverrideResult.empty();
        }
        String raw = rawProfile.trim();

        // Full auth profile store integration deferred — basic validation only
        if (raw.isEmpty()) {
            return ProfileOverrideResult.empty();
        }

        // Stub: accept any profile name for now
        return ProfileOverrideResult.success(raw);
    }

    /**
     * Format a "cooldown until" duration.
     */
    public static String formatUntil(long timestampMs) {
        long now = System.currentTimeMillis();
        long remainingMs = Math.max(0, timestampMs - now);
        long minutes = Math.round(remainingMs / 60_000.0);
        if (minutes < 1)
            return "soon";
        if (minutes < 60)
            return minutes + "m";
        long hours = Math.round(minutes / 60.0);
        if (hours < 48)
            return hours + "h";
        long days = Math.round(hours / 24.0);
        return days + "d";
    }
}
