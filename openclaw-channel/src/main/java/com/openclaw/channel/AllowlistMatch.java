package com.openclaw.channel;

/**
 * Allowlist match result types and formatting.
 * Corresponds to TypeScript's channels/allowlist-match.ts.
 */
public final class AllowlistMatch {

    private AllowlistMatch() {
    }

    // =========================================================================
    // Constants — match source values
    // =========================================================================

    public static final String SOURCE_WILDCARD = "wildcard";
    public static final String SOURCE_ID = "id";
    public static final String SOURCE_NAME = "name";
    public static final String SOURCE_TAG = "tag";
    public static final String SOURCE_USERNAME = "username";
    public static final String SOURCE_PREFIXED_ID = "prefixed-id";
    public static final String SOURCE_PREFIXED_USER = "prefixed-user";
    public static final String SOURCE_PREFIXED_NAME = "prefixed-name";
    public static final String SOURCE_SLUG = "slug";
    public static final String SOURCE_LOCALPART = "localpart";

    // =========================================================================
    // Types
    // =========================================================================

    /** Result of an allowlist match check. */
    public record Result(boolean allowed, String matchKey, String matchSource) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Format allowlist match metadata for logging.
     */
    public static String formatMatchMeta(String matchKey, String matchSource) {
        return "matchKey=" + (matchKey != null ? matchKey : "none")
                + " matchSource=" + (matchSource != null ? matchSource : "none");
    }
}
