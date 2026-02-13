package com.openclaw.channel;

import java.util.regex.Pattern;

/**
 * Messaging target types and utilities.
 * Corresponds to TypeScript's channels/targets.ts.
 */
public final class MessagingTarget {

    private MessagingTarget() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    /** Target kind: "user" or "channel". */
    public static final String KIND_USER = "user";
    public static final String KIND_CHANNEL = "channel";

    /** A resolved messaging target. */
    public record Target(String kind, String id, String raw, String normalized) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Normalize a target ID by combining kind and id.
     */
    public static String normalizeTargetId(String kind, String id) {
        return (kind + ":" + id).toLowerCase();
    }

    /**
     * Build a MessagingTarget from components.
     */
    public static Target build(String kind, String id, String raw) {
        return new Target(kind, id, raw, normalizeTargetId(kind, id));
    }

    /**
     * Ensure a candidate ID matches a required pattern, throwing if not.
     */
    public static String ensureTargetId(String candidate, Pattern pattern, String errorMessage) {
        if (!pattern.matcher(candidate).matches()) {
            throw new IllegalArgumentException(errorMessage);
        }
        return candidate;
    }

    /**
     * Require a target of a specific kind, throwing if missing or wrong kind.
     */
    public static String requireTargetKind(String platform, Target target, String kind) {
        if (target == null) {
            throw new IllegalArgumentException(platform + " " + kind + " id is required.");
        }
        if (!kind.equals(target.kind())) {
            throw new IllegalArgumentException(
                    platform + " " + kind + " id is required (use " + kind + ":<id>).");
        }
        return target.id();
    }
}
