package com.openclaw.agent.embedded;

/**
 * Common types for the pi-embedded subsystem.
 * Mirrors {@code agents/pi-embedded-helpers/types.ts}.
 */
public final class EmbeddedTypes {

    private EmbeddedTypes() {
    }

    /** A context file injected into the embedded prompt. */
    public record EmbeddedContextFile(String path, String content) {
    }

    /** Reasons for failing over to a different model. */
    public enum FailoverReason {
        AUTH, FORMAT, RATE_LIMIT, BILLING, TIMEOUT, UNKNOWN;

        public String toValue() {
            return name().toLowerCase();
        }

        public static FailoverReason fromValue(String value) {
            if (value == null)
                return UNKNOWN;
            return switch (value.toLowerCase()) {
                case "auth" -> AUTH;
                case "format" -> FORMAT;
                case "rate_limit" -> RATE_LIMIT;
                case "billing" -> BILLING;
                case "timeout" -> TIMEOUT;
                default -> UNKNOWN;
            };
        }
    }
}
