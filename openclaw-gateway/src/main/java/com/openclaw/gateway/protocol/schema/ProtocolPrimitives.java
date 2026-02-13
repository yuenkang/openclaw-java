package com.openclaw.gateway.protocol.schema;

/**
 * Gateway protocol primitives and constants.
 * Mirrors {@code protocol/schema/primitives.ts} and
 * {@code protocol/schema/error-codes.ts}.
 */
public final class ProtocolPrimitives {

    private ProtocolPrimitives() {
    }

    /** Maximum length for session labels. */
    public static final int SESSION_LABEL_MAX_LENGTH = 100;

    /** Protocol version. Mirrors PROTOCOL_VERSION from protocol-schemas.ts. */
    public static final int PROTOCOL_VERSION = 3;

    // ── Error Codes (from error-codes.ts) ────────────────────────

    public static final class ErrorCodes {
        public static final String NOT_LINKED = "NOT_LINKED";
        public static final String NOT_PAIRED = "NOT_PAIRED";
        public static final String AGENT_TIMEOUT = "AGENT_TIMEOUT";
        public static final String INVALID_REQUEST = "INVALID_REQUEST";
        public static final String UNAVAILABLE = "UNAVAILABLE";

        private ErrorCodes() {
        }
    }
}
