package com.openclaw.gateway.server;

import java.nio.charset.StandardCharsets;

/**
 * WebSocket close-reason truncation utility.
 * <p>
 * The WebSocket spec limits close-frame reason to 123 bytes.
 * This mirrors {@code server/close-reason.ts}.
 */
public final class CloseReason {

    private CloseReason() {
    }

    /** Maximum bytes allowed in a WebSocket close-reason payload. */
    public static final int CLOSE_REASON_MAX_BYTES = 120;

    /**
     * Truncate a close-reason string so that its UTF-8 encoded form
     * fits within {@link #CLOSE_REASON_MAX_BYTES} bytes.
     */
    public static String truncate(String reason) {
        return truncate(reason, CLOSE_REASON_MAX_BYTES);
    }

    /**
     * Truncate a close-reason string to the given byte limit.
     */
    public static String truncate(String reason, int maxBytes) {
        if (reason == null || reason.isEmpty()) {
            return "invalid handshake";
        }
        byte[] encoded = reason.getBytes(StandardCharsets.UTF_8);
        if (encoded.length <= maxBytes) {
            return reason;
        }
        // Truncate at byte level and decode back (may drop trailing partial char).
        return new String(encoded, 0, maxBytes, StandardCharsets.UTF_8);
    }
}
