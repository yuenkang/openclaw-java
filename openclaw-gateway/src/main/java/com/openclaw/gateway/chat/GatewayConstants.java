package com.openclaw.gateway.chat;

/**
 * Gateway WebSocket and chat constants.
 * Corresponds to TypeScript's server-constants.ts.
 */
public final class GatewayConstants {

    private GatewayConstants() {
    }

    /** Maximum incoming WebSocket frame size (512 KB). */
    public static final int MAX_PAYLOAD_BYTES = 512 * 1024;

    /**
     * Per-connection send buffer limit before marking as slow consumer (1.5 MB).
     */
    public static final int MAX_BUFFERED_BYTES = (int) (1.5 * 1024 * 1024);

    /** Default maximum chat history messages bytes (6 MB). */
    public static final int DEFAULT_MAX_CHAT_HISTORY_MESSAGES_BYTES = 6 * 1024 * 1024;

    /** Default WebSocket handshake timeout. */
    public static final int DEFAULT_HANDSHAKE_TIMEOUT_MS = 10_000;

    /** Server tick interval for periodic maintenance. */
    public static final int TICK_INTERVAL_MS = 30_000;

    /** Health check refresh interval. */
    public static final int HEALTH_REFRESH_INTERVAL_MS = 60_000;

    /** Deduplication entry TTL. */
    public static final int DEDUPE_TTL_MS = 5 * 60_000;

    /** Maximum deduplication entries to track. */
    public static final int DEDUPE_MAX = 1000;

    private static volatile int maxChatHistoryMessagesBytes = DEFAULT_MAX_CHAT_HISTORY_MESSAGES_BYTES;

    public static int getMaxChatHistoryMessagesBytes() {
        return maxChatHistoryMessagesBytes;
    }

    /**
     * Test-only: override maxChatHistoryMessagesBytes.
     */
    public static void setMaxChatHistoryMessagesBytesForTest(Integer value) {
        if (value == null) {
            maxChatHistoryMessagesBytes = DEFAULT_MAX_CHAT_HISTORY_MESSAGES_BYTES;
            return;
        }
        if (value > 0) {
            maxChatHistoryMessagesBytes = value;
        }
    }
}
