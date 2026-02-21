package com.openclaw.autoreply.reply;

/**
 * Types for block streaming coalescing configuration.
 * Mirrors types from {@code auto-reply/reply/block-streaming.ts}.
 */
public final class BlockStreamingTypes {

    private BlockStreamingTypes() {
    }

    /** Resolved block streaming coalescing parameters. */
    public record BlockStreamingCoalescing(
            int minChars,
            int maxChars,
            int idleMs,
            String joiner,
            boolean flushOnEnqueue) {
    }

    /** Default values. */
    public static final int DEFAULT_BLOCK_STREAM_MIN = 800;
    public static final int DEFAULT_BLOCK_STREAM_MAX = 1200;
    public static final int DEFAULT_BLOCK_STREAM_COALESCE_IDLE_MS = 1000;
}
