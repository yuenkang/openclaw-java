package com.openclaw.autoreply.reply;

import com.openclaw.autoreply.ReplyTokens;

/**
 * Memory flush settings resolution and threshold checks.
 * Triggers a pre-compaction memory flush turn when token usage approaches the
 * context window.
 * Mirrors {@code auto-reply/reply/memory-flush.ts}.
 */
public final class MemoryFlush {

    private MemoryFlush() {
    }

    public static final int DEFAULT_MEMORY_FLUSH_SOFT_TOKENS = 4000;

    public static final String DEFAULT_MEMORY_FLUSH_PROMPT = "Pre-compaction memory flush. " +
            "Store durable memories now (use memory/YYYY-MM-DD.md; create memory/ if needed). " +
            "If nothing to store, reply with " + ReplyTokens.SILENT_REPLY_TOKEN + ".";

    public static final String DEFAULT_MEMORY_FLUSH_SYSTEM_PROMPT = "Pre-compaction memory flush turn. " +
            "The session is near auto-compaction; capture durable memories to disk. " +
            "You may reply, but usually " + ReplyTokens.SILENT_REPLY_TOKEN + " is correct.";

    /** Resolved memory flush settings. */
    public record MemoryFlushSettings(
            boolean enabled,
            int softThresholdTokens,
            String prompt,
            String systemPrompt,
            int reserveTokensFloor) {
    }

    /** Normalize a non-negative integer from an arbitrary value. */
    public static Integer normalizeNonNegativeInt(Object value) {
        if (!(value instanceof Number n))
            return null;
        double d = n.doubleValue();
        if (!Double.isFinite(d))
            return null;
        int i = (int) Math.floor(d);
        return i >= 0 ? i : null;
    }

    /** Ensure the prompt text contains the silent reply hint. */
    public static String ensureNoReplyHint(String text) {
        if (text.contains(ReplyTokens.SILENT_REPLY_TOKEN))
            return text;
        return text + "\n\nIf no user-visible reply is needed, start with " + ReplyTokens.SILENT_REPLY_TOKEN + ".";
    }

    /**
     * Check whether a memory flush turn should be run.
     *
     * @param totalTokens                current token count in session
     * @param compactionCount            how many compactions have occurred
     * @param memoryFlushCompactionCount last compaction count when flush was run
     * @param contextWindowTokens        total context window size
     * @param reserveTokensFloor         tokens reserved for prompt overhead
     * @param softThresholdTokens        how many tokens before compaction to
     *                                   trigger flush
     */
    public static boolean shouldRunMemoryFlush(
            Integer totalTokens,
            Integer compactionCount,
            Integer memoryFlushCompactionCount,
            int contextWindowTokens,
            int reserveTokensFloor,
            int softThresholdTokens) {

        if (totalTokens == null || totalTokens <= 0)
            return false;
        int contextWindow = Math.max(1, contextWindowTokens);
        int reserve = Math.max(0, reserveTokensFloor);
        int soft = Math.max(0, softThresholdTokens);
        int threshold = Math.max(0, contextWindow - reserve - soft);
        if (threshold <= 0)
            return false;
        if (totalTokens < threshold)
            return false;

        int compCount = compactionCount != null ? compactionCount : 0;
        if (memoryFlushCompactionCount != null && memoryFlushCompactionCount == compCount)
            return false;

        return true;
    }
}
