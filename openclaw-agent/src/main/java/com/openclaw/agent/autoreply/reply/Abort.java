package com.openclaw.agent.autoreply.reply;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

/**
 * Abort trigger detection, abort memory management, and fast-abort handling.
 * Mirrors {@code auto-reply/reply/abort.ts}.
 */
public final class Abort {

    private Abort() {
    }

    /* ── constants ──────────────────────────────────────────── */

    private static final Pattern ABORT_TRIGGER_RE = Pattern.compile(
            "^\\s*(/(?:stop|abort|cancel|kill|quit|halt|reset|clear|end|done|exit|bye)(?:\\s.*)?|"
                    + "stop|abort|cancel|quit)\\s*$",
            Pattern.CASE_INSENSITIVE);

    private static final Pattern STOP_SUBAGENT_RE = Pattern.compile(
            "^\\s*/stop\\s*$", Pattern.CASE_INSENSITIVE);

    /* ── abort memory ──────────────────────────────────────── */

    /** Global in-memory abort state keyed by session key. */
    private static final Map<String, Boolean> ABORT_MEMORY = new ConcurrentHashMap<>();

    /**
     * Set whether the previous run was aborted for a given key.
     */
    public static void setAbortMemory(String key, boolean aborted) {
        if (key == null || key.isBlank())
            return;
        if (aborted) {
            ABORT_MEMORY.put(key.trim(), true);
        } else {
            ABORT_MEMORY.remove(key.trim());
        }
    }

    /**
     * Check whether the abort flag is set for a key.
     */
    public static boolean getAbortMemory(String key) {
        if (key == null || key.isBlank())
            return false;
        return Boolean.TRUE.equals(ABORT_MEMORY.get(key.trim()));
    }

    /**
     * Clear the abort flag for a key.
     */
    public static void clearAbortMemory(String key) {
        setAbortMemory(key, false);
    }

    /* ── trigger detection ─────────────────────────────────── */

    /**
     * Check whether a message text is an abort trigger.
     */
    public static boolean isAbortTrigger(String text) {
        if (text == null || text.isBlank())
            return false;
        return ABORT_TRIGGER_RE.matcher(text).matches();
    }

    /**
     * Check whether a message is a stop-subagent command.
     */
    public static boolean isStopSubagentTrigger(String text) {
        if (text == null || text.isBlank())
            return false;
        return STOP_SUBAGENT_RE.matcher(text).matches();
    }

    /**
     * Extract the slash command from an abort trigger, if any.
     *
     * @return the command (e.g. "/stop") or null
     */
    public static String extractAbortCommand(String text) {
        if (text == null)
            return null;
        String trimmed = text.trim();
        if (trimmed.startsWith("/")) {
            int spaceIdx = trimmed.indexOf(' ');
            return spaceIdx > 0 ? trimmed.substring(0, spaceIdx).toLowerCase() : trimmed.toLowerCase();
        }
        return null;
    }

    /* ── fast abort ────────────────────────────────────────── */

    /** Result from a fast-abort attempt. */
    public record FastAbortResult(boolean aborted, String replyText) {
    }

    /**
     * Attempt a fast abort from the inbound message.
     * If the message matches an abort trigger, set the abort memory and
     * return a reply.
     *
     * @param text     inbound message text
     * @param abortKey session key for abort memory
     * @return result with aborted=true if handled, or aborted=false
     */
    public static FastAbortResult tryFastAbortFromMessage(String text, String abortKey) {
        if (!isAbortTrigger(text)) {
            return new FastAbortResult(false, null);
        }
        setAbortMemory(abortKey, true);
        String cmd = extractAbortCommand(text);
        String reply = cmd != null
                ? String.format("Acknowledged `%s`. The previous run has been flagged as aborted.", cmd)
                : "Acknowledged. The previous run has been flagged as aborted.";
        return new FastAbortResult(true, reply);
    }
}
