package com.openclaw.gateway.chat;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Chat abort controller — manages abort signals for active chat runs.
 * Corresponds to TypeScript's chat-abort.ts.
 */
public class ChatAbortController {

    // =========================================================================
    // Types
    // =========================================================================

    /**
     * Tracks an active chat run's abort controller state.
     * Uses AtomicBoolean for thread-safe mutable abort flag
     * (record components cannot be volatile/mutable).
     */
    public static class AbortEntry {
        private final String runId;
        private final String sessionId;
        private final String sessionKey;
        private final long startedAtMs;
        private final long expiresAtMs;
        private final AtomicBoolean aborted;

        public AbortEntry(String runId, String sessionId, String sessionKey, long startedAtMs, long expiresAtMs) {
            this(runId, sessionId, sessionKey, startedAtMs, expiresAtMs, false);
        }

        public AbortEntry(String runId, String sessionId, String sessionKey,
                long startedAtMs, long expiresAtMs, boolean aborted) {
            this.runId = runId;
            this.sessionId = sessionId;
            this.sessionKey = sessionKey;
            this.startedAtMs = startedAtMs;
            this.expiresAtMs = expiresAtMs;
            this.aborted = new AtomicBoolean(aborted);
        }

        public String runId() {
            return runId;
        }

        public String sessionId() {
            return sessionId;
        }

        public String sessionKey() {
            return sessionKey;
        }

        public long startedAtMs() {
            return startedAtMs;
        }

        public long expiresAtMs() {
            return expiresAtMs;
        }

        public boolean aborted() {
            return aborted.get();
        }

        public void setAborted(boolean value) {
            aborted.set(value);
        }
    }

    public record AbortResult(boolean aborted) {
    }

    public record AbortMultiResult(boolean aborted, List<String> runIds) {
    }

    // =========================================================================
    // Stop command detection
    // =========================================================================

    /**
     * Check if the text is a chat stop command ("/stop").
     */
    public static boolean isChatStopCommandText(String text) {
        if (text == null)
            return false;
        String trimmed = text.trim();
        if (trimmed.isEmpty())
            return false;
        return trimmed.equalsIgnoreCase("/stop");
    }

    /**
     * Calculate the expiration timestamp for a chat run.
     */
    public static long resolveChatRunExpiresAtMs(long now, long timeoutMs,
            long graceMs, long minMs, long maxMs) {
        long boundedTimeout = Math.max(0, timeoutMs);
        long target = now + boundedTimeout + graceMs;
        long min = now + minMs;
        long max = now + maxMs;
        return Math.min(max, Math.max(min, target));
    }

    /**
     * Calculate with default grace/min/max values.
     */
    public static long resolveChatRunExpiresAtMs(long now, long timeoutMs) {
        return resolveChatRunExpiresAtMs(now, timeoutMs,
                60_000L, 2 * 60_000L, 24 * 60 * 60_000L);
    }

    // =========================================================================
    // Instance state & operations
    // =========================================================================

    private final Map<String, AbortEntry> controllers = new ConcurrentHashMap<>();
    private final Map<String, Long> abortedRuns = new ConcurrentHashMap<>();

    public void register(AbortEntry entry) {
        controllers.put(entry.runId(), entry);
    }

    /**
     * Abort a specific chat run by runId.
     */
    public AbortResult abortChatRunById(String runId, String sessionKey) {
        AbortEntry active = controllers.get(runId);
        if (active == null) {
            return new AbortResult(false);
        }
        if (!active.sessionKey().equals(sessionKey)) {
            return new AbortResult(false);
        }
        abortedRuns.put(runId, System.currentTimeMillis());
        controllers.remove(runId);
        return new AbortResult(true);
    }

    /**
     * Abort all chat runs for a given session key.
     */
    public AbortMultiResult abortChatRunsForSessionKey(String sessionKey) {
        List<String> abortedIds = new ArrayList<>();
        for (var entry : controllers.entrySet()) {
            if (entry.getValue().sessionKey().equals(sessionKey)) {
                AbortResult result = abortChatRunById(entry.getKey(), sessionKey);
                if (result.aborted()) {
                    abortedIds.add(entry.getKey());
                }
            }
        }
        return new AbortMultiResult(!abortedIds.isEmpty(), abortedIds);
    }

    /**
     * Check if a run was previously aborted.
     */
    public boolean wasAborted(String runId) {
        return abortedRuns.containsKey(runId);
    }

    /**
     * Get an active entry.
     */
    public AbortEntry get(String runId) {
        return controllers.get(runId);
    }

    /**
     * Remove an entry.
     */
    public AbortEntry remove(String runId) {
        return controllers.remove(runId);
    }
}
