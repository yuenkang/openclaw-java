package com.openclaw.gateway.chat;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Mutable state for an active chat session: run registry, text buffers, timing,
 * and aborted-run tracking.
 * <p>
 * Corresponds to TypeScript's {@code createChatRunState()} in server-chat.ts.
 */
public class ChatRunState {

    private final ChatRunRegistry registry = new ChatRunRegistry();

    /** Accumulated assistant text per clientRunId. */
    private final Map<String, String> buffers = new ConcurrentHashMap<>();

    /** Timestamp (epoch ms) of the last delta broadcast per clientRunId. */
    private final Map<String, Long> deltaSentAt = new ConcurrentHashMap<>();

    /** Aborted runs: clientRunId → abort timestamp (epoch ms). */
    private final Map<String, Long> abortedRuns = new ConcurrentHashMap<>();

    public ChatRunRegistry getRegistry() {
        return registry;
    }

    public Map<String, String> getBuffers() {
        return buffers;
    }

    public Map<String, Long> getDeltaSentAt() {
        return deltaSentAt;
    }

    public Map<String, Long> getAbortedRuns() {
        return abortedRuns;
    }

    /**
     * Reset all state.
     */
    public void clear() {
        registry.clear();
        buffers.clear();
        deltaSentAt.clear();
        abortedRuns.clear();
    }
}
