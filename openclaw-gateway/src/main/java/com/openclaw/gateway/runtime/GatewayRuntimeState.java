package com.openclaw.gateway.runtime;

import com.openclaw.gateway.chat.ChatAbortController;
import com.openclaw.gateway.chat.ChatRunRegistry;
import com.openclaw.gateway.chat.ToolEventRecipientRegistry;
import com.openclaw.gateway.websocket.GatewayConnection;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Gateway runtime state — holds live runtime objects such as connected clients,
 * chat runs, abort controllers, and dedupe cache.
 * Corresponds to TS {@code server-runtime-state.ts} + {@code server-shared.ts}.
 */
@Slf4j
@Getter
public class GatewayRuntimeState {

    /** Connected WebSocket clients. */
    private final Set<GatewayConnection> clients = new CopyOnWriteArraySet<>();

    /** Agent run sequence per session. */
    private final Map<String, AtomicLong> agentRunSeq = new ConcurrentHashMap<>();

    /** Request dedupe cache (idempotency). */
    private final Map<String, DedupeEntry> dedupe = new ConcurrentHashMap<>();

    /** Chat run registry. */
    private final ChatRunRegistry chatRunRegistry = new ChatRunRegistry();

    /** Chat run streaming buffers. */
    private final Map<String, StringBuilder> chatRunBuffers = new ConcurrentHashMap<>();

    /** Last delta sent timestamp per chat run. */
    private final Map<String, Long> chatDeltaSentAt = new ConcurrentHashMap<>();

    /** Abort controllers for active chat sessions. */
    private final ChatAbortController chatAbortController = new ChatAbortController();

    /** Tool event recipients for verbose mode. */
    private final ToolEventRecipientRegistry toolEventRecipients = new ToolEventRecipientRegistry();

    /** Global sequence counter for run IDs. */
    private final AtomicLong globalSeq = new AtomicLong(0);

    /**
     * Next agent run sequence number for a session.
     */
    public long nextAgentRunSeq(String sessionId) {
        return agentRunSeq
                .computeIfAbsent(sessionId, k -> new AtomicLong(0))
                .incrementAndGet();
    }

    /**
     * Add a dedupe entry.
     */
    public void addDedupeEntry(String requestId, DedupeEntry entry) {
        dedupe.put(requestId, entry);
    }

    /**
     * Clean up expired dedupe entries.
     */
    public void cleanupDedupe(long maxAgeMs) {
        long cutoff = System.currentTimeMillis() - maxAgeMs;
        dedupe.entrySet().removeIf(e -> e.getValue().ts < cutoff);
    }

    /**
     * Clear all state (used during shutdown).
     */
    public void clear() {
        chatRunRegistry.clear();
        chatRunBuffers.clear();
        chatDeltaSentAt.clear();
        toolEventRecipients.clear();
        agentRunSeq.clear();
        dedupe.clear();
    }

    // =========================================================================
    // DedupeEntry — TS server-shared.ts
    // =========================================================================

    /**
     * Dedupe entry for request idempotency.
     */
    @Getter
    public static class DedupeEntry {
        private final long ts;
        private final boolean ok;
        private final Object payload;
        private final Object error;

        public DedupeEntry(boolean ok, Object payload, Object error) {
            this.ts = System.currentTimeMillis();
            this.ok = ok;
            this.payload = payload;
            this.error = error;
        }
    }
}
