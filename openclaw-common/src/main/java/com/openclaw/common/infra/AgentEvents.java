package com.openclaw.common.infra;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

/**
 * Agent event bus — emits lifecycle, tool, assistant, and error events
 * scoped per run with strictly monotonic sequence numbers.
 * Corresponds to TypeScript's infra/agent-events.ts.
 */
public final class AgentEvents {

    private AgentEvents() {
    }

    // ── Types ───────────────────────────────────────────────────────────

    /** Well-known event stream names. */
    public static final String STREAM_LIFECYCLE = "lifecycle";
    public static final String STREAM_TOOL = "tool";
    public static final String STREAM_ASSISTANT = "assistant";
    public static final String STREAM_ERROR = "error";

    /**
     * Event payload emitted by the agent engine.
     */
    public record EventPayload(
            String runId,
            int seq,
            String stream,
            long ts,
            Map<String, Object> data,
            String sessionKey) {
    }

    /**
     * Context associated with an agent run.
     */
    public static class RunContext {
        private volatile String sessionKey;
        private volatile String verboseLevel;
        private volatile Boolean isHeartbeat;

        public RunContext(String sessionKey, String verboseLevel, Boolean isHeartbeat) {
            this.sessionKey = sessionKey;
            this.verboseLevel = verboseLevel;
            this.isHeartbeat = isHeartbeat;
        }

        public String getSessionKey() {
            return sessionKey;
        }

        public void setSessionKey(String sessionKey) {
            this.sessionKey = sessionKey;
        }

        public String getVerboseLevel() {
            return verboseLevel;
        }

        public void setVerboseLevel(String verboseLevel) {
            this.verboseLevel = verboseLevel;
        }

        public Boolean getIsHeartbeat() {
            return isHeartbeat;
        }

        public void setIsHeartbeat(Boolean isHeartbeat) {
            this.isHeartbeat = isHeartbeat;
        }
    }

    // ── State ───────────────────────────────────────────────────────────

    private static final Map<String, AtomicInteger> seqByRun = new ConcurrentHashMap<>();
    private static final Map<String, RunContext> runContextById = new ConcurrentHashMap<>();
    private static final Set<Consumer<EventPayload>> listeners = new CopyOnWriteArraySet<>();

    // ── Run context ─────────────────────────────────────────────────────

    public static void registerRunContext(String runId, RunContext context) {
        if (runId == null || runId.isEmpty()) {
            return;
        }
        runContextById.merge(runId, context, (existing, incoming) -> {
            if (incoming.sessionKey != null && !incoming.sessionKey.equals(existing.sessionKey)) {
                existing.sessionKey = incoming.sessionKey;
            }
            if (incoming.verboseLevel != null && !incoming.verboseLevel.equals(existing.verboseLevel)) {
                existing.verboseLevel = incoming.verboseLevel;
            }
            if (incoming.isHeartbeat != null && !incoming.isHeartbeat.equals(existing.isHeartbeat)) {
                existing.isHeartbeat = incoming.isHeartbeat;
            }
            return existing;
        });
    }

    public static RunContext getRunContext(String runId) {
        return runContextById.get(runId);
    }

    public static void clearRunContext(String runId) {
        runContextById.remove(runId);
        seqByRun.remove(runId);
    }

    // ── Emit ────────────────────────────────────────────────────────────

    /**
     * Emit an agent event, enriching it with a sequence number and timestamp.
     */
    public static void emit(String runId, String stream, Map<String, Object> data, String sessionKey) {
        AtomicInteger counter = seqByRun.computeIfAbsent(runId, k -> new AtomicInteger(0));
        int nextSeq = counter.incrementAndGet();

        RunContext ctx = runContextById.get(runId);
        String effectiveSessionKey = (sessionKey != null && !sessionKey.isBlank())
                ? sessionKey
                : (ctx != null ? ctx.sessionKey : null);

        EventPayload payload = new EventPayload(runId, nextSeq, stream, System.currentTimeMillis(),
                data != null ? data : Map.of(), effectiveSessionKey);

        for (Consumer<EventPayload> listener : listeners) {
            try {
                listener.accept(payload);
            } catch (Exception ignored) {
                // ignore listener errors
            }
        }
    }

    /**
     * Register a listener for agent events.
     *
     * @return a Runnable that removes the listener when called
     */
    public static Runnable onEvent(Consumer<EventPayload> listener) {
        listeners.add(listener);
        return () -> listeners.remove(listener);
    }

    // ── Test support ────────────────────────────────────────────────────

    public static void resetForTest() {
        seqByRun.clear();
        runContextById.clear();
        listeners.clear();
    }
}
