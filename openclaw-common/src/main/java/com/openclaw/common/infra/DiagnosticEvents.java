package com.openclaw.common.infra;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

/**
 * Diagnostic event bus for structured telemetry (usage, webhooks, messages,
 * sessions, queue lanes, heartbeat).
 * Corresponds to TypeScript's infra/diagnostic-events.ts.
 */
public final class DiagnosticEvents {

    private DiagnosticEvents() {
    }

    // ── Event types ─────────────────────────────────────────────────────

    /** Well-known diagnostic event types. */
    public static final String TYPE_MODEL_USAGE = "model.usage";
    public static final String TYPE_WEBHOOK_RECEIVED = "webhook.received";
    public static final String TYPE_WEBHOOK_PROCESSED = "webhook.processed";
    public static final String TYPE_WEBHOOK_ERROR = "webhook.error";
    public static final String TYPE_MESSAGE_QUEUED = "message.queued";
    public static final String TYPE_MESSAGE_PROCESSED = "message.processed";
    public static final String TYPE_SESSION_STATE = "session.state";
    public static final String TYPE_SESSION_STUCK = "session.stuck";
    public static final String TYPE_QUEUE_LANE_ENQUEUE = "queue.lane.enqueue";
    public static final String TYPE_QUEUE_LANE_DEQUEUE = "queue.lane.dequeue";
    public static final String TYPE_RUN_ATTEMPT = "run.attempt";
    public static final String TYPE_DIAGNOSTIC_HEARTBEAT = "diagnostic.heartbeat";

    /** Session processing state. */
    public enum SessionState {
        IDLE, PROCESSING, WAITING
    }

    /** Message processing outcome. */
    public enum MessageOutcome {
        COMPLETED, SKIPPED, ERROR
    }

    // ── Payload ─────────────────────────────────────────────────────────

    /**
     * Generic diagnostic event payload.
     * Use the {@code type} field to discriminate, and the {@code data} map for
     * type-specific fields.
     */
    public record EventPayload(
            String type,
            int seq,
            long ts,
            Map<String, Object> data) {
    }

    // ── State ───────────────────────────────────────────────────────────

    private static final AtomicInteger seq = new AtomicInteger(0);
    private static final Set<Consumer<EventPayload>> listeners = new CopyOnWriteArraySet<>();

    // ── Enablement check ────────────────────────────────────────────────

    /**
     * Check whether diagnostics are enabled. In Java this is typically driven
     * by Spring configuration, so this delegates to a simple flag check.
     */
    private static volatile boolean enabled = false;

    public static void setEnabled(boolean flag) {
        enabled = flag;
    }

    public static boolean isEnabled() {
        return enabled;
    }

    // ── Emit ────────────────────────────────────────────────────────────

    /**
     * Emit a diagnostic event.
     *
     * @param type the event type (use constants defined above)
     * @param data type-specific data fields
     */
    public static void emit(String type, Map<String, Object> data) {
        EventPayload payload = new EventPayload(
                type,
                seq.incrementAndGet(),
                System.currentTimeMillis(),
                data != null ? data : Map.of());

        for (Consumer<EventPayload> listener : listeners) {
            try {
                listener.accept(payload);
            } catch (Exception ignored) {
                // ignore listener failures
            }
        }
    }

    /**
     * Register a listener for diagnostic events.
     *
     * @return a Runnable that removes the listener when called
     */
    public static Runnable onEvent(Consumer<EventPayload> listener) {
        listeners.add(listener);
        return () -> listeners.remove(listener);
    }

    // ── Test support ────────────────────────────────────────────────────

    public static void resetForTest() {
        seq.set(0);
        listeners.clear();
        enabled = false;
    }
}
