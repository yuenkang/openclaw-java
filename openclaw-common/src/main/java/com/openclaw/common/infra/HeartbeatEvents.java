package com.openclaw.common.infra;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.function.Consumer;

/**
 * Heartbeat event bus — tracks the status of periodic heartbeat messages
 * (sent, ok, skipped, failed) and notifies listeners.
 * Corresponds to TypeScript's infra/heartbeat-events.ts.
 */
public final class HeartbeatEvents {

    private HeartbeatEvents() {
    }

    // ── Types ───────────────────────────────────────────────────────────

    /** Indicator type for UI display. */
    public enum IndicatorType {
        OK, ALERT, ERROR
    }

    /** Heartbeat status. */
    public enum Status {
        SENT, OK_EMPTY, OK_TOKEN, SKIPPED, FAILED;

        /**
         * Map status to an indicator type (or {@code null} for SKIPPED).
         */
        public IndicatorType toIndicatorType() {
            return switch (this) {
                case OK_EMPTY, OK_TOKEN -> IndicatorType.OK;
                case SENT -> IndicatorType.ALERT;
                case FAILED -> IndicatorType.ERROR;
                case SKIPPED -> null;
            };
        }
    }

    /**
     * Heartbeat event payload.
     */
    public record EventPayload(
            long ts,
            Status status,
            String to,
            String accountId,
            String preview,
            Long durationMs,
            Boolean hasMedia,
            String reason,
            String channel,
            Boolean silent,
            IndicatorType indicatorType) {
    }

    // ── Builder ─────────────────────────────────────────────────────────

    public static class Builder {
        private Status status;
        private String to;
        private String accountId;
        private String preview;
        private Long durationMs;
        private Boolean hasMedia;
        private String reason;
        private String channel;
        private Boolean silent;

        public Builder status(Status status) {
            this.status = status;
            return this;
        }

        public Builder to(String to) {
            this.to = to;
            return this;
        }

        public Builder accountId(String accountId) {
            this.accountId = accountId;
            return this;
        }

        public Builder preview(String preview) {
            this.preview = preview;
            return this;
        }

        public Builder durationMs(long durationMs) {
            this.durationMs = durationMs;
            return this;
        }

        public Builder hasMedia(boolean hasMedia) {
            this.hasMedia = hasMedia;
            return this;
        }

        public Builder reason(String reason) {
            this.reason = reason;
            return this;
        }

        public Builder channel(String channel) {
            this.channel = channel;
            return this;
        }

        public Builder silent(boolean silent) {
            this.silent = silent;
            return this;
        }

        public EventPayload build() {
            return new EventPayload(
                    System.currentTimeMillis(), status, to, accountId, preview,
                    durationMs, hasMedia, reason, channel, silent,
                    status != null ? status.toIndicatorType() : null);
        }
    }

    // ── State ───────────────────────────────────────────────────────────

    private static volatile EventPayload lastHeartbeat;
    private static final Set<Consumer<EventPayload>> listeners = new CopyOnWriteArraySet<>();

    // ── Emit ────────────────────────────────────────────────────────────

    /**
     * Emit a heartbeat event.
     */
    public static void emit(EventPayload event) {
        lastHeartbeat = event;
        for (Consumer<EventPayload> listener : listeners) {
            try {
                listener.accept(event);
            } catch (Exception ignored) {
                // ignore
            }
        }
    }

    /**
     * Emit a heartbeat event using a builder.
     */
    public static void emit(Consumer<Builder> configure) {
        Builder builder = new Builder();
        configure.accept(builder);
        emit(builder.build());
    }

    /**
     * Register a listener.
     *
     * @return a Runnable to unregister
     */
    public static Runnable onEvent(Consumer<EventPayload> listener) {
        listeners.add(listener);
        return () -> listeners.remove(listener);
    }

    /**
     * Get the last heartbeat event, or {@code null}.
     */
    public static EventPayload getLastEvent() {
        return lastHeartbeat;
    }

    public static void resetForTest() {
        lastHeartbeat = null;
        listeners.clear();
    }
}
