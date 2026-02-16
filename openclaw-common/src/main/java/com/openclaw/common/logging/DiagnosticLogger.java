package com.openclaw.common.logging;

import java.util.Map;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Diagnostic logger for runtime observability: session states, webhook stats,
 * message queue events, lane operations, and periodic heartbeat.
 * Translates TS logging/diagnostic.ts.
 *
 */
public class DiagnosticLogger {

    private static final SubsystemLogger log = SubsystemLogger.create("diagnostic");

    // -----------------------------------------------------------------------
    // Session state tracking
    // -----------------------------------------------------------------------

    public enum SessionStateValue {
        IDLE, PROCESSING, QUEUED, STUCK;

        public static SessionStateValue fromString(String s) {
            if (s == null)
                return IDLE;
            return switch (s.toLowerCase()) {
                case "processing" -> PROCESSING;
                case "queued" -> QUEUED;
                case "stuck" -> STUCK;
                default -> IDLE;
            };
        }
    }

    public record SessionState(
            String sessionId,
            String sessionKey,
            long lastActivity,
            SessionStateValue state,
            int queueDepth) {
    }

    private static final ConcurrentHashMap<String, SessionState> sessionStates = new ConcurrentHashMap<>();

    // -----------------------------------------------------------------------
    // Webhook statistics
    // -----------------------------------------------------------------------

    private static final AtomicLong webhookReceived = new AtomicLong();
    private static final AtomicLong webhookProcessed = new AtomicLong();
    private static final AtomicLong webhookErrors = new AtomicLong();
    private static final AtomicLong webhookLastReceived = new AtomicLong();
    private static final AtomicLong lastActivityAt = new AtomicLong();

    // -----------------------------------------------------------------------
    // Heartbeat
    // -----------------------------------------------------------------------

    private static volatile ScheduledFuture<?> heartbeatFuture;
    private static final ScheduledExecutorService heartbeatExecutor = Executors.newSingleThreadScheduledExecutor(r -> {
        Thread t = new Thread(r, "diagnostic-heartbeat");
        t.setDaemon(true);
        return t;
    });

    // -----------------------------------------------------------------------
    // Activity tracking
    // -----------------------------------------------------------------------

    public static void markActivity() {
        lastActivityAt.set(System.currentTimeMillis());
    }

    // -----------------------------------------------------------------------
    // Session state
    // -----------------------------------------------------------------------

    private static String resolveSessionKey(String sessionKey, String sessionId) {
        return (sessionKey != null && !sessionKey.isEmpty()) ? sessionKey
                : (sessionId != null ? sessionId : "unknown");
    }

    public static SessionState getSessionState(String sessionKey, String sessionId) {
        String key = resolveSessionKey(sessionKey, sessionId);
        return sessionStates.computeIfAbsent(key,
                k -> new SessionState(sessionId, sessionKey, System.currentTimeMillis(),
                        SessionStateValue.IDLE, 0));
    }

    // -----------------------------------------------------------------------
    // Webhook events
    // -----------------------------------------------------------------------

    public static void logWebhookReceived(String channel, String updateType, String chatId) {
        webhookReceived.incrementAndGet();
        webhookLastReceived.set(System.currentTimeMillis());
        markActivity();
        log.debug("Webhook received", Map.of(
                "channel", channel,
                "updateType", str(updateType),
                "chatId", str(chatId),
                "totalReceived", webhookReceived.get()));
    }

    public static void logWebhookProcessed(String channel, String updateType,
            String chatId, long durationMs) {
        webhookProcessed.incrementAndGet();
        log.debug("Webhook processed", Map.of(
                "channel", channel,
                "updateType", str(updateType),
                "chatId", str(chatId),
                "durationMs", durationMs,
                "totalProcessed", webhookProcessed.get()));
    }

    public static void logWebhookError(String channel, String updateType,
            String chatId, String error) {
        webhookErrors.incrementAndGet();
        log.error("Webhook error", Map.of(
                "channel", channel,
                "updateType", str(updateType),
                "chatId", str(chatId),
                "error", str(error),
                "totalErrors", webhookErrors.get()));
    }

    // -----------------------------------------------------------------------
    // Message queue events
    // -----------------------------------------------------------------------

    public static void logMessageQueued(String sessionId, String sessionKey,
            String channel, String source) {
        String key = resolveSessionKey(sessionKey, sessionId);
        sessionStates.compute(key, (k, prev) -> {
            int depth = (prev != null) ? prev.queueDepth() + 1 : 1;
            return new SessionState(sessionId, sessionKey,
                    System.currentTimeMillis(), SessionStateValue.QUEUED, depth);
        });
        markActivity();
        log.debug("Message queued", Map.of(
                "sessionKey", str(sessionKey),
                "channel", str(channel),
                "source", str(source)));
    }

    public static void logMessageProcessed(String channel, String sessionId,
            String sessionKey, long durationMs,
            String outcome, String reason, String error) {
        String key = resolveSessionKey(sessionKey, sessionId);
        sessionStates.compute(key, (k, prev) -> {
            int depth = (prev != null) ? Math.max(0, prev.queueDepth() - 1) : 0;
            return new SessionState(sessionId, sessionKey,
                    System.currentTimeMillis(), SessionStateValue.IDLE, depth);
        });

        var meta = new java.util.HashMap<String, Object>();
        meta.put("channel", str(channel));
        meta.put("sessionKey", str(sessionKey));
        meta.put("durationMs", durationMs);
        meta.put("outcome", str(outcome));
        if (reason != null)
            meta.put("reason", reason);
        if (error != null)
            meta.put("error", error);

        if ("error".equals(outcome)) {
            log.error("Message processing failed", meta);
        } else {
            log.debug("Message processed", meta);
        }
    }

    // -----------------------------------------------------------------------
    // Session state change
    // -----------------------------------------------------------------------

    public static void logSessionStateChange(String sessionId, String sessionKey,
            SessionStateValue state, String reason) {
        String key = resolveSessionKey(sessionKey, sessionId);
        sessionStates.compute(key, (k, prev) -> {
            int depth = prev != null ? prev.queueDepth() : 0;
            return new SessionState(sessionId, sessionKey,
                    System.currentTimeMillis(), state, depth);
        });
        log.info("Session state changed", Map.of(
                "sessionKey", str(sessionKey),
                "state", state.name(),
                "reason", str(reason)));
    }

    public static void logSessionStuck(String sessionId, String sessionKey,
            SessionStateValue state, long ageMs) {
        log.warn("Session appears stuck", Map.of(
                "sessionKey", str(sessionKey),
                "state", state.name(),
                "ageMs", ageMs));
    }

    // -----------------------------------------------------------------------
    // Lane events
    // -----------------------------------------------------------------------

    public static void logLaneEnqueue(String lane, int queueSize) {
        log.debug("Lane enqueue", Map.of("lane", lane, "queueSize", queueSize));
    }

    public static void logLaneDequeue(String lane, long waitMs, int queueSize) {
        log.debug("Lane dequeue", Map.of(
                "lane", lane, "waitMs", waitMs, "queueSize", queueSize));
    }

    // -----------------------------------------------------------------------
    // Run tracking
    // -----------------------------------------------------------------------

    public static void logRunAttempt(String sessionId, String sessionKey,
            String runId, int attempt) {
        log.debug("Run attempt", Map.of(
                "sessionKey", str(sessionKey),
                "runId", str(runId),
                "attempt", attempt));
    }

    public static void logActiveRuns() {
        int active = (int) sessionStates.values().stream()
                .filter(s -> s.state() == SessionStateValue.PROCESSING)
                .count();
        log.debug("Active runs snapshot", Map.of("activeCount", active));
    }

    // -----------------------------------------------------------------------
    // Heartbeat
    // -----------------------------------------------------------------------

    public static void startDiagnosticHeartbeat() {
        if (heartbeatFuture != null) {
            return;
        }
        heartbeatFuture = heartbeatExecutor.scheduleAtFixedRate(() -> {
            try {
                int totalSessions = sessionStates.size();
                long active = sessionStates.values().stream()
                        .filter(s -> s.state() == SessionStateValue.PROCESSING)
                        .count();
                long queued = sessionStates.values().stream()
                        .filter(s -> s.state() == SessionStateValue.QUEUED)
                        .count();
                long now = System.currentTimeMillis();

                // Detect stuck sessions (> 5 minutes)
                sessionStates.forEach((key, state) -> {
                    if (state.state() == SessionStateValue.PROCESSING) {
                        long age = now - state.lastActivity();
                        if (age > 5 * 60 * 1000) {
                            logSessionStuck(state.sessionId(), state.sessionKey(),
                                    state.state(), age);
                        }
                    }
                });

                log.info("Heartbeat", Map.of(
                        "totalSessions", totalSessions,
                        "active", active,
                        "queued", queued,
                        "webhookReceived", webhookReceived.get(),
                        "webhookProcessed", webhookProcessed.get(),
                        "webhookErrors", webhookErrors.get(),
                        "lastActivityAgeMs", lastActivityAt.get() > 0
                                ? now - lastActivityAt.get()
                                : -1));
            } catch (Exception e) {
                log.error("Heartbeat error", e);
            }
        }, 30, 30, TimeUnit.SECONDS);
    }

    public static void stopDiagnosticHeartbeat() {
        if (heartbeatFuture != null) {
            heartbeatFuture.cancel(false);
            heartbeatFuture = null;
        }
    }

    // -----------------------------------------------------------------------
    // Reset (for testing)
    // -----------------------------------------------------------------------

    public static void reset() {
        sessionStates.clear();
        webhookReceived.set(0);
        webhookProcessed.set(0);
        webhookErrors.set(0);
        webhookLastReceived.set(0);
        lastActivityAt.set(0);
        stopDiagnosticHeartbeat();
    }

    private static String str(Object value) {
        return value != null ? value.toString() : "";
    }
}
