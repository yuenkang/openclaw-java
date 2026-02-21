package com.openclaw.gateway.runtime;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;

/**
 * Thread-safe runtime metrics collector for the gateway.
 * Tracks request counts, active connections, agent runs, and error rates.
 *
 * <p>
 * All counters are non-blocking and suitable for high-concurrency use.
 * </p>
 */
public class RuntimeMetrics {

    private static final RuntimeMetrics INSTANCE = new RuntimeMetrics();

    // ── WebSocket connections ─────────────────────────────────────

    private final AtomicLong activeWebSocketConnections = new AtomicLong(0);
    private final LongAdder totalWebSocketConnections = new LongAdder();
    private final LongAdder totalWebSocketDisconnections = new LongAdder();

    // ── HTTP requests ─────────────────────────────────────────────

    private final LongAdder totalHttpRequests = new LongAdder();
    private final ConcurrentHashMap<String, LongAdder> httpRequestsByMethod = new ConcurrentHashMap<>();

    // ── Agent runs ────────────────────────────────────────────────

    private final AtomicLong activeAgentRuns = new AtomicLong(0);
    private final LongAdder totalAgentRuns = new LongAdder();
    private final LongAdder agentRunErrors = new LongAdder();
    private final LongAdder totalToolCalls = new LongAdder();

    // ── Latency ───────────────────────────────────────────────────

    private final AtomicLong lastAgentRunDurationMs = new AtomicLong(0);
    private final LongAdder totalAgentRunDurationMs = new LongAdder();

    // ── Messages ──────────────────────────────────────────────────

    private final LongAdder inboundMessages = new LongAdder();
    private final LongAdder outboundMessages = new LongAdder();

    // ── Instance access ───────────────────────────────────────────

    public static RuntimeMetrics getInstance() {
        return INSTANCE;
    }

    // ── WebSocket ─────────────────────────────────────────────────

    public void onWebSocketConnect() {
        activeWebSocketConnections.incrementAndGet();
        totalWebSocketConnections.increment();
    }

    public void onWebSocketDisconnect() {
        activeWebSocketConnections.decrementAndGet();
        totalWebSocketDisconnections.increment();
    }

    public long getActiveWebSocketConnections() {
        return activeWebSocketConnections.get();
    }

    public long getTotalWebSocketConnections() {
        return totalWebSocketConnections.sum();
    }

    // ── HTTP ──────────────────────────────────────────────────────

    public void onHttpRequest(String method) {
        totalHttpRequests.increment();
        httpRequestsByMethod
                .computeIfAbsent(method, k -> new LongAdder())
                .increment();
    }

    public long getTotalHttpRequests() {
        return totalHttpRequests.sum();
    }

    public long getHttpRequestsByMethod(String method) {
        LongAdder adder = httpRequestsByMethod.get(method);
        return adder != null ? adder.sum() : 0;
    }

    // ── Agent runs ────────────────────────────────────────────────

    public void onAgentRunStart() {
        activeAgentRuns.incrementAndGet();
        totalAgentRuns.increment();
    }

    public void onAgentRunEnd(long durationMs, boolean success) {
        activeAgentRuns.decrementAndGet();
        lastAgentRunDurationMs.set(durationMs);
        totalAgentRunDurationMs.add(durationMs);
        if (!success) {
            agentRunErrors.increment();
        }
    }

    public void onToolCall() {
        totalToolCalls.increment();
    }

    public long getActiveAgentRuns() {
        return activeAgentRuns.get();
    }

    public long getTotalAgentRuns() {
        return totalAgentRuns.sum();
    }

    public long getAgentRunErrors() {
        return agentRunErrors.sum();
    }

    public long getTotalToolCalls() {
        return totalToolCalls.sum();
    }

    public long getLastAgentRunDurationMs() {
        return lastAgentRunDurationMs.get();
    }

    public long getAverageAgentRunDurationMs() {
        long total = totalAgentRuns.sum();
        return total > 0 ? totalAgentRunDurationMs.sum() / total : 0;
    }

    // ── Messages ──────────────────────────────────────────────────

    public void onInboundMessage() {
        inboundMessages.increment();
    }

    public void onOutboundMessage() {
        outboundMessages.increment();
    }

    public long getInboundMessages() {
        return inboundMessages.sum();
    }

    public long getOutboundMessages() {
        return outboundMessages.sum();
    }

    // ── Snapshot ───────────────────────────────────────────────────

    /**
     * Create a point-in-time snapshot of all metrics.
     */
    public Snapshot snapshot() {
        return new Snapshot(
                activeWebSocketConnections.get(),
                totalWebSocketConnections.sum(),
                totalHttpRequests.sum(),
                activeAgentRuns.get(),
                totalAgentRuns.sum(),
                agentRunErrors.sum(),
                totalToolCalls.sum(),
                getAverageAgentRunDurationMs(),
                lastAgentRunDurationMs.get(),
                inboundMessages.sum(),
                outboundMessages.sum());
    }

    /**
     * Immutable snapshot of runtime metrics at a point in time.
     */
    public record Snapshot(
            long activeWebSocketConnections,
            long totalWebSocketConnections,
            long totalHttpRequests,
            long activeAgentRuns,
            long totalAgentRuns,
            long agentRunErrors,
            long totalToolCalls,
            long averageAgentRunDurationMs,
            long lastAgentRunDurationMs,
            long inboundMessages,
            long outboundMessages) {
    }

    // ── Reset (for testing) ───────────────────────────────────────

    /**
     * Reset all counters. Use only in tests.
     */
    public void reset() {
        activeWebSocketConnections.set(0);
        totalWebSocketConnections.reset();
        totalWebSocketDisconnections.reset();
        totalHttpRequests.reset();
        httpRequestsByMethod.clear();
        activeAgentRuns.set(0);
        totalAgentRuns.reset();
        agentRunErrors.reset();
        totalToolCalls.reset();
        lastAgentRunDurationMs.set(0);
        totalAgentRunDurationMs.reset();
        inboundMessages.reset();
        outboundMessages.reset();
    }
}
