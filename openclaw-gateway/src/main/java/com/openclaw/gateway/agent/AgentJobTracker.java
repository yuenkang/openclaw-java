package com.openclaw.gateway.agent;

import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.concurrent.*;

/**
 * Tracks agent run lifecycle events and provides a wait-for-completion API.
 * Corresponds to TypeScript's server-methods/agent-job.ts.
 *
 * <p>
 * Cached run snapshots are retained for 10 minutes.
 * </p>
 */
@Slf4j
public class AgentJobTracker {

    private static final long CACHE_TTL_MS = 10 * 60_000; // 10 minutes

    private final Map<String, AgentRunSnapshot> cache = new ConcurrentHashMap<>();
    private final Map<String, Long> runStarts = new ConcurrentHashMap<>();

    /**
     * Records a start event for the given run.
     */
    public void recordStart(String runId, long startedAtMs) {
        runStarts.put(runId, startedAtMs);
        pruneCache(System.currentTimeMillis());
    }

    /**
     * Records a completion or error event for the given run.
     */
    public void recordEnd(String runId, String status, String error) {
        long now = System.currentTimeMillis();
        Long startedAt = runStarts.remove(runId);
        var snapshot = new AgentRunSnapshot(
                runId,
                "error".equals(status) ? "error" : "ok",
                startedAt,
                now,
                error,
                now);
        cache.put(runId, snapshot);
        pruneCache(now);
    }

    /**
     * Returns a cached snapshot, or null if not available.
     */
    public AgentRunSnapshot getSnapshot(String runId) {
        pruneCache(System.currentTimeMillis());
        return cache.get(runId);
    }

    /**
     * Waits for an agent run to complete, up to the specified timeout.
     *
     * @param runId     the run ID to wait for
     * @param timeoutMs maximum wait time in milliseconds
     * @return the snapshot, or null if timed out
     */
    public CompletableFuture<AgentRunSnapshot> waitForJob(String runId, long timeoutMs) {
        var cached = getSnapshot(runId);
        if (cached != null) {
            return CompletableFuture.completedFuture(cached);
        }
        if (timeoutMs <= 0) {
            return CompletableFuture.completedFuture(null);
        }

        // Poll with backoff (simple approach — event-driven approach would be better)
        CompletableFuture<AgentRunSnapshot> future = new CompletableFuture<>();
        ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "agent-job-wait-" + runId);
            t.setDaemon(true);
            return t;
        });

        long deadline = System.currentTimeMillis() + timeoutMs;
        executor.scheduleAtFixedRate(() -> {
            var snap = getSnapshot(runId);
            if (snap != null) {
                future.complete(snap);
                executor.shutdown();
                return;
            }
            if (System.currentTimeMillis() >= deadline) {
                future.complete(null);
                executor.shutdown();
            }
        }, 200, 500, TimeUnit.MILLISECONDS);

        // Safety timeout
        executor.schedule(() -> {
            if (!future.isDone()) {
                future.complete(null);
                executor.shutdown();
            }
        }, timeoutMs + 100, TimeUnit.MILLISECONDS);

        return future;
    }

    private void pruneCache(long now) {
        cache.entrySet().removeIf(e -> now - e.getValue().ts() > CACHE_TTL_MS);
    }

    /**
     * Immutable snapshot of an agent run result.
     */
    public record AgentRunSnapshot(
            String runId,
            String status,
            Long startedAtMs,
            Long endedAtMs,
            String error,
            long ts) {
    }
}
