package com.openclaw.agent.runtime;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import lombok.extern.slf4j.Slf4j;

/**
 * Registry of active agent runs with queue/abort/wait capabilities.
 * Corresponds to TypeScript's pi-embedded-runner/runs.ts.
 *
 * <p>
 * Thread-safe; designed for concurrent access from multiple sessions.
 * </p>
 */
@Slf4j
public class ActiveRunRegistry {

    /**
     * Handle for an active agent run, allowing external interaction.
     */
    public interface RunHandle {
        /** Queue a follow-up message into the active run. */
        CompletableFuture<Void> queueMessage(String text);

        /** Whether the run is currently streaming LLM output. */
        boolean isStreaming();

        /** Whether the run is currently compacting history. */
        boolean isCompacting();

        /** Abort the active run. */
        void abort();
    }

    private final Map<String, RunHandle> activeRuns = new ConcurrentHashMap<>();
    private final Map<String, Set<CompletableFuture<Boolean>>> waiters = new ConcurrentHashMap<>();

    // ── Registration ──────────────────────────────────────────────────

    /**
     * Register an active run for the given session.
     * If a run is already active for this session, it is replaced.
     */
    public void setActiveRun(String sessionId, RunHandle handle) {
        boolean wasActive = activeRuns.containsKey(sessionId);
        activeRuns.put(sessionId, handle);
        log.debug("run registered: sessionId={} totalActive={} replaced={}",
                sessionId, activeRuns.size(), wasActive);
    }

    /**
     * Clear the active run for the given session.
     * Only clears if the provided handle matches the current active handle.
     */
    public void clearActiveRun(String sessionId, RunHandle handle) {
        if (activeRuns.get(sessionId) == handle) {
            activeRuns.remove(sessionId);
            log.debug("run cleared: sessionId={} totalActive={}", sessionId, activeRuns.size());
            notifyRunEnded(sessionId);
        } else {
            log.debug("run clear skipped: sessionId={} reason=handle_mismatch", sessionId);
        }
    }

    /**
     * Force-clear any active run for a session (regardless of handle match).
     */
    public void forceCloseRun(String sessionId) {
        RunHandle removed = activeRuns.remove(sessionId);
        if (removed != null) {
            log.debug("run force-closed: sessionId={}", sessionId);
            notifyRunEnded(sessionId);
        }
    }

    // ── Query ─────────────────────────────────────────────────────────

    /** Whether a run is currently active for the given session. */
    public boolean isRunActive(String sessionId) {
        return activeRuns.containsKey(sessionId);
    }

    /** Whether the active run is currently streaming. */
    public boolean isStreaming(String sessionId) {
        RunHandle handle = activeRuns.get(sessionId);
        return handle != null && handle.isStreaming();
    }

    /** Whether the active run is currently compacting. */
    public boolean isCompacting(String sessionId) {
        RunHandle handle = activeRuns.get(sessionId);
        return handle != null && handle.isCompacting();
    }

    /** Get the number of currently active runs. */
    public int activeCount() {
        return activeRuns.size();
    }

    // ── Message queue ─────────────────────────────────────────────────

    /**
     * Queue a message into the active run for the given session.
     *
     * @return true if the message was queued, false if no active/streaming run
     */
    public boolean queueMessage(String sessionId, String text) {
        RunHandle handle = activeRuns.get(sessionId);
        if (handle == null) {
            log.debug("queue message failed: sessionId={} reason=no_active_run", sessionId);
            return false;
        }
        if (!handle.isStreaming()) {
            log.debug("queue message failed: sessionId={} reason=not_streaming", sessionId);
            return false;
        }
        if (handle.isCompacting()) {
            log.debug("queue message failed: sessionId={} reason=compacting", sessionId);
            return false;
        }
        handle.queueMessage(text);
        log.debug("message queued: sessionId={}", sessionId);
        return true;
    }

    // ── Abort ─────────────────────────────────────────────────────────

    /**
     * Abort the active run for the given session.
     *
     * @return true if a run was found and aborted
     */
    public boolean abortRun(String sessionId) {
        RunHandle handle = activeRuns.get(sessionId);
        if (handle == null) {
            log.debug("abort failed: sessionId={} reason=no_active_run", sessionId);
            return false;
        }
        log.info("aborting run: sessionId={}", sessionId);
        handle.abort();
        return true;
    }

    // ── Wait ──────────────────────────────────────────────────────────

    /**
     * Wait for the active run to end, with a timeout.
     *
     * @param sessionId the session
     * @param timeoutMs max wait in milliseconds
     * @return CompletableFuture completing with true if the run ended, false on
     *         timeout
     */
    public CompletableFuture<Boolean> waitForRunEnd(String sessionId, long timeoutMs) {
        if (sessionId == null || !activeRuns.containsKey(sessionId)) {
            return CompletableFuture.completedFuture(true);
        }

        CompletableFuture<Boolean> future = new CompletableFuture<>();

        // Register waiter
        waiters.computeIfAbsent(sessionId, k -> ConcurrentHashMap.newKeySet()).add(future);

        // Schedule timeout
        long effectiveTimeout = Math.max(100, timeoutMs);
        CompletableFuture.delayedExecutor(effectiveTimeout, TimeUnit.MILLISECONDS)
                .execute(() -> {
                    if (!future.isDone()) {
                        future.complete(false);
                        Set<CompletableFuture<Boolean>> set = waiters.get(sessionId);
                        if (set != null) {
                            set.remove(future);
                            if (set.isEmpty()) {
                                waiters.remove(sessionId);
                            }
                        }
                        log.warn("wait timeout: sessionId={} timeoutMs={}", sessionId, effectiveTimeout);
                    }
                });

        // Check again in case the run ended between the check and registration
        if (!activeRuns.containsKey(sessionId) && !future.isDone()) {
            future.complete(true);
            Set<CompletableFuture<Boolean>> set = waiters.get(sessionId);
            if (set != null) {
                set.remove(future);
                if (set.isEmpty()) {
                    waiters.remove(sessionId);
                }
            }
        }

        return future;
    }

    // ── Notify waiters ────────────────────────────────────────────────

    private void notifyRunEnded(String sessionId) {
        Set<CompletableFuture<Boolean>> set = waiters.remove(sessionId);
        if (set == null || set.isEmpty()) {
            return;
        }
        log.debug("notifying waiters: sessionId={} waiterCount={}", sessionId, set.size());
        for (CompletableFuture<Boolean> f : set) {
            if (!f.isDone()) {
                f.complete(true);
            }
        }
    }
}
