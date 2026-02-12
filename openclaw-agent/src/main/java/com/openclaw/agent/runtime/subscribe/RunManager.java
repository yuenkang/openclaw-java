package com.openclaw.agent.runtime.subscribe;

import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.*;
import java.util.function.Consumer;

/**
 * Run manager — tracks active embedded runs, supports message queuing, abort,
 * and wait.
 * Corresponds to TypeScript pi-embedded-runner/runs.ts.
 */
@Slf4j
public final class RunManager {

    private RunManager() {
    }

    /** Handle for an active embedded run. */
    public interface EmbeddedRunHandle {
        CompletableFuture<Void> queueMessage(String text);

        boolean isStreaming();

        boolean isCompacting();

        void abort();
    }

    private static final ConcurrentHashMap<String, EmbeddedRunHandle> ACTIVE_RUNS = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<String, Set<RunWaiter>> WAITERS = new ConcurrentHashMap<>();

    private record RunWaiter(CompletableFuture<Boolean> future, ScheduledFuture<?> timer) {
    }

    private static final ScheduledExecutorService SCHEDULER = Executors.newSingleThreadScheduledExecutor(r -> {
        Thread t = new Thread(r, "run-manager-timer");
        t.setDaemon(true);
        return t;
    });

    // ── Queue / Abort ────────────────────────────────────────────────

    /**
     * Queue a message to an active embedded run.
     * 
     * @return true if message was queued successfully
     */
    public static boolean queueMessage(String sessionId, String text) {
        EmbeddedRunHandle handle = ACTIVE_RUNS.get(sessionId);
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
        return true;
    }

    /**
     * Abort an active embedded run.
     * 
     * @return true if run was found and abort was requested
     */
    public static boolean abort(String sessionId) {
        EmbeddedRunHandle handle = ACTIVE_RUNS.get(sessionId);
        if (handle == null) {
            log.debug("abort failed: sessionId={} reason=no_active_run", sessionId);
            return false;
        }
        log.debug("aborting run: sessionId={}", sessionId);
        handle.abort();
        return true;
    }

    // ── Status checks ───────────────────────────────────────────────

    public static boolean isRunActive(String sessionId) {
        boolean active = ACTIVE_RUNS.containsKey(sessionId);
        if (active)
            log.debug("run active check: sessionId={} active=true", sessionId);
        return active;
    }

    public static boolean isRunStreaming(String sessionId) {
        EmbeddedRunHandle handle = ACTIVE_RUNS.get(sessionId);
        return handle != null && handle.isStreaming();
    }

    // ── Wait for run end ────────────────────────────────────────────

    /**
     * Wait for an active run to finish, with timeout.
     * 
     * @return CompletableFuture resolving to true if ended, false on timeout
     */
    public static CompletableFuture<Boolean> waitForRunEnd(String sessionId, long timeoutMs) {
        if (sessionId == null || !ACTIVE_RUNS.containsKey(sessionId)) {
            return CompletableFuture.completedFuture(true);
        }
        log.debug("waiting for run end: sessionId={} timeoutMs={}", sessionId, timeoutMs);
        CompletableFuture<Boolean> future = new CompletableFuture<>();
        long effectiveTimeout = Math.max(100, timeoutMs);
        ScheduledFuture<?> timer = SCHEDULER.schedule(() -> {
            if (future.complete(false)) {
                log.warn("wait timeout: sessionId={} timeoutMs={}", sessionId, timeoutMs);
                removeWaiter(sessionId, future);
            }
        }, effectiveTimeout, TimeUnit.MILLISECONDS);
        RunWaiter waiter = new RunWaiter(future, timer);
        WAITERS.computeIfAbsent(sessionId, k -> ConcurrentHashMap.newKeySet()).add(waiter);
        // Double-check after registering
        if (!ACTIVE_RUNS.containsKey(sessionId)) {
            timer.cancel(false);
            future.complete(true);
            removeWaiter(sessionId, future);
        }
        return future;
    }

    public static CompletableFuture<Boolean> waitForRunEnd(String sessionId) {
        return waitForRunEnd(sessionId, 15_000);
    }

    // ── Register / Clear ────────────────────────────────────────────

    public static void setActiveRun(String sessionId, EmbeddedRunHandle handle) {
        boolean wasActive = ACTIVE_RUNS.containsKey(sessionId);
        ACTIVE_RUNS.put(sessionId, handle);
        if (sessionId != null && !sessionId.startsWith("probe-")) {
            log.debug("run registered: sessionId={} totalActive={} replaced={}",
                    sessionId, ACTIVE_RUNS.size(), wasActive);
        }
    }

    public static void clearActiveRun(String sessionId, EmbeddedRunHandle handle) {
        if (ACTIVE_RUNS.get(sessionId) == handle) {
            ACTIVE_RUNS.remove(sessionId);
            if (sessionId != null && !sessionId.startsWith("probe-")) {
                log.debug("run cleared: sessionId={} totalActive={}", sessionId, ACTIVE_RUNS.size());
            }
            notifyRunEnded(sessionId);
        } else {
            log.debug("run clear skipped: sessionId={} reason=handle_mismatch", sessionId);
        }
    }

    // ── Internal ────────────────────────────────────────────────────

    private static void notifyRunEnded(String sessionId) {
        Set<RunWaiter> waiters = WAITERS.remove(sessionId);
        if (waiters == null || waiters.isEmpty())
            return;
        log.debug("notifying waiters: sessionId={} waiterCount={}", sessionId, waiters.size());
        for (RunWaiter waiter : waiters) {
            waiter.timer().cancel(false);
            waiter.future().complete(true);
        }
    }

    private static void removeWaiter(String sessionId, CompletableFuture<Boolean> future) {
        Set<RunWaiter> waiters = WAITERS.get(sessionId);
        if (waiters != null) {
            waiters.removeIf(w -> w.future() == future);
            if (waiters.isEmpty())
                WAITERS.remove(sessionId);
        }
    }
}
