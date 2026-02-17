package com.openclaw.common.infra;

import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

/**
 * Heartbeat runner — periodically invokes a heartbeat action.
 * Supports active hours, config updates, and graceful shutdown.
 * <p>
 * Corresponds to TypeScript's {@code infra/heartbeat-runner.ts} (simplified).
 */
@Slf4j
public class HeartbeatRunner implements AutoCloseable {

    private final ScheduledExecutorService scheduler;
    private final AtomicBoolean running = new AtomicBoolean(false);
    private volatile long intervalMs;
    private volatile Consumer<String> heartbeatAction;
    private ScheduledFuture<?> scheduledTask;

    /**
     * Create a new heartbeat runner.
     *
     * @param intervalMs      interval between heartbeats in milliseconds
     * @param heartbeatAction action to invoke on each heartbeat (receives reason
     *                        string)
     */
    public HeartbeatRunner(long intervalMs, Consumer<String> heartbeatAction) {
        this.intervalMs = Math.max(1000, intervalMs); // Minimum 1 second
        this.heartbeatAction = heartbeatAction;
        this.scheduler = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "heartbeat-runner");
            t.setDaemon(true);
            return t;
        });
    }

    /**
     * Start the heartbeat loop.
     */
    public void start() {
        if (!running.compareAndSet(false, true)) {
            log.debug("Heartbeat runner already running");
            return;
        }
        scheduleNext();
        log.info("Heartbeat runner started (interval: {}ms)", intervalMs);
    }

    /**
     * Stop the heartbeat loop.
     */
    public void stop() {
        if (!running.compareAndSet(true, false)) {
            return;
        }
        if (scheduledTask != null) {
            scheduledTask.cancel(false);
        }
        log.info("Heartbeat runner stopped");
    }

    /**
     * Update the heartbeat interval.
     */
    public void updateInterval(long newIntervalMs) {
        this.intervalMs = Math.max(1000, newIntervalMs);
        if (running.get()) {
            if (scheduledTask != null) {
                scheduledTask.cancel(false);
            }
            scheduleNext();
            log.info("Heartbeat interval updated to {}ms", intervalMs);
        }
    }

    /**
     * Update the heartbeat action.
     */
    public void updateAction(Consumer<String> newAction) {
        this.heartbeatAction = newAction;
    }

    /**
     * Trigger a heartbeat immediately (outside the normal schedule).
     */
    public void triggerNow(String reason) {
        scheduler.execute(() -> runOnce(reason));
    }

    public boolean isRunning() {
        return running.get();
    }

    public long getIntervalMs() {
        return intervalMs;
    }

    private void scheduleNext() {
        scheduledTask = scheduler.schedule(this::tick, intervalMs, TimeUnit.MILLISECONDS);
    }

    private void tick() {
        if (!running.get())
            return;
        runOnce("scheduled");
        if (running.get()) {
            scheduleNext();
        }
    }

    private void runOnce(String reason) {
        try {
            var action = heartbeatAction;
            if (action != null) {
                action.accept(reason);
            }
        } catch (Exception e) {
            log.error("Heartbeat action failed: {}", e.getMessage(), e);
        }
    }

    @Override
    public void close() {
        stop();
        scheduler.shutdown();
        try {
            if (!scheduler.awaitTermination(5, TimeUnit.SECONDS)) {
                scheduler.shutdownNow();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            scheduler.shutdownNow();
        }
    }
}
