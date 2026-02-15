package com.openclaw.gateway.runtime;

import com.openclaw.common.infra.HeartbeatEvents;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.ZonedDateTime;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

/**
 * Heartbeat runner — periodically runs heartbeat tasks based on a configurable
 * interval.
 * The actual heartbeat logic (LLM call, delivery) is supplied via a callback.
 * Corresponds to TypeScript's infra/heartbeat-runner.ts (simplified for Java
 * server).
 */
public class HeartbeatRunner {

    private static final Logger log = LoggerFactory.getLogger(HeartbeatRunner.class);

    private static final long DEFAULT_INTERVAL_MS = 60_000 * 30; // 30 min default
    private static final long MIN_INTERVAL_MS = 10_000; // 10 seconds minimum

    // ── Result type ─────────────────────────────────────────────────────

    public record RunResult(String status, String reason, Long durationMs) {
        public static RunResult ran(long durationMs) {
            return new RunResult("ran", null, durationMs);
        }

        public static RunResult skipped(String reason) {
            return new RunResult("skipped", reason, null);
        }

        public static RunResult failed(String reason) {
            return new RunResult("failed", reason, null);
        }
    }

    // ── Summary ─────────────────────────────────────────────────────────

    public record Summary(boolean enabled, long everyMs, String prompt, String target, String model) {
    }

    // ── Fields ──────────────────────────────────────────────────────────

    private final ScheduledExecutorService scheduler;
    private final Function<String, RunResult> heartbeatTask;
    private final AtomicBoolean enabled = new AtomicBoolean(true);
    private final AtomicLong intervalMs = new AtomicLong(DEFAULT_INTERVAL_MS);
    private final AtomicLong lastRunMs = new AtomicLong(0);
    private final AtomicReference<ScheduledFuture<?>> scheduledTask = new AtomicReference<>();

    // ── Constructor ─────────────────────────────────────────────────────

    /**
     * @param scheduler     executor for scheduling periodic tasks
     * @param heartbeatTask callback that performs the actual heartbeat; receives a
     *                      reason string
     */
    public HeartbeatRunner(ScheduledExecutorService scheduler,
            Function<String, RunResult> heartbeatTask) {
        this.scheduler = scheduler;
        this.heartbeatTask = heartbeatTask;
    }

    // ── Lifecycle ───────────────────────────────────────────────────────

    /**
     * Start periodic heartbeat execution.
     */
    public void start() {
        if (!enabled.get()) {
            return;
        }
        stop();
        long interval = Math.max(MIN_INTERVAL_MS, intervalMs.get());
        ScheduledFuture<?> future = scheduler.scheduleWithFixedDelay(
                this::runOnce,
                interval,
                interval,
                TimeUnit.MILLISECONDS);
        scheduledTask.set(future);
        log.info("Heartbeat started with interval {}ms", interval);
    }

    /**
     * Stop periodic heartbeat execution.
     */
    public void stop() {
        ScheduledFuture<?> future = scheduledTask.getAndSet(null);
        if (future != null) {
            future.cancel(false);
            log.info("Heartbeat stopped");
        }
    }

    /**
     * Run a single heartbeat iteration.
     */
    public RunResult runOnce() {
        return runOnce("scheduled");
    }

    /**
     * Run a single heartbeat iteration with a specific reason.
     */
    public RunResult runOnce(String reason) {
        if (!enabled.get()) {
            return RunResult.skipped("disabled");
        }

        long startedAt = System.currentTimeMillis();
        try {
            RunResult result = heartbeatTask.apply(reason);
            lastRunMs.set(System.currentTimeMillis());

            if ("ran".equals(result.status())) {
                log.debug("Heartbeat completed in {}ms", result.durationMs());
            } else if ("skipped".equals(result.status())) {
                log.debug("Heartbeat skipped: {}", result.reason());
            }
            return result;
        } catch (Exception e) {
            long duration = System.currentTimeMillis() - startedAt;
            log.warn("Heartbeat failed after {}ms: {}", duration, e.getMessage());
            HeartbeatEvents.emit(b -> b
                    .status(HeartbeatEvents.Status.FAILED)
                    .reason(e.getMessage())
                    .durationMs(duration));
            return RunResult.failed(e.getMessage());
        }
    }

    // ── Configuration ───────────────────────────────────────────────────

    public void setEnabled(boolean flag) {
        enabled.set(flag);
        if (!flag) {
            stop();
        }
    }

    public boolean isEnabled() {
        return enabled.get();
    }

    public void setIntervalMs(long ms) {
        intervalMs.set(Math.max(MIN_INTERVAL_MS, ms));
    }

    public long getIntervalMs() {
        return intervalMs.get();
    }

    /**
     * Update interval and restart if running.
     */
    public void updateInterval(long ms) {
        setIntervalMs(ms);
        if (scheduledTask.get() != null) {
            start(); // restart with new interval
        }
    }

    /**
     * Parse a duration string like "30m", "1h", "90s" into milliseconds.
     */
    public static long parseDurationMs(String raw) {
        if (raw == null || raw.isBlank()) {
            return DEFAULT_INTERVAL_MS;
        }
        String trimmed = raw.trim().toLowerCase();
        try {
            if (trimmed.endsWith("ms")) {
                return Long.parseLong(trimmed.substring(0, trimmed.length() - 2));
            }
            if (trimmed.endsWith("s")) {
                return Long.parseLong(trimmed.substring(0, trimmed.length() - 1)) * 1000;
            }
            if (trimmed.endsWith("m")) {
                return Long.parseLong(trimmed.substring(0, trimmed.length() - 1)) * 60_000;
            }
            if (trimmed.endsWith("h")) {
                return Long.parseLong(trimmed.substring(0, trimmed.length() - 1)) * 3_600_000;
            }
            // Try as plain number (minutes)
            return Long.parseLong(trimmed) * 60_000;
        } catch (NumberFormatException e) {
            return DEFAULT_INTERVAL_MS;
        }
    }

    // ── Active hours ────────────────────────────────────────────────────

    /**
     * Check if the current time is within active hours.
     *
     * @param startHhmm start time as "HH:mm"
     * @param endHhmm   end time as "HH:mm" (or "24:00")
     * @param timezone  IANA timezone string
     * @return true if within active hours, or if arguments are invalid
     */
    public static boolean isWithinActiveHours(String startHhmm, String endHhmm, String timezone) {
        if (startHhmm == null || endHhmm == null) {
            return true;
        }
        try {
            int startMin = parseHhmm(startHhmm, false);
            int endMin = parseHhmm(endHhmm, true);
            if (startMin < 0 || endMin < 0 || startMin == endMin) {
                return true;
            }

            ZonedDateTime now = ZonedDateTime.now(java.time.ZoneId.of(
                    timezone != null ? timezone : "UTC"));
            int currentMin = now.getHour() * 60 + now.getMinute();

            if (endMin > startMin) {
                return currentMin >= startMin && currentMin < endMin;
            } else {
                return currentMin >= startMin || currentMin < endMin;
            }
        } catch (Exception e) {
            return true; // on error, allow
        }
    }

    private static int parseHhmm(String hhmm, boolean allow24) {
        if (hhmm == null || !hhmm.matches("^([01]\\d|2[0-3]|24):[0-5]\\d$")) {
            return -1;
        }
        String[] parts = hhmm.split(":");
        int h = Integer.parseInt(parts[0]);
        int m = Integer.parseInt(parts[1]);
        if (h == 24 && (!allow24 || m != 0)) {
            return -1;
        }
        return h * 60 + m;
    }
}
