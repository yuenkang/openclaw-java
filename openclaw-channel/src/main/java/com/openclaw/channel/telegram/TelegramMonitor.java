package com.openclaw.channel.telegram;

import lombok.extern.slf4j.Slf4j;

import java.time.Instant;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Telegram connection health monitoring.
 * Tracks update timestamps and detects connection stalls.
 * Corresponds to TypeScript's telegram/monitor.ts.
 */
@Slf4j
public class TelegramMonitor {

    private final String accountId;
    private final AtomicLong lastUpdateAt = new AtomicLong(System.currentTimeMillis());
    private final AtomicLong updateCount = new AtomicLong(0);
    private final AtomicReference<String> status = new AtomicReference<>("idle");
    private final long stallThresholdMs;

    public TelegramMonitor(String accountId) {
        this(accountId, 5 * 60_000); // Default 5 minutes
    }

    public TelegramMonitor(String accountId, long stallThresholdMs) {
        this.accountId = accountId;
        this.stallThresholdMs = stallThresholdMs;
    }

    /**
     * Record that an update was received.
     */
    public void recordUpdate() {
        lastUpdateAt.set(System.currentTimeMillis());
        updateCount.incrementAndGet();
        status.set("active");
    }

    /**
     * Record that polling started.
     */
    public void recordPollingStarted() {
        status.set("polling");
        log.debug("[monitor] Polling started for account: {}", accountId);
    }

    /**
     * Record that polling stopped.
     */
    public void recordPollingStopped() {
        status.set("stopped");
        log.info("[monitor] Polling stopped for account: {}", accountId);
    }

    /**
     * Record a polling error.
     */
    public void recordError(Exception error) {
        status.set("error");
        log.warn("[monitor] Polling error for account {}: {}", accountId, error.getMessage());
    }

    /**
     * Check if the connection appears stalled.
     */
    public boolean isStalled() {
        long elapsed = System.currentTimeMillis() - lastUpdateAt.get();
        return elapsed > stallThresholdMs && "polling".equals(status.get());
    }

    /**
     * Get a health summary.
     */
    public HealthStatus getHealthStatus() {
        return new HealthStatus(
                accountId,
                status.get(),
                Instant.ofEpochMilli(lastUpdateAt.get()),
                updateCount.get(),
                isStalled());
    }

    public record HealthStatus(
            String accountId,
            String status,
            Instant lastUpdateAt,
            long updateCount,
            boolean stalled) {
    }
}
