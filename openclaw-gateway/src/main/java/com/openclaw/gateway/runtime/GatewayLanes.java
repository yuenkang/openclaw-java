package com.openclaw.gateway.runtime;

import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;

/**
 * Concurrent lane management — controls how many agent runs may execute
 * in parallel per lane (main / cron / subagent).
 * Corresponds to TS {@code server-lanes.ts}.
 */
@Slf4j
public class GatewayLanes {

    /** Lane identifiers, matching TS CommandLane enum. */
    public enum Lane {
        MAIN, CRON, SUBAGENT
    }

    /** Default concurrency limits. */
    private static final Map<Lane, Integer> DEFAULTS = Map.of(
            Lane.MAIN, 1,
            Lane.CRON, 1,
            Lane.SUBAGENT, 2);

    private final Map<Lane, Semaphore> semaphores = new ConcurrentHashMap<>();

    public GatewayLanes() {
        DEFAULTS.forEach((lane, permits) -> semaphores.put(lane, new Semaphore(permits)));
    }

    /**
     * Update concurrency limit for a lane.
     */
    public void setConcurrency(Lane lane, int maxConcurrent) {
        int clamped = Math.max(1, maxConcurrent);
        semaphores.put(lane, new Semaphore(clamped));
        log.debug("lane {} concurrency set to {}", lane, clamped);
    }

    /**
     * Apply lane concurrency from config.
     * Corresponds to TS {@code applyGatewayLaneConcurrency()}.
     */
    public void applyFromConfig(
            int mainConcurrent,
            int cronConcurrent,
            int subagentConcurrent) {
        setConcurrency(Lane.MAIN, mainConcurrent);
        setConcurrency(Lane.CRON, cronConcurrent);
        setConcurrency(Lane.SUBAGENT, subagentConcurrent);
    }

    /**
     * Try to acquire a permit for the given lane (non-blocking).
     */
    public boolean tryAcquire(Lane lane) {
        Semaphore sem = semaphores.get(lane);
        return sem != null && sem.tryAcquire();
    }

    /**
     * Release a permit for the given lane.
     */
    public void release(Lane lane) {
        Semaphore sem = semaphores.get(lane);
        if (sem != null)
            sem.release();
    }

    /**
     * Get available permits for a lane.
     */
    public int availablePermits(Lane lane) {
        Semaphore sem = semaphores.get(lane);
        return sem != null ? sem.availablePermits() : 0;
    }
}
