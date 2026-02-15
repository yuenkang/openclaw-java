package com.openclaw.common.infra;

import java.util.concurrent.ThreadLocalRandom;

/**
 * Exponential backoff computation and sleep-with-abort.
 * Corresponds to TypeScript's infra/backoff.ts.
 */
public final class Backoff {

    private Backoff() {
    }

    /**
     * Backoff policy configuration.
     *
     * @param initialMs initial delay in milliseconds
     * @param maxMs     maximum delay in milliseconds
     * @param factor    multiplicative factor per attempt
     * @param jitter    jitter ratio (0..1)
     */
    public record Policy(long initialMs, long maxMs, double factor, double jitter) {

        /** Sensible default: 300ms initial, 30s max, factor 2, no jitter. */
        public static final Policy DEFAULT = new Policy(300, 30_000, 2.0, 0.0);
    }

    /**
     * Compute the backoff delay for a given attempt.
     *
     * @param policy  backoff policy
     * @param attempt 1-based attempt number
     * @return delay in milliseconds (capped at {@code policy.maxMs})
     */
    public static long compute(Policy policy, int attempt) {
        double base = policy.initialMs * Math.pow(policy.factor, Math.max(attempt - 1, 0));
        double jitter = base * policy.jitter * ThreadLocalRandom.current().nextDouble();
        return Math.min(policy.maxMs, Math.round(base + jitter));
    }

    /**
     * Sleep for the specified duration, respecting an interrupt.
     *
     * @param ms milliseconds to sleep; if {@code <= 0} returns immediately
     * @throws InterruptedException if the thread is interrupted during sleep
     */
    public static void sleep(long ms) throws InterruptedException {
        if (ms <= 0) {
            return;
        }
        Thread.sleep(ms);
    }

    /**
     * Sleep for the specified duration, aborting if the flag is set.
     *
     * @param ms      milliseconds to sleep
     * @param aborted volatile flag to check for abort; may be {@code null}
     * @throws InterruptedException if interrupted or aborted
     */
    public static void sleepWithAbort(long ms, java.util.concurrent.atomic.AtomicBoolean aborted)
            throws InterruptedException {
        if (ms <= 0) {
            return;
        }
        long deadline = System.currentTimeMillis() + ms;
        long remaining = ms;
        while (remaining > 0) {
            if (aborted != null && aborted.get()) {
                throw new InterruptedException("aborted");
            }
            Thread.sleep(Math.min(remaining, 100));
            remaining = deadline - System.currentTimeMillis();
        }
    }
}
