package com.openclaw.common.infra;

import java.util.concurrent.Callable;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.BiPredicate;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Generic retry executor with exponential backoff, jitter, retryAfter support,
 * and
 * configurable shouldRetry / onRetry hooks.
 * Corresponds to TypeScript's infra/retry.ts.
 */
public final class RetryRunner {

    // ── Configuration ──────────────────────────────────────────────────

    /**
     * Retry configuration.
     *
     * @param attempts   maximum number of attempts (>= 1)
     * @param minDelayMs minimum delay between retries in ms
     * @param maxDelayMs maximum delay between retries in ms
     * @param jitter     jitter ratio (0..1); 0 = no jitter
     */
    public record Config(int attempts, long minDelayMs, long maxDelayMs, double jitter) {

        public static final Config DEFAULT = new Config(3, 300, 30_000, 0.0);
    }

    /**
     * Information passed to onRetry callback.
     */
    public record RetryInfo(int attempt, int maxAttempts, long delayMs, Throwable err, String label) {
    }

    // ── Fields ──────────────────────────────────────────────────────────

    private final Config config;
    private final BiPredicate<Throwable, Integer> shouldRetry;
    private final Function<Throwable, Long> retryAfterMs;
    private final Consumer<RetryInfo> onRetry;

    // ── Constructors ────────────────────────────────────────────────────

    public RetryRunner(Config config,
            BiPredicate<Throwable, Integer> shouldRetry,
            Function<Throwable, Long> retryAfterMs,
            Consumer<RetryInfo> onRetry) {
        this.config = config != null ? config : Config.DEFAULT;
        this.shouldRetry = shouldRetry != null ? shouldRetry : (err, attempt) -> true;
        this.retryAfterMs = retryAfterMs;
        this.onRetry = onRetry;
    }

    public RetryRunner(Config config) {
        this(config, null, null, null);
    }

    public RetryRunner() {
        this(Config.DEFAULT);
    }

    // ── Execute ─────────────────────────────────────────────────────────

    /**
     * Execute the callable with retry logic.
     *
     * @param fn    the operation to retry
     * @param label optional label for logging
     * @param <T>   return type
     * @return the result of the first successful call
     * @throws Exception the last exception if all attempts fail
     */
    public <T> T execute(Callable<T> fn, String label) throws Exception {
        int maxAttempts = config.attempts;
        long minDelay = config.minDelayMs;
        long maxDelay = config.maxDelayMs > 0 ? config.maxDelayMs : Long.MAX_VALUE;
        double jitter = config.jitter;
        Throwable lastErr = null;

        for (int attempt = 1; attempt <= maxAttempts; attempt++) {
            try {
                return fn.call();
            } catch (Exception err) {
                lastErr = err;
                if (attempt >= maxAttempts || !shouldRetry.test(err, attempt)) {
                    break;
                }

                // Compute delay
                long retryAfter = retryAfterMs != null ? retryAfterMs.apply(err) : -1;
                boolean hasRetryAfter = retryAfter > 0;
                long baseDelay = hasRetryAfter
                        ? Math.max(retryAfter, minDelay)
                        : minDelay * (1L << (attempt - 1)); // * 2^(attempt-1)
                long delay = Math.min(baseDelay, maxDelay);
                delay = applyJitter(delay, jitter);
                delay = Math.min(Math.max(delay, minDelay), maxDelay);

                if (onRetry != null) {
                    onRetry.accept(new RetryInfo(attempt, maxAttempts, delay, err, label));
                }

                Thread.sleep(delay);
            }
        }

        if (lastErr instanceof Exception ex) {
            throw ex;
        }
        throw new RuntimeException("Retry failed", lastErr);
    }

    /**
     * Execute with no label.
     */
    public <T> T execute(Callable<T> fn) throws Exception {
        return execute(fn, null);
    }

    /**
     * Execute a Runnable (no return value) with retry.
     */
    public void execute(Runnable fn, String label) throws Exception {
        execute(() -> {
            fn.run();
            return null;
        }, label);
    }

    // ── Static helpers ──────────────────────────────────────────────────

    /**
     * Simple retry: execute fn up to {@code attempts} times with exponential
     * backoff.
     *
     * @param fn             the operation
     * @param attempts       max attempts
     * @param initialDelayMs initial delay
     * @param <T>            return type
     * @return result of the first successful call
     * @throws Exception the last exception
     */
    public static <T> T retrySimple(Callable<T> fn, int attempts, long initialDelayMs) throws Exception {
        attempts = Math.max(1, attempts);
        Throwable lastErr = null;
        for (int i = 0; i < attempts; i++) {
            try {
                return fn.call();
            } catch (Exception err) {
                lastErr = err;
                if (i == attempts - 1) {
                    break;
                }
                long delay = initialDelayMs * (1L << i);
                Thread.sleep(delay);
            }
        }
        if (lastErr instanceof Exception ex) {
            throw ex;
        }
        throw new RuntimeException("Retry failed", lastErr);
    }

    /**
     * Resolve a retry config by merging overrides onto defaults.
     *
     * @param defaults  base defaults (non-null)
     * @param overrides overrides (may be {@code null})
     * @return merged config
     */
    public static Config resolveConfig(Config defaults, Config overrides) {
        if (overrides == null) {
            return defaults;
        }
        int attempts = Math.max(1, overrides.attempts > 0 ? overrides.attempts : defaults.attempts);
        long minDelayMs = Math.max(0, overrides.minDelayMs > 0 ? overrides.minDelayMs : defaults.minDelayMs);
        long maxDelayMs = Math.max(minDelayMs, overrides.maxDelayMs > 0 ? overrides.maxDelayMs : defaults.maxDelayMs);
        double jitter = overrides.jitter >= 0 ? Math.min(overrides.jitter, 1.0) : defaults.jitter;
        return new Config(attempts, minDelayMs, maxDelayMs, jitter);
    }

    // ── Private ─────────────────────────────────────────────────────────

    private static long applyJitter(long delayMs, double jitter) {
        if (jitter <= 0) {
            return delayMs;
        }
        double offset = (ThreadLocalRandom.current().nextDouble() * 2 - 1) * jitter;
        return Math.max(0, Math.round(delayMs * (1 + offset)));
    }
}
