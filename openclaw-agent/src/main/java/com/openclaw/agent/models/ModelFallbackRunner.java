package com.openclaw.agent.models;

import com.openclaw.agent.models.ModelSelector.ModelRef;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiFunction;

/**
 * Executes a function with model fallback support.
 * On failure, retries with the next candidate model in the fallback chain.
 * Corresponds to TypeScript's model-fallback.ts (runWithModelFallback).
 */
@Slf4j
public class ModelFallbackRunner {

    /**
     * Record of a failed attempt during fallback execution.
     */
    public record FallbackAttempt(
            String provider,
            String model,
            String error,
            int status,
            String code) {
    }

    /**
     * Successful result of a fallback-aware execution.
     */
    public record FallbackResult<T>(
            T result,
            String provider,
            String model,
            List<FallbackAttempt> attempts) {
    }

    /**
     * Callback invoked on each failed attempt (for logging/telemetry).
     */
    @FunctionalInterface
    public interface ErrorCallback {
        void onError(String provider, String model, Throwable error, int attempt, int total);
    }

    /**
     * Run a function with model fallback. Tries each candidate model in order.
     * Aborts immediately on non-retryable errors (e.g. AbortException).
     *
     * @param cfg               OpenClaw config (for resolving fallback candidates)
     * @param provider          primary provider id
     * @param model             primary model id
     * @param fallbacksOverride explicit fallback list (null = use config defaults)
     * @param run               the function to run (provider, model) -> result
     * @param onError           optional callback for each failed attempt
     * @return the result with metadata about which model succeeded
     */
    public static <T> CompletableFuture<FallbackResult<T>> runWithModelFallback(
            OpenClawConfig cfg,
            String provider,
            String model,
            List<String> fallbacksOverride,
            BiFunction<String, String, CompletableFuture<T>> run,
            ErrorCallback onError) {

        List<ModelRef> candidates;
        if (fallbacksOverride != null) {
            // Build from explicit overrides
            candidates = new ArrayList<>();
            candidates.add(new ModelRef(
                    ModelSelector.normalizeProviderId(provider), model));
            for (String raw : fallbacksOverride) {
                ModelRef ref = ModelSelector.parseModelRef(raw, ModelSelector.DEFAULT_PROVIDER);
                if (ref != null && !candidates.contains(ref)) {
                    candidates.add(ref);
                }
            }
        } else {
            candidates = ModelSelector.resolveFallbackCandidates(cfg, provider, model);
        }

        List<FallbackAttempt> attempts = new ArrayList<>();
        return runCandidates(candidates, 0, attempts, run, onError);
    }

    /**
     * Convenience: run with config fallbacks only (no explicit overrides).
     */
    public static <T> CompletableFuture<FallbackResult<T>> runWithModelFallback(
            OpenClawConfig cfg,
            String provider,
            String model,
            BiFunction<String, String, CompletableFuture<T>> run) {
        return runWithModelFallback(cfg, provider, model, null, run, null);
    }

    private static <T> CompletableFuture<FallbackResult<T>> runCandidates(
            List<ModelRef> candidates,
            int index,
            List<FallbackAttempt> attempts,
            BiFunction<String, String, CompletableFuture<T>> run,
            ErrorCallback onError) {

        if (index >= candidates.size()) {
            String msg = "All model candidates exhausted after " + attempts.size() + " attempts";
            if (!attempts.isEmpty()) {
                msg += "; last error: " + attempts.get(attempts.size() - 1).error();
            }
            return CompletableFuture.failedFuture(new ModelFallbackException(msg, attempts));
        }

        ModelRef candidate = candidates.get(index);
        log.debug("Trying model {}/{} (attempt {}/{})",
                candidate.provider(), candidate.model(), index + 1, candidates.size());

        return run.apply(candidate.provider(), candidate.model())
                .thenApply(result -> new FallbackResult<>(result, candidate.provider(), candidate.model(), attempts))
                .exceptionallyCompose(error -> {
                    Throwable cause = unwrap(error);

                    // Record the attempt
                    int status = extractStatus(cause);
                    String code = extractCode(cause);
                    attempts.add(new FallbackAttempt(
                            candidate.provider(), candidate.model(),
                            cause.getMessage(), status, code));

                    // Notify callback
                    if (onError != null) {
                        try {
                            onError.onError(candidate.provider(), candidate.model(),
                                    cause, index + 1, candidates.size());
                        } catch (Exception e) {
                            log.debug("Error callback threw: {}", e.getMessage());
                        }
                    }

                    // Don't retry on abort/cancellation
                    if (isAbortError(cause)) {
                        return CompletableFuture.failedFuture(
                                new ModelFallbackException("Aborted", attempts));
                    }

                    // Try next candidate
                    return runCandidates(candidates, index + 1, attempts, run, onError);
                });
    }

    private static Throwable unwrap(Throwable t) {
        while (t instanceof java.util.concurrent.CompletionException && t.getCause() != null) {
            t = t.getCause();
        }
        return t;
    }

    private static boolean isAbortError(Throwable t) {
        return t instanceof java.util.concurrent.CancellationException
                || t instanceof InterruptedException
                || (t.getMessage() != null && t.getMessage().contains("abort"));
    }

    private static int extractStatus(Throwable t) {
        // If the exception carries an HTTP status, extract it
        if (t instanceof HttpStatusException hse) {
            return hse.getStatusCode();
        }
        return 0;
    }

    private static String extractCode(Throwable t) {
        if (t instanceof HttpStatusException hse) {
            return hse.getCode();
        }
        return null;
    }

    // =========================================================================
    // Exception types
    // =========================================================================

    /**
     * Exception indicating an HTTP error with a status code.
     * Model providers can throw this for failover-eligible errors.
     */
    public static class HttpStatusException extends RuntimeException {
        private final int statusCode;
        private final String code;

        public HttpStatusException(int statusCode, String code, String message) {
            super(message);
            this.statusCode = statusCode;
            this.code = code;
        }

        public int getStatusCode() {
            return statusCode;
        }

        public String getCode() {
            return code;
        }
    }

    /**
     * Exception thrown when all fallback candidates are exhausted.
     */
    public static class ModelFallbackException extends RuntimeException {
        private final List<FallbackAttempt> attempts;

        public ModelFallbackException(String message, List<FallbackAttempt> attempts) {
            super(message);
            this.attempts = attempts;
        }

        public List<FallbackAttempt> getAttempts() {
            return attempts;
        }
    }
}
