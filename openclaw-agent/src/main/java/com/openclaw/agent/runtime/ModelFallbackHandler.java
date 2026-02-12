package com.openclaw.agent.runtime;

import com.openclaw.agent.models.ModelSelector.ModelRef;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.function.BiFunction;

/**
 * Runs an operation with automatic model fallback.
 * If the primary model fails, tries configured fallback models in order.
 * Corresponds to TypeScript's model-fallback.ts runWithModelFallback.
 */
@Slf4j
public class ModelFallbackHandler {

    public record FallbackAttempt(String provider, String model, String error) {
    }

    public record FallbackResult<T>(T result, String provider, String model, List<FallbackAttempt> attempts) {
    }

    /**
     * Run an operation with automatic model fallback.
     *
     * @param candidates ordered list of model candidates to try
     * @param runner     function that takes (provider, model) and returns a result
     * @param <T>        the result type
     * @return FallbackResult with the successful result and attempt history
     * @throws RuntimeException if all candidates fail
     */
    public static <T> FallbackResult<T> runWithFallback(
            List<ModelRef> candidates,
            BiFunction<String, String, T> runner) {
        return runWithFallback(candidates, runner, null);
    }

    /**
     * Run an operation with automatic model fallback and error callback.
     *
     * @param candidates ordered list of model candidates to try
     * @param runner     function that takes (provider, model) and returns a result
     * @param onError    optional callback invoked on each failure
     * @param <T>        the result type
     * @return FallbackResult with the successful result and attempt history
     * @throws RuntimeException if all candidates fail
     */
    public static <T> FallbackResult<T> runWithFallback(
            List<ModelRef> candidates,
            BiFunction<String, String, T> runner,
            ErrorCallback onError) {

        if (candidates.isEmpty()) {
            throw new IllegalArgumentException("No model candidates provided");
        }

        List<FallbackAttempt> attempts = new ArrayList<>();
        Exception lastError = null;

        for (int i = 0; i < candidates.size(); i++) {
            ModelRef candidate = candidates.get(i);
            try {
                log.debug("Trying model {}/{} (attempt {}/{})",
                        candidate.provider(), candidate.model(), i + 1, candidates.size());

                T result = runner.apply(candidate.provider(), candidate.model());

                return new FallbackResult<>(result, candidate.provider(), candidate.model(), attempts);
            } catch (Exception e) {
                // Check for user abort — don't fall back on abort
                if (isAbortError(e)) {
                    throw e;
                }

                lastError = e;
                String errorMsg = e.getMessage() != null ? e.getMessage() : e.getClass().getSimpleName();
                attempts.add(new FallbackAttempt(candidate.provider(), candidate.model(), errorMsg));

                log.warn("Model {}/{} failed (attempt {}/{}): {}",
                        candidate.provider(), candidate.model(), i + 1, candidates.size(), errorMsg);

                if (onError != null) {
                    try {
                        onError.onError(candidate.provider(), candidate.model(), e, i + 1, candidates.size());
                    } catch (Exception callbackError) {
                        log.debug("Error callback threw: {}", callbackError.getMessage());
                    }
                }
            }
        }

        // All candidates exhausted
        if (attempts.size() <= 1 && lastError != null) {
            throw new RuntimeException(lastError);
        }

        StringBuilder summary = new StringBuilder();
        for (int i = 0; i < attempts.size(); i++) {
            if (i > 0)
                summary.append(" | ");
            FallbackAttempt a = attempts.get(i);
            summary.append(a.provider()).append("/").append(a.model()).append(": ").append(a.error());
        }

        throw new RuntimeException(
                String.format("All models failed (%d): %s", attempts.size(), summary),
                lastError);
    }

    /**
     * Check if an error indicates a user-initiated abort (should not trigger
     * fallback).
     */
    private static boolean isAbortError(Exception e) {
        if (e instanceof InterruptedException)
            return true;
        String name = e.getClass().getSimpleName();
        return "AbortException".equals(name) || "CancellationException".equals(name);
    }

    @FunctionalInterface
    public interface ErrorCallback {
        void onError(String provider, String model, Exception error, int attempt, int total);
    }
}
