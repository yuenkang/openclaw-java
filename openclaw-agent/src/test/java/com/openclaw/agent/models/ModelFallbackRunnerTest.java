package com.openclaw.agent.models;

import com.openclaw.agent.runtime.ErrorClassifier;
import org.junit.jupiter.api.Test;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link ModelFallbackRunner}.
 */
class ModelFallbackRunnerTest {

    // ── Backoff computation ───────────────────────────────────────

    @Test
    void computeBackoff_rateLimitExponential() {
        assertEquals(500, ModelFallbackRunner.computeBackoffMs(
                ErrorClassifier.FailoverReason.RATE_LIMIT, 0));
        assertEquals(1000, ModelFallbackRunner.computeBackoffMs(
                ErrorClassifier.FailoverReason.RATE_LIMIT, 1));
        assertEquals(2000, ModelFallbackRunner.computeBackoffMs(
                ErrorClassifier.FailoverReason.RATE_LIMIT, 2));
    }

    @Test
    void computeBackoff_overloadedExponential() {
        assertEquals(500, ModelFallbackRunner.computeBackoffMs(
                ErrorClassifier.FailoverReason.OVERLOADED, 0));
        assertEquals(1000, ModelFallbackRunner.computeBackoffMs(
                ErrorClassifier.FailoverReason.OVERLOADED, 1));
    }

    @Test
    void computeBackoff_timeoutExponential() {
        assertEquals(500, ModelFallbackRunner.computeBackoffMs(
                ErrorClassifier.FailoverReason.TIMEOUT, 0));
    }

    @Test
    void computeBackoff_cappedAtMax() {
        long result = ModelFallbackRunner.computeBackoffMs(
                ErrorClassifier.FailoverReason.RATE_LIMIT, 10);
        assertEquals(ModelFallbackRunner.MAX_BACKOFF_MS, result);
    }

    @Test
    void computeBackoff_authImmediate() {
        assertEquals(0, ModelFallbackRunner.computeBackoffMs(
                ErrorClassifier.FailoverReason.AUTH, 0));
    }

    @Test
    void computeBackoff_billingImmediate() {
        assertEquals(0, ModelFallbackRunner.computeBackoffMs(
                ErrorClassifier.FailoverReason.BILLING, 0));
    }

    @Test
    void computeBackoff_unknownImmediate() {
        assertEquals(0, ModelFallbackRunner.computeBackoffMs(
                ErrorClassifier.FailoverReason.UNKNOWN, 0));
    }

    // ── FallbackAttempt record ─────────────────────────────────────

    @Test
    void fallbackAttemptRecord() {
        var attempt = new ModelFallbackRunner.FallbackAttempt(
                "anthropic", "claude-3-sonnet", "rate_limit", 429, "rate_limit_exceeded");
        assertEquals("anthropic", attempt.provider());
        assertEquals("claude-3-sonnet", attempt.model());
        assertEquals("rate_limit", attempt.error());
        assertEquals(429, attempt.status());
        assertEquals("rate_limit_exceeded", attempt.code());
    }

    // ── HttpStatusException ───────────────────────────────────────

    @Test
    void httpStatusException() {
        var ex = new ModelFallbackRunner.HttpStatusException(429, "rate_limit", "Too Many Requests");
        assertEquals(429, ex.getStatusCode());
        assertEquals("rate_limit", ex.getCode());
        assertEquals("Too Many Requests", ex.getMessage());
    }

    // ── ModelFallbackException ─────────────────────────────────────

    @Test
    void modelFallbackException() {
        var attempts = java.util.List.of(
                new ModelFallbackRunner.FallbackAttempt("openai", "gpt-4", "timeout", 504, null));
        var ex = new ModelFallbackRunner.ModelFallbackException("All exhausted", attempts);
        assertEquals("All exhausted", ex.getMessage());
        assertEquals(1, ex.getAttempts().size());
        assertEquals("gpt-4", ex.getAttempts().get(0).model());
    }

    // ── Successful first attempt ──────────────────────────────────

    @Test
    void successfulFirstAttempt() throws Exception {
        var cfg = new com.openclaw.common.config.OpenClawConfig();
        var result = ModelFallbackRunner.runWithModelFallback(
                cfg, "anthropic", "claude-3-sonnet",
                java.util.List.of("openai/gpt-4"),
                (prov, model) -> CompletableFuture.completedFuture("OK"),
                null).get();

        assertEquals("OK", result.result());
        assertEquals("anthropic", result.provider());
        assertEquals("claude-3-sonnet", result.model());
        assertTrue(result.attempts().isEmpty());
    }

    // ── Non-retryable error aborts immediately ────────────────────

    @Test
    void contextOverflowStopsFallback() {
        var cfg = new com.openclaw.common.config.OpenClawConfig();
        var future = ModelFallbackRunner.runWithModelFallback(
                cfg, "anthropic", "claude-3-sonnet",
                java.util.List.of("openai/gpt-4"),
                (prov, model) -> CompletableFuture.failedFuture(
                        new RuntimeException("maximum context length exceeded")),
                null);

        ExecutionException ex = assertThrows(ExecutionException.class, future::get);
        assertInstanceOf(ModelFallbackRunner.ModelFallbackException.class, ex.getCause());
        // Should fail after first attempt, not try second candidate
        var mfe = (ModelFallbackRunner.ModelFallbackException) ex.getCause();
        assertEquals(1, mfe.getAttempts().size());
        assertTrue(mfe.getMessage().contains("CONTEXT_OVERFLOW"));
    }
}
