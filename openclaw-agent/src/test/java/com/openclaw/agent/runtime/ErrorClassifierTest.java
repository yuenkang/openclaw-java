package com.openclaw.agent.runtime;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link ErrorClassifier}.
 */
class ErrorClassifierTest {

    // ── Context overflow ───────────────────────────────────────────

    @Nested
    class ContextOverflow {

        @ParameterizedTest
        @ValueSource(strings = {
                "maximum context length exceeded",
                "context_length_exceeded",
                "Your request has too many tokens",
                "token limit reached",
                "exceeds the model's maximum context",
                "input is too long for this model",
                "prompt is too long",
                "request too large",
                "content_too_large",
                "max_tokens must be at least 1",
                "payload size exceeds the allowed limit"
        })
        void shouldDetectContextOverflow(String msg) {
            assertTrue(ErrorClassifier.isContextOverflowError(msg));
            assertEquals(ErrorClassifier.FailoverReason.CONTEXT_OVERFLOW,
                    ErrorClassifier.classifyFailoverReason(msg));
        }

        @ParameterizedTest
        @NullAndEmptySource
        void nullOrEmptyIsNotOverflow(String msg) {
            assertFalse(ErrorClassifier.isContextOverflowError(msg));
        }

        @Test
        void likelyContextOverflowIncludesHints() {
            assertTrue(ErrorClassifier.isLikelyContextOverflowError("context window too small for this request"));
            assertTrue(ErrorClassifier.isLikelyContextOverflowError("context overflow detected"));
            assertTrue(ErrorClassifier.isLikelyContextOverflowError("prompt too large to process"));
        }

        @Test
        void contextOverflowIsNotFailoverEligible() {
            assertFalse(ErrorClassifier.isFailoverEligible(ErrorClassifier.FailoverReason.CONTEXT_OVERFLOW));
        }
    }

    // ── Rate limit ────────────────────────────────────────────────

    @Nested
    class RateLimit {

        @ParameterizedTest
        @ValueSource(strings = {
                "rate_limit exceeded",
                "Rate limit reached",
                "Too Many Requests",
                "429 Too many requests",
                "exceeded your current quota",
                "resource has been exhausted",
                "quota exceeded",
                "resource_exhausted",
                "usage limit"
        })
        void shouldDetectRateLimit(String msg) {
            assertTrue(ErrorClassifier.isRateLimitError(msg));
            assertEquals(ErrorClassifier.FailoverReason.RATE_LIMIT,
                    ErrorClassifier.classifyFailoverReason(msg));
        }

        @Test
        void rateLimitIsFailoverEligible() {
            assertTrue(ErrorClassifier.isFailoverEligible(ErrorClassifier.FailoverReason.RATE_LIMIT));
        }
    }

    // ── Billing ───────────────────────────────────────────────────

    @Nested
    class Billing {

        @ParameterizedTest
        @ValueSource(strings = {
                "billing issue on your account",
                "insufficient_quota",
                "insufficient funds",
                "payment required",
                "account deactivated",
                "spending limit reached"
        })
        void shouldDetectBilling(String msg) {
            assertTrue(ErrorClassifier.isBillingError(msg));
        }

        @Test
        void billingIsFailoverEligible() {
            assertTrue(ErrorClassifier.isFailoverEligible(ErrorClassifier.FailoverReason.BILLING));
        }
    }

    // ── Auth ──────────────────────────────────────────────────────

    @Nested
    class Auth {

        @ParameterizedTest
        @ValueSource(strings = {
                "401 Unauthorized",
                "403 Forbidden",
                "Invalid API key provided",
                "invalid_api_key",
                "permission denied",
                "access denied"
        })
        void shouldDetectAuth(String msg) {
            assertTrue(ErrorClassifier.isAuthError(msg));
        }

        @Test
        void authIsFailoverEligible() {
            assertTrue(ErrorClassifier.isFailoverEligible(ErrorClassifier.FailoverReason.AUTH));
        }
    }

    // ── Timeout ───────────────────────────────────────────────────

    @Nested
    class Timeout {

        @ParameterizedTest
        @ValueSource(strings = {
                "Request timeout",
                "timed out waiting for response",
                "ETIMEDOUT",
                "ECONNABORTED",
                "deadline exceeded",
                "504 Gateway Timeout"
        })
        void shouldDetectTimeout(String msg) {
            assertTrue(ErrorClassifier.isTimeoutError(msg));
        }

        @Test
        void timeoutIsFailoverEligible() {
            assertTrue(ErrorClassifier.isFailoverEligible(ErrorClassifier.FailoverReason.TIMEOUT));
        }
    }

    // ── Overloaded ────────────────────────────────────────────────

    @Nested
    class Overloaded {

        @ParameterizedTest
        @ValueSource(strings = {
                "server overloaded",
                "503 Service Unavailable",
                "capacity limit",
                "server busy",
                "temporarily unavailable",
                "502 Bad Gateway"
        })
        void shouldDetectOverloaded(String msg) {
            assertTrue(ErrorClassifier.isOverloadedError(msg));
        }

        @Test
        void overloadedIsFailoverEligible() {
            assertTrue(ErrorClassifier.isFailoverEligible(ErrorClassifier.FailoverReason.OVERLOADED));
        }
    }

    // ── Model not found ──────────────────────────────────────────

    @Nested
    class ModelNotFound {

        @ParameterizedTest
        @ValueSource(strings = {
                "model not found",
                "unknown model gpt-5",
                "model_not_found",
                "The model does not exist",
                "not available",
                "404 model error"
        })
        void shouldDetectModelNotFound(String msg) {
            assertTrue(ErrorClassifier.isModelNotFoundError(msg));
        }

        @Test
        void modelNotFoundIsNotFailoverEligible() {
            assertFalse(ErrorClassifier.isFailoverEligible(ErrorClassifier.FailoverReason.MODEL_NOT_FOUND));
        }
    }

    // ── Classify failover reason ─────────────────────────────────

    @Nested
    class ClassifyReason {

        @ParameterizedTest
        @NullAndEmptySource
        void nullOrEmptyReturnsUnknown(String msg) {
            assertEquals(ErrorClassifier.FailoverReason.UNKNOWN,
                    ErrorClassifier.classifyFailoverReason(msg));
        }

        @Test
        void unknownErrorReturnsUnknown() {
            assertEquals(ErrorClassifier.FailoverReason.UNKNOWN,
                    ErrorClassifier.classifyFailoverReason("something weird happened"));
        }

        @Test
        void contextOverflowTakesPrecedenceOverTimeout() {
            // If an error mentions both, context overflow is checked first
            assertEquals(ErrorClassifier.FailoverReason.CONTEXT_OVERFLOW,
                    ErrorClassifier.classifyFailoverReason("maximum context length exceeded; timed out"));
        }

        @Test
        void unknownIsNotFailoverEligible() {
            assertFalse(ErrorClassifier.isFailoverEligible(ErrorClassifier.FailoverReason.UNKNOWN));
        }
    }

    // ── Compaction failure ────────────────────────────────────────

    @Nested
    class CompactionFailure {

        @ParameterizedTest
        @ValueSource(strings = {
                "compaction failed",
                "failed to compact history",
                "summarization failed",
                "compaction error occurred"
        })
        void shouldDetectCompactionFailure(String msg) {
            assertTrue(ErrorClassifier.isCompactionFailureError(msg));
        }

        @ParameterizedTest
        @NullAndEmptySource
        void nullOrEmptyIsNotCompactionFailure(String msg) {
            assertFalse(ErrorClassifier.isCompactionFailureError(msg));
        }
    }

    // ── Text sanitization ────────────────────────────────────────

    @Nested
    class TextSanitization {

        @Test
        void stripFinalTags() {
            assertEquals("Hello  world", ErrorClassifier.stripFinalTags("Hello <final> world</final>"));
            assertEquals("Hello world", ErrorClassifier.stripFinalTags("Hello <final>world< /final >"));
            assertNull(ErrorClassifier.stripFinalTags(null));
        }

        @Test
        void sanitizeRemovesErrorPrefixes() {
            assertEquals("something went wrong",
                    ErrorClassifier.sanitizeUserFacingText("Error: something went wrong"));
            assertEquals("bad request",
                    ErrorClassifier.sanitizeUserFacingText("API Error: bad request"));
            assertEquals("failure",
                    ErrorClassifier.sanitizeUserFacingText("OpenAI Error: failure"));
            assertEquals("issue",
                    ErrorClassifier.sanitizeUserFacingText("Anthropic Error - issue"));
        }

        @Test
        void sanitizeCollapsesDuplicateLines() {
            // Input has no Error prefix, so only duplicate collapse applies
            String input = "line A\nline A\nline A\nOK";
            String result = ErrorClassifier.sanitizeUserFacingText(input);
            assertEquals("line A\nOK", result);
        }

        @Test
        void sanitizeNullOrBlank() {
            assertNull(ErrorClassifier.sanitizeUserFacingText(null));
            assertEquals("  ", ErrorClassifier.sanitizeUserFacingText("  "));
        }

        @Test
        void collapseConsecutiveDuplicateLines() {
            assertEquals("a\nb\nc", ErrorClassifier.collapseConsecutiveDuplicateLines("a\na\nb\nb\nc"));
            assertNull(ErrorClassifier.collapseConsecutiveDuplicateLines(null));
            assertEquals("single", ErrorClassifier.collapseConsecutiveDuplicateLines("single"));
        }
    }

    // ── formatAssistantErrorText ──────────────────────────────────

    @Nested
    class FormatAssistantError {

        @Test
        void billingErrorReturnsSpecialMessage() {
            String result = ErrorClassifier.formatAssistantErrorText("insufficient_quota on your account");
            assertEquals(ErrorClassifier.BILLING_ERROR_USER_MESSAGE, result);
        }

        @Test
        void authErrorReturnsAuthMessage() {
            String result = ErrorClassifier.formatAssistantErrorText("401 Unauthorized");
            assertTrue(result.contains("Authentication error"));
        }

        @Test
        void rateLimitReturnsRateLimitMessage() {
            String result = ErrorClassifier.formatAssistantErrorText("rate_limit exceeded");
            assertTrue(result.contains("Rate limit"));
        }

        @Test
        void timeoutReturnsTimeoutMessage() {
            String result = ErrorClassifier.formatAssistantErrorText("Request timeout");
            assertTrue(result.contains("timed out"));
        }

        @Test
        void overloadedReturnsOverloadedMessage() {
            String result = ErrorClassifier.formatAssistantErrorText("503 Service Unavailable");
            assertTrue(result.contains("overloaded"));
        }

        @Test
        void contextOverflowReturnsOverflowMessage() {
            String result = ErrorClassifier.formatAssistantErrorText("maximum context length exceeded");
            assertTrue(result.contains("Context window overflow"));
        }

        @Test
        void modelNotFoundReturnsModelMessage() {
            String result = ErrorClassifier.formatAssistantErrorText("model_not_found");
            assertTrue(result.contains("Model not found"));
        }

        @Test
        void unknownErrorReturnsSanitizedOriginal() {
            String result = ErrorClassifier.formatAssistantErrorText("Error: something broke");
            assertEquals("something broke", result);
        }

        @Test
        void nullOrEmptyReturnsNull() {
            assertNull(ErrorClassifier.formatAssistantErrorText(null));
            assertNull(ErrorClassifier.formatAssistantErrorText(""));
            assertNull(ErrorClassifier.formatAssistantErrorText("   "));
        }
    }
}
