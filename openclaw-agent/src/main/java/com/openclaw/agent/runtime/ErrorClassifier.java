package com.openclaw.agent.runtime;

import java.util.List;
import java.util.regex.Pattern;

import lombok.extern.slf4j.Slf4j;

/**
 * Error classification and failover judgment for agent runs.
 * Corresponds to TypeScript's pi-embedded-helpers/errors.ts.
 *
 * <p>
 * Provides regex-based classification of LLM API errors into categories
 * (rate_limit, billing, auth, timeout, context_overflow, etc.) to drive
 * failover and retry decisions.
 * </p>
 */
@Slf4j
public final class ErrorClassifier {

    private ErrorClassifier() {
    }

    // ── Failover reason enum ──────────────────────────────────────────

    public enum FailoverReason {
        RATE_LIMIT,
        BILLING,
        AUTH,
        TIMEOUT,
        CONTEXT_OVERFLOW,
        OVERLOADED,
        MODEL_NOT_FOUND,
        UNKNOWN
    }

    // ── Billing error user message ────────────────────────────────────

    public static final String BILLING_ERROR_USER_MESSAGE = "⚠️ API provider returned a billing error — your API key has run out of "
            + "credits or has an insufficient balance. Check your provider's billing "
            + "dashboard and top up or switch to a different API key.";

    // ── Context overflow ──────────────────────────────────────────────

    private static final List<Pattern> CONTEXT_OVERFLOW_PATTERNS = List.of(
            Pattern.compile("maximum context length", Pattern.CASE_INSENSITIVE),
            Pattern.compile("context_length_exceeded", Pattern.CASE_INSENSITIVE),
            Pattern.compile("context length exceeded", Pattern.CASE_INSENSITIVE),
            Pattern.compile("maximum.*token", Pattern.CASE_INSENSITIVE),
            Pattern.compile("token limit", Pattern.CASE_INSENSITIVE),
            Pattern.compile("too many tokens", Pattern.CASE_INSENSITIVE),
            Pattern.compile("tokens? exceed", Pattern.CASE_INSENSITIVE),
            Pattern.compile("exceeds the model'?s maximum", Pattern.CASE_INSENSITIVE),
            Pattern.compile("input is too long", Pattern.CASE_INSENSITIVE),
            Pattern.compile("prompt is too long", Pattern.CASE_INSENSITIVE),
            Pattern.compile("request too large", Pattern.CASE_INSENSITIVE),
            Pattern.compile("content_too_large", Pattern.CASE_INSENSITIVE),
            Pattern.compile("max_tokens.*must be at least 1", Pattern.CASE_INSENSITIVE),
            Pattern.compile("payload size exceeds", Pattern.CASE_INSENSITIVE));

    private static final Pattern CONTEXT_WINDOW_TOO_SMALL_RE = Pattern.compile("context window.*(too small|minimum is)",
            Pattern.CASE_INSENSITIVE);

    private static final Pattern CONTEXT_OVERFLOW_HINT_RE = Pattern.compile(
            "context.*overflow|context window.*(too (?:large|long)|exceed|over|limit|max(?:imum)?|"
                    + "requested|sent|tokens)|(?:prompt|request|input).*(too (?:large|long)|exceed|"
                    + "over|limit|max(?:imum)?)",
            Pattern.CASE_INSENSITIVE);

    public static boolean isContextOverflowError(String errorMessage) {
        if (errorMessage == null || errorMessage.isBlank()) {
            return false;
        }
        for (Pattern p : CONTEXT_OVERFLOW_PATTERNS) {
            if (p.matcher(errorMessage).find()) {
                return true;
            }
        }
        return false;
    }

    public static boolean isLikelyContextOverflowError(String errorMessage) {
        if (errorMessage == null || errorMessage.isBlank()) {
            return false;
        }
        if (isContextOverflowError(errorMessage)) {
            return true;
        }
        if (CONTEXT_WINDOW_TOO_SMALL_RE.matcher(errorMessage).find()) {
            return true;
        }
        return CONTEXT_OVERFLOW_HINT_RE.matcher(errorMessage).find();
    }

    // ── Compaction failure ─────────────────────────────────────────────

    private static final Pattern COMPACTION_FAILURE_RE = Pattern.compile(
            "compaction.*fail|failed.*compact|summariz.*fail|compaction error",
            Pattern.CASE_INSENSITIVE);

    public static boolean isCompactionFailureError(String errorMessage) {
        if (errorMessage == null || errorMessage.isBlank()) {
            return false;
        }
        return COMPACTION_FAILURE_RE.matcher(errorMessage).find();
    }

    // ── Rate limit ────────────────────────────────────────────────────

    private static final List<Pattern> RATE_LIMIT_PATTERNS = List.of(
            Pattern.compile("rate[_ ]limit|too many requests|429", Pattern.CASE_INSENSITIVE),
            Pattern.compile("exceeded your current quota", Pattern.CASE_INSENSITIVE),
            Pattern.compile("resource has been exhausted", Pattern.CASE_INSENSITIVE),
            Pattern.compile("quota exceeded", Pattern.CASE_INSENSITIVE),
            Pattern.compile("resource_exhausted", Pattern.CASE_INSENSITIVE),
            Pattern.compile("usage limit", Pattern.CASE_INSENSITIVE));

    public static boolean isRateLimitError(String errorMessage) {
        return matchesAny(errorMessage, RATE_LIMIT_PATTERNS);
    }

    // ── Billing ───────────────────────────────────────────────────────

    private static final List<Pattern> BILLING_PATTERNS = List.of(
            Pattern.compile("billing|payment|subscription|balance|credit", Pattern.CASE_INSENSITIVE),
            Pattern.compile("insufficient_quota|insufficient.funds", Pattern.CASE_INSENSITIVE),
            Pattern.compile("pay.*required|account.*deactivat", Pattern.CASE_INSENSITIVE),
            Pattern.compile("spending limit|budget", Pattern.CASE_INSENSITIVE));

    public static boolean isBillingError(String errorMessage) {
        return matchesAny(errorMessage, BILLING_PATTERNS);
    }

    // ── Auth ──────────────────────────────────────────────────────────

    private static final List<Pattern> AUTH_PATTERNS = List.of(
            Pattern.compile("401|403|authentication|unauthorized|forbidden", Pattern.CASE_INSENSITIVE),
            Pattern.compile("invalid.*api.?key|invalid.*token|api.?key.*invalid", Pattern.CASE_INSENSITIVE),
            Pattern.compile("permission denied|access denied", Pattern.CASE_INSENSITIVE),
            Pattern.compile("invalid_api_key|invalid_auth", Pattern.CASE_INSENSITIVE));

    public static boolean isAuthError(String errorMessage) {
        return matchesAny(errorMessage, AUTH_PATTERNS);
    }

    // ── Timeout ───────────────────────────────────────────────────────

    private static final List<Pattern> TIMEOUT_PATTERNS = List.of(
            Pattern.compile("timeout|timed? out|ETIMEDOUT|ECONNABORTED", Pattern.CASE_INSENSITIVE),
            Pattern.compile("deadline exceeded|request took too long", Pattern.CASE_INSENSITIVE),
            Pattern.compile("524|408|504|gateway.?timeout", Pattern.CASE_INSENSITIVE));

    public static boolean isTimeoutError(String errorMessage) {
        return matchesAny(errorMessage, TIMEOUT_PATTERNS);
    }

    // ── Overloaded ────────────────────────────────────────────────────

    private static final List<Pattern> OVERLOADED_PATTERNS = List.of(
            Pattern.compile("overloaded|503|service unavailable", Pattern.CASE_INSENSITIVE),
            Pattern.compile("capacity|server.*busy|temporarily unavailable", Pattern.CASE_INSENSITIVE),
            Pattern.compile("529|502|bad gateway", Pattern.CASE_INSENSITIVE));

    public static boolean isOverloadedError(String errorMessage) {
        return matchesAny(errorMessage, OVERLOADED_PATTERNS);
    }

    // ── Model not found ───────────────────────────────────────────────

    private static final List<Pattern> MODEL_NOT_FOUND_PATTERNS = List.of(
            Pattern.compile("model.*not found|unknown model|model_not_found", Pattern.CASE_INSENSITIVE),
            Pattern.compile("does not exist|not available", Pattern.CASE_INSENSITIVE),
            Pattern.compile("404.*model|model.*404", Pattern.CASE_INSENSITIVE));

    public static boolean isModelNotFoundError(String errorMessage) {
        return matchesAny(errorMessage, MODEL_NOT_FOUND_PATTERNS);
    }

    // ── Classify failover reason ──────────────────────────────────────

    /**
     * Classify an error message into a {@link FailoverReason}.
     * Order matters — more specific checks first.
     */
    public static FailoverReason classifyFailoverReason(String errorMessage) {
        if (errorMessage == null || errorMessage.isBlank()) {
            return FailoverReason.UNKNOWN;
        }
        if (isContextOverflowError(errorMessage)) {
            return FailoverReason.CONTEXT_OVERFLOW;
        }
        if (isAuthError(errorMessage)) {
            return FailoverReason.AUTH;
        }
        if (isBillingError(errorMessage)) {
            return FailoverReason.BILLING;
        }
        if (isRateLimitError(errorMessage)) {
            return FailoverReason.RATE_LIMIT;
        }
        if (isTimeoutError(errorMessage)) {
            return FailoverReason.TIMEOUT;
        }
        if (isOverloadedError(errorMessage)) {
            return FailoverReason.OVERLOADED;
        }
        if (isModelNotFoundError(errorMessage)) {
            return FailoverReason.MODEL_NOT_FOUND;
        }
        return FailoverReason.UNKNOWN;
    }

    // ── Failover eligibility ──────────────────────────────────────────

    /**
     * Whether the given reason is eligible for automatic auth profile failover.
     */
    public static boolean isFailoverEligible(FailoverReason reason) {
        return switch (reason) {
            case RATE_LIMIT, BILLING, AUTH, TIMEOUT, OVERLOADED -> true;
            case CONTEXT_OVERFLOW, MODEL_NOT_FOUND, UNKNOWN -> false;
        };
    }

    // ── Text sanitization ─────────────────────────────────────────────

    private static final Pattern FINAL_TAG_RE = Pattern.compile("<\\s*/?\\s*final\\s*>", Pattern.CASE_INSENSITIVE);
    private static final Pattern ERROR_PREFIX_RE = Pattern.compile(
            "^(?:error|api\\s*error|apierror|openai\\s*error|anthropic\\s*error|gateway\\s*error)[:\\s-]+",
            Pattern.CASE_INSENSITIVE);

    /**
     * Strip &lt;final&gt; tags from text — some models emit them.
     */
    public static String stripFinalTags(String text) {
        if (text == null) {
            return null;
        }
        return FINAL_TAG_RE.matcher(text).replaceAll("").trim();
    }

    /**
     * Sanitize text for user display:
     * - Strip error prefixes
     * - Strip final tags
     * - Collapse duplicate blocks
     * - Trim whitespace
     */
    public static String sanitizeUserFacingText(String text) {
        if (text == null || text.isBlank()) {
            return text;
        }
        String result = text.trim();

        // Strip error prefixes
        result = ERROR_PREFIX_RE.matcher(result).replaceFirst("");

        // Strip final tags
        result = stripFinalTags(result);

        // Collapse consecutive duplicate lines
        result = collapseConsecutiveDuplicateLines(result);

        return result.trim();
    }

    /**
     * Format an error message for display, classifying and wrapping as needed.
     */
    public static String formatAssistantErrorText(String errorMessage) {
        if (errorMessage == null || errorMessage.isBlank()) {
            return null;
        }

        FailoverReason reason = classifyFailoverReason(errorMessage);

        return switch (reason) {
            case BILLING -> BILLING_ERROR_USER_MESSAGE;
            case AUTH -> "⚠️ Authentication error — check your API key configuration.";
            case RATE_LIMIT -> "⚠️ Rate limit reached — please wait a moment and try again.";
            case TIMEOUT -> "⚠️ Request timed out — the API provider took too long to respond.";
            case OVERLOADED -> "⚠️ API provider is currently overloaded — please try again later.";
            case CONTEXT_OVERFLOW -> "⚠️ Context window overflow — the conversation is too long. "
                    + "Try compacting or starting a new session.";
            case MODEL_NOT_FOUND -> "⚠️ Model not found — check your model configuration.";
            case UNKNOWN -> sanitizeUserFacingText(errorMessage);
        };
    }

    // ── Helpers ───────────────────────────────────────────────────────

    private static boolean matchesAny(String text, List<Pattern> patterns) {
        if (text == null || text.isBlank()) {
            return false;
        }
        for (Pattern p : patterns) {
            if (p.matcher(text).find()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Collapse consecutive duplicate lines in text.
     */
    static String collapseConsecutiveDuplicateLines(String text) {
        if (text == null || text.isBlank()) {
            return text;
        }
        String[] lines = text.split("\n");
        if (lines.length <= 1) {
            return text;
        }
        StringBuilder sb = new StringBuilder();
        String lastLine = null;
        for (String line : lines) {
            String trimmed = line.trim();
            if (trimmed.equals(lastLine)) {
                continue; // skip duplicate
            }
            if (sb.length() > 0) {
                sb.append('\n');
            }
            sb.append(line);
            lastLine = trimmed;
        }
        return sb.toString();
    }
}
