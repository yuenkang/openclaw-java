package com.openclaw.agent.embedded;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * API error classification, parsing, and formatting utilities.
 * Mirrors {@code agents/pi-embedded-helpers/errors.ts}.
 */
public final class EmbeddedErrors {

    private EmbeddedErrors() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper();

    public static final String BILLING_ERROR_USER_MESSAGE = "⚠️ API provider returned a billing error — your API key has run out of credits "
            + "or has an insufficient balance. Check your provider's billing dashboard and top up "
            + "or switch to a different API key.";

    // --- Context overflow detection ---

    private static final Pattern CONTEXT_OVERFLOW_RE = Pattern.compile(
            "maximum context length|max_tokens|too many tokens|"
                    + "maximum.*token.*limit|context length exceeded|"
                    + "this model's maximum|exceeds the model|"
                    + "input is too long|token limit reached|"
                    + "reduce your prompt|context window|"
                    + "max input tokens|total input tokens|"
                    + "exceeded.*content size|string too long",
            Pattern.CASE_INSENSITIVE);

    private static final Pattern LIKELY_OVERFLOW_RE = Pattern.compile(
            "context.*overflow|context window.*(too (?:large|long)|exceed|over|limit|"
                    + "max(?:imum)?|requested|sent|tokens)|"
                    + "(?:prompt|request|input).*(too (?:large|long)|exceed|over|limit|max(?:imum)?)",
            Pattern.CASE_INSENSITIVE);

    public static boolean isContextOverflowError(String message) {
        if (message == null || message.isBlank())
            return false;
        return CONTEXT_OVERFLOW_RE.matcher(message).find();
    }

    public static boolean isLikelyContextOverflowError(String message) {
        if (message == null || message.isBlank())
            return false;
        return LIKELY_OVERFLOW_RE.matcher(message).find();
    }

    public static boolean isCompactionFailureError(String message) {
        if (message == null || message.isBlank())
            return false;
        String lower = message.toLowerCase();
        return lower.contains("compaction failed")
                || lower.contains("compaction error")
                || lower.contains("failed to compact");
    }

    // --- Error pattern matching ---

    private static final Pattern RATE_LIMIT_RE = Pattern.compile(
            "rate[_ ]limit|too many requests|429|"
                    + "exceeded your current quota|resource.has been exhausted|"
                    + "quota exceeded|resource_exhausted|usage limit",
            Pattern.CASE_INSENSITIVE);

    private static final Pattern TIMEOUT_RE = Pattern.compile(
            "timeout|timed out|deadline exceeded|ETIMEDOUT|ESOCKETTIMEDOUT|"
                    + "UND_ERR_CONNECT_TIMEOUT|UND_ERR_HEADERS_TIMEOUT",
            Pattern.CASE_INSENSITIVE);

    private static final Pattern BILLING_RE = Pattern.compile(
            "billing|payment|subscription|insufficient.balance|"
                    + "out of credits|credit.limit|plan.limit|"
                    + "exceeded.*spending.*limit|spending.cap|"
                    + "budget.exceeded|budget.limit",
            Pattern.CASE_INSENSITIVE);

    private static final Pattern AUTH_RE = Pattern.compile(
            "authenticate|unauthorized|invalid.*api.key|invalid.*auth|"
                    + "permission.*denied|forbidden|access.denied|"
                    + "invalid.*token|expired.*token|revoked",
            Pattern.CASE_INSENSITIVE);

    private static final Pattern OVERLOADED_RE = Pattern.compile(
            "overloaded|overload|capacity|503|service.unavailable|"
                    + "temporarily.unavailable|server.*busy|system.*busy",
            Pattern.CASE_INSENSITIVE);

    public static boolean isRateLimitErrorMessage(String raw) {
        return raw != null && RATE_LIMIT_RE.matcher(raw).find();
    }

    public static boolean isTimeoutErrorMessage(String raw) {
        return raw != null && TIMEOUT_RE.matcher(raw).find();
    }

    public static boolean isBillingErrorMessage(String raw) {
        if (raw == null)
            return false;
        if (BILLING_RE.matcher(raw).find())
            return true;
        // Also check if it's a 402 status
        return raw.contains("402") && raw.toLowerCase().contains("payment");
    }

    public static boolean isAuthErrorMessage(String raw) {
        return raw != null && AUTH_RE.matcher(raw).find();
    }

    public static boolean isOverloadedErrorMessage(String raw) {
        return raw != null && OVERLOADED_RE.matcher(raw).find();
    }

    // --- Failover reason classification ---

    /**
     * Classify an error message into a FailoverReason.
     */
    public static EmbeddedTypes.FailoverReason classifyErrorForFailover(String message) {
        if (message == null || message.isBlank())
            return EmbeddedTypes.FailoverReason.UNKNOWN;
        if (isAuthErrorMessage(message))
            return EmbeddedTypes.FailoverReason.AUTH;
        if (isRateLimitErrorMessage(message))
            return EmbeddedTypes.FailoverReason.RATE_LIMIT;
        if (isBillingErrorMessage(message))
            return EmbeddedTypes.FailoverReason.BILLING;
        if (isTimeoutErrorMessage(message))
            return EmbeddedTypes.FailoverReason.TIMEOUT;
        return EmbeddedTypes.FailoverReason.UNKNOWN;
    }

    // --- Error payload parsing ---

    public record ApiErrorInfo(String httpCode, String type, String message, String requestId) {
    }

    /**
     * Parse structured error info from a raw error string.
     */
    public static ApiErrorInfo parseApiErrorInfo(String raw) {
        if (raw == null || raw.isBlank())
            return null;
        String trimmed = raw.trim();
        // Try JSON parsing
        try {
            JsonNode node = MAPPER.readTree(trimmed);
            JsonNode error = node.has("error") ? node.get("error") : node;
            String type = error.has("type") ? error.get("type").asText(null) : null;
            String message = error.has("message") ? error.get("message").asText(null) : null;
            String requestId = null;
            if (node.has("request_id"))
                requestId = node.get("request_id").asText(null);
            else if (node.has("requestId"))
                requestId = node.get("requestId").asText(null);
            String httpCode = error.has("code") ? error.get("code").asText(null) : null;
            if (httpCode == null && node.has("status"))
                httpCode = node.get("status").asText(null);
            if (type != null || message != null) {
                return new ApiErrorInfo(httpCode, type, message, requestId);
            }
        } catch (Exception ignored) {
        }
        return null;
    }

    /**
     * Check if a raw string is a JSON API error payload.
     */
    public static boolean isRawApiErrorPayload(String raw) {
        return parseApiErrorInfo(raw) != null;
    }

    // --- Text sanitization ---

    private static final Pattern FINAL_TAG_RE = Pattern.compile(
            "<\\s*/?\\s*final\\s*>", Pattern.CASE_INSENSITIVE);
    private static final Pattern ERROR_PAYLOAD_PREFIX_RE = Pattern.compile(
            "^(?:error|api\\s*error|apierror|openai\\s*error|anthropic\\s*error|gateway\\s*error)[:\\s-]+",
            Pattern.CASE_INSENSITIVE);

    /**
     * Strip &lt;final&gt; / &lt;/final&gt; tags from text.
     */
    public static String stripFinalTagsFromText(String text) {
        if (text == null)
            return text;
        String cleaned = FINAL_TAG_RE.matcher(text).replaceAll("");
        return cleaned.trim().isEmpty() ? text : cleaned;
    }

    /**
     * Collapse consecutive duplicate paragraph blocks in text.
     */
    public static String collapseConsecutiveDuplicateBlocks(String text) {
        if (text == null || text.length() < 100)
            return text;
        String[] blocks = text.split("\\n{2,}");
        if (blocks.length <= 1)
            return text;
        List<String> result = new ArrayList<>();
        String lastNormalized = null;
        for (String block : blocks) {
            String normalized = block.trim().toLowerCase().replaceAll("\\s+", " ");
            if (normalized.equals(lastNormalized) && normalized.length() >= 20)
                continue;
            result.add(block);
            lastNormalized = normalized;
        }
        return String.join("\n\n", result);
    }

    /**
     * Check if text looks like an HTTP error message.
     */
    public static boolean isLikelyHttpErrorText(String raw) {
        if (raw == null)
            return false;
        String first = raw.trim().split("\\n")[0].trim().toLowerCase();
        List<String> errorStarts = List.of(
                "error", "bad request", "not found", "unauthorized", "forbidden",
                "internal server", "service unavailable", "gateway", "rate limit",
                "overloaded", "timeout", "timed out", "invalid", "too many requests", "permission");
        String lower = first.toLowerCase();
        return errorStarts.stream().anyMatch(lower::startsWith)
                || ERROR_PAYLOAD_PREFIX_RE.matcher(first).find();
    }

    /**
     * Format a raw API error for user-facing display.
     */
    public static String formatRawAssistantErrorForUi(String raw) {
        if (raw == null || raw.isBlank())
            return "An unknown error occurred.";
        ApiErrorInfo info = parseApiErrorInfo(raw);
        if (info != null && info.message() != null) {
            StringBuilder sb = new StringBuilder("⚠️ ");
            if (info.type() != null)
                sb.append(info.type()).append(": ");
            sb.append(info.message());
            if (info.httpCode() != null)
                sb.append(" (").append(info.httpCode()).append(")");
            return sb.toString();
        }
        // Strip common error prefixes
        String cleaned = ERROR_PAYLOAD_PREFIX_RE.matcher(raw.trim()).replaceFirst("");
        if (cleaned.length() > 500)
            cleaned = cleaned.substring(0, 497) + "…";
        return "⚠️ " + cleaned;
    }

    /**
     * Sanitize user-facing text: strip final tags, collapse duplicates, etc.
     */
    public static String sanitizeUserFacingText(String text) {
        if (text == null)
            return text;
        String result = stripFinalTagsFromText(text);
        result = collapseConsecutiveDuplicateBlocks(result);
        return result.trim();
    }

    /**
     * Check if a tool-call input path error.
     */
    private static final Pattern TOOL_CALL_INPUT_PATH_RE = Pattern.compile(
            "messages\\.\\d+\\.content\\.\\d+\\.tool_(?:use|call)\\.(?:input|arguments)",
            Pattern.CASE_INSENSITIVE);

    public static boolean isMissingToolCallInputError(String raw) {
        if (raw == null)
            return false;
        return TOOL_CALL_INPUT_PATH_RE.matcher(raw).find()
                && (raw.toLowerCase().contains("missing") || raw.toLowerCase().contains("required"));
    }
}
