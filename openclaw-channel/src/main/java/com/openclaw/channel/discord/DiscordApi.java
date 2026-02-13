package com.openclaw.channel.discord;

/**
 * Discord REST API client with retry and rate-limit handling.
 * Corresponds to TypeScript's discord/api.ts.
 */
public final class DiscordApi {

    private DiscordApi() {
    }

    public static final String API_BASE = "https://discord.com/api/v10";

    /** Default retry parameters for Discord API calls. */
    public static final int DEFAULT_RETRY_ATTEMPTS = 3;
    public static final int DEFAULT_MIN_DELAY_MS = 500;
    public static final int DEFAULT_MAX_DELAY_MS = 30_000;
    public static final double DEFAULT_JITTER = 0.1;

    // =========================================================================
    // Error type
    // =========================================================================

    public static class ApiError extends RuntimeException {
        private final int status;
        private final Double retryAfterSeconds;

        public ApiError(String message, int status, Double retryAfterSeconds) {
            super(message);
            this.status = status;
            this.retryAfterSeconds = retryAfterSeconds;
        }

        public int getStatus() {
            return status;
        }

        public Double getRetryAfterSeconds() {
            return retryAfterSeconds;
        }
    }

    // =========================================================================
    // Error text parsing
    // =========================================================================

    /**
     * Format a Discord API error JSON body into a human-readable message.
     * Extracts "message" and optional "retry_after" from the response payload.
     */
    public static String formatErrorText(String responseBody) {
        if (responseBody == null)
            return null;
        String trimmed = responseBody.trim();
        if (trimmed.isEmpty())
            return null;
        if (!trimmed.startsWith("{") || !trimmed.endsWith("}")) {
            return trimmed;
        }
        // Simple JSON extraction without a full parser
        String message = extractJsonString(trimmed, "message");
        String retryAfterStr = extractJsonNumber(trimmed, "retry_after");
        String msg = message != null && !message.isEmpty() ? message : "unknown error";
        if (retryAfterStr != null) {
            try {
                double seconds = Double.parseDouble(retryAfterStr);
                String formatted = seconds < 10 ? String.format("%.1fs", seconds)
                        : Math.round(seconds) + "s";
                return msg + " (retry after " + formatted + ")";
            } catch (NumberFormatException ignored) {
            }
        }
        return msg;
    }

    /**
     * Check if an API error is rate-limited (HTTP 429) and should be retried.
     */
    public static boolean isRateLimited(Throwable err) {
        return err instanceof ApiError && ((ApiError) err).getStatus() == 429;
    }

    // =========================================================================
    // Internal JSON helpers (simple extraction without full parser for lightweight
    // use)
    // =========================================================================

    static String extractJsonString(String json, String key) {
        String pattern = "\"" + key + "\"";
        int idx = json.indexOf(pattern);
        if (idx < 0)
            return null;
        int colonIdx = json.indexOf(':', idx + pattern.length());
        if (colonIdx < 0)
            return null;
        int quoteStart = json.indexOf('"', colonIdx + 1);
        if (quoteStart < 0)
            return null;
        int quoteEnd = json.indexOf('"', quoteStart + 1);
        if (quoteEnd < 0)
            return null;
        return json.substring(quoteStart + 1, quoteEnd);
    }

    static String extractJsonNumber(String json, String key) {
        String pattern = "\"" + key + "\"";
        int idx = json.indexOf(pattern);
        if (idx < 0)
            return null;
        int colonIdx = json.indexOf(':', idx + pattern.length());
        if (colonIdx < 0)
            return null;
        int start = colonIdx + 1;
        while (start < json.length() && json.charAt(start) == ' ')
            start++;
        if (start >= json.length())
            return null;
        int end = start;
        while (end < json.length() && (Character.isDigit(json.charAt(end))
                || json.charAt(end) == '.' || json.charAt(end) == '-'))
            end++;
        if (end == start)
            return null;
        return json.substring(start, end);
    }
}
