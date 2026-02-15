package com.openclaw.common.infra;

import java.util.function.BiPredicate;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.regex.Pattern;

/**
 * Retry configuration and channel-specific retry runner factories.
 * Corresponds to TypeScript's infra/retry-policy.ts.
 */
public final class RetryPolicy {

    private RetryPolicy() {
    }

    // ── Discord defaults ──────────────────────────────────────────────

    public static final RetryRunner.Config DISCORD_RETRY_DEFAULTS = new RetryRunner.Config(3, 500, 30_000, 0.1);

    // ── Telegram defaults ─────────────────────────────────────────────

    public static final RetryRunner.Config TELEGRAM_RETRY_DEFAULTS = new RetryRunner.Config(3, 400, 30_000, 0.1);

    private static final Pattern TELEGRAM_RETRY_RE = Pattern.compile(
            "429|timeout|connect|reset|closed|unavailable|temporarily",
            Pattern.CASE_INSENSITIVE);

    /**
     * Extract Telegram retry_after value from an exception, if present.
     * Looks for a "retryAfter" or "retry_after" field via message parsing.
     *
     * @return retry delay in ms, or {@code -1} if not available
     */
    public static long getTelegramRetryAfterMs(Throwable err) {
        // Walk through cause chain looking for retry_after hints in messages
        Throwable current = err;
        while (current != null) {
            String msg = current.getMessage();
            if (msg != null) {
                // Look for patterns like "retry_after: 5" or "retry_after":5
                var matcher = Pattern.compile("retry_after[\":\\s]+(\\d+)").matcher(msg);
                if (matcher.find()) {
                    try {
                        long seconds = Long.parseLong(matcher.group(1));
                        if (seconds > 0 && seconds < 3600) {
                            return seconds * 1000;
                        }
                    } catch (NumberFormatException ignored) {
                    }
                }
            }
            current = current.getCause();
        }
        return -1;
    }

    /**
     * Create a Discord retry runner with rate-limit awareness.
     *
     * @param overrides optional config overrides (may be {@code null})
     * @param verbose   whether to log retry attempts
     * @return configured {@link RetryRunner}
     */
    public static RetryRunner createDiscordRetryRunner(RetryRunner.Config overrides, boolean verbose) {
        RetryRunner.Config resolved = RetryRunner.resolveConfig(DISCORD_RETRY_DEFAULTS, overrides);
        return new RetryRunner(resolved,
                (err, attempt) -> {
                    String msg = err.getMessage();
                    return msg != null && msg.contains("429");
                },
                null,
                verbose ? info -> System.err.printf(
                        "discord %s rate limited, retry %d/%d in %dms%n",
                        info.label() != null ? info.label() : "request",
                        info.attempt(),
                        Math.max(1, info.maxAttempts() - 1),
                        info.delayMs()) : null);
    }

    /**
     * Create a Telegram retry runner with network-error awareness.
     *
     * @param overrides  optional config overrides (may be {@code null})
     * @param verbose    whether to log retry attempts
     * @param extraRetry additional shouldRetry predicate (may be {@code null})
     * @return configured {@link RetryRunner}
     */
    public static RetryRunner createTelegramRetryRunner(
            RetryRunner.Config overrides,
            boolean verbose,
            BiPredicate<Throwable, Integer> extraRetry) {

        RetryRunner.Config resolved = RetryRunner.resolveConfig(TELEGRAM_RETRY_DEFAULTS, overrides);

        BiPredicate<Throwable, Integer> shouldRetry = (err, attempt) -> {
            if (extraRetry != null && extraRetry.test(err, attempt)) {
                return true;
            }
            String msg = formatErrorMessage(err);
            return TELEGRAM_RETRY_RE.matcher(msg).find();
        };

        Function<Throwable, Long> retryAfterMs = err -> getTelegramRetryAfterMs(err);

        Consumer<RetryRunner.RetryInfo> onRetry = verbose ? info -> {
            int maxRetries = Math.max(1, info.maxAttempts() - 1);
            System.err.printf(
                    "telegram send retry %d/%d for %s in %dms: %s%n",
                    info.attempt(), maxRetries,
                    info.label() != null ? info.label() : "request",
                    info.delayMs(),
                    formatErrorMessage(info.err()));
        } : null;

        return new RetryRunner(resolved, shouldRetry, retryAfterMs, onRetry);
    }

    /**
     * Format an error's message chain for logging.
     */
    static String formatErrorMessage(Throwable err) {
        if (err == null) {
            return "unknown error";
        }
        StringBuilder sb = new StringBuilder();
        Throwable current = err;
        while (current != null) {
            if (sb.length() > 0) {
                sb.append(" → ");
            }
            sb.append(current.getMessage() != null ? current.getMessage() : current.getClass().getSimpleName());
            current = current.getCause();
        }
        return sb.toString();
    }
}
