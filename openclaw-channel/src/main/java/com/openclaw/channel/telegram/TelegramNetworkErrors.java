package com.openclaw.channel.telegram;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashSet;
import java.util.Set;

/**
 * Telegram network error classification for retry decisions.
 * Corresponds to TypeScript's telegram/network-errors.ts.
 */
public final class TelegramNetworkErrors {

    private TelegramNetworkErrors() {
    }

    private static final Set<String> RECOVERABLE_ERROR_CODES = Set.of(
            "ECONNRESET", "ECONNREFUSED", "EPIPE", "ETIMEDOUT",
            "ESOCKETTIMEDOUT", "ENETUNREACH", "EHOSTUNREACH", "ENOTFOUND",
            "EAI_AGAIN", "UND_ERR_CONNECT_TIMEOUT", "UND_ERR_HEADERS_TIMEOUT",
            "UND_ERR_BODY_TIMEOUT", "UND_ERR_SOCKET", "UND_ERR_ABORTED",
            "ECONNABORTED", "ERR_NETWORK");

    private static final Set<String> RECOVERABLE_ERROR_NAMES = Set.of(
            "AbortError", "TimeoutError", "ConnectTimeoutError",
            "HeadersTimeoutError", "BodyTimeoutError");

    private static final String[] RECOVERABLE_MESSAGE_SNIPPETS = {
            "fetch failed", "typeerror: fetch failed", "undici",
            "network error", "network request",
            "client network socket disconnected", "socket hang up",
            "getaddrinfo", "timeout", "timed out"
    };

    /** Network error context: where the error occurred. */
    public enum Context {
        POLLING, SEND, WEBHOOK, UNKNOWN
    }

    /**
     * Check whether the given exception chain represents a recoverable network
     * error.
     *
     * @param err               the root exception
     * @param context           where the error occurred
     * @param allowMessageMatch whether to match by error message text
     *                          (defaults to true unless context is SEND)
     * @return true if the error is likely recoverable (transient network issue)
     */
    public static boolean isRecoverable(Throwable err, Context context, Boolean allowMessageMatch) {
        if (err == null)
            return false;
        boolean matchMessage = allowMessageMatch != null ? allowMessageMatch : context != Context.SEND;

        Deque<Throwable> queue = new ArrayDeque<>();
        Set<Throwable> seen = new HashSet<>();
        queue.add(err);

        while (!queue.isEmpty()) {
            Throwable current = queue.poll();
            if (current == null || !seen.add(current))
                continue;

            // Check by class simple name
            String name = current.getClass().getSimpleName();
            if (RECOVERABLE_ERROR_NAMES.contains(name))
                return true;

            // Check by message for error codes
            String msg = current.getMessage();
            if (msg != null) {
                String upperMsg = msg.toUpperCase();
                for (String code : RECOVERABLE_ERROR_CODES) {
                    if (upperMsg.contains(code))
                        return true;
                }
                if (matchMessage) {
                    String lowerMsg = msg.toLowerCase();
                    for (String snippet : RECOVERABLE_MESSAGE_SNIPPETS) {
                        if (lowerMsg.contains(snippet))
                            return true;
                    }
                }
            }

            // Walk the cause chain
            if (current.getCause() != null && !seen.contains(current.getCause())) {
                queue.add(current.getCause());
            }
        }
        return false;
    }

    public static boolean isRecoverable(Throwable err) {
        return isRecoverable(err, Context.UNKNOWN, null);
    }
}
