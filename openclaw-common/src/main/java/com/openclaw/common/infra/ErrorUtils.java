package com.openclaw.common.infra;

/**
 * Error formatting utilities — safely extract messages and codes from
 * exceptions.
 * Corresponds to TypeScript's infra/errors.ts.
 */
public final class ErrorUtils {

    private ErrorUtils() {
    }

    /**
     * Extract an error code from a throwable (checks for HTTP status or custom
     * codes).
     *
     * @return the code as a string, or null
     */
    public static String extractErrorCode(Throwable err) {
        if (err == null)
            return null;

        // Check for "INVALID_CONFIG" pattern in message
        String message = err.getMessage();
        if (message != null && message.contains("INVALID_CONFIG")) {
            return "INVALID_CONFIG";
        }

        return null;
    }

    /**
     * Format an exception message safely.
     *
     * @return a non-null human-readable error string
     */
    public static String formatErrorMessage(Throwable err) {
        if (err == null)
            return "Error";
        String msg = err.getMessage();
        if (msg != null && !msg.isEmpty()) {
            return msg;
        }
        return err.getClass().getSimpleName();
    }

    /**
     * Format an uncaught error with stack trace for INVALID_CONFIG, otherwise
     * message only.
     */
    public static String formatUncaughtError(Throwable err) {
        if (err == null)
            return "Error";
        if ("INVALID_CONFIG".equals(extractErrorCode(err))) {
            return formatErrorMessage(err);
        }
        // Include stack trace for unexpected errors
        StringBuilder sb = new StringBuilder();
        sb.append(formatErrorMessage(err));
        StackTraceElement[] stack = err.getStackTrace();
        if (stack != null && stack.length > 0) {
            sb.append("\n");
            for (int i = 0; i < Math.min(stack.length, 10); i++) {
                sb.append("    at ").append(stack[i]).append("\n");
            }
        }
        return sb.toString();
    }

    /**
     * Format any object as an error message (for non-Throwable cases).
     */
    public static String formatErrorMessage(Object err) {
        if (err == null)
            return "Error";
        if (err instanceof Throwable t)
            return formatErrorMessage(t);
        return err.toString();
    }
}
