package com.openclaw.agent.runtime;

/**
 * Abort/cancellation detection utilities.
 * Corresponds to TypeScript pi-embedded-runner/abort.ts.
 */
public final class AbortUtils {

    private AbortUtils() {
    }

    /**
     * Check if an exception represents an abort/cancellation.
     */
    public static boolean isAbortError(Throwable err) {
        if (err == null)
            return false;

        // Check by class name
        String className = err.getClass().getSimpleName();
        if ("AbortError".equals(className) || err instanceof InterruptedException) {
            return true;
        }

        // Check by message
        String message = err.getMessage();
        if (message != null && message.toLowerCase().contains("aborted")) {
            return true;
        }

        // Check cause chain
        Throwable cause = err.getCause();
        if (cause != null && cause != err) {
            return isAbortError(cause);
        }

        return false;
    }
}
