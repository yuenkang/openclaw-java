package com.openclaw.agent.runner;

/**
 * Abort/cancel error detection.
 * Mirrors {@code agents/pi-embedded-runner/abort.ts}.
 */
public final class AbortDetector {

    private AbortDetector() {
    }

    /**
     * Check if an error represents an abort/cancellation.
     */
    public static boolean isAbortError(Throwable err) {
        if (err == null)
            return false;
        String name = err.getClass().getSimpleName();
        if ("AbortError".equals(name) || err instanceof java.util.concurrent.CancellationException) {
            return true;
        }
        String message = err.getMessage();
        return message != null && message.toLowerCase().contains("aborted");
    }

    /**
     * Check if an error object (possibly not a Throwable) represents an abort.
     */
    public static boolean isAbortError(Object err) {
        if (err == null)
            return false;
        if (err instanceof Throwable t)
            return isAbortError(t);
        if (err instanceof String s)
            return s.toLowerCase().contains("aborted");
        return false;
    }
}
