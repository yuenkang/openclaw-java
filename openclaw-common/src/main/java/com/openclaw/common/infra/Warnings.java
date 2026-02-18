package com.openclaw.common.infra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Collects startup warnings and flushes them in a single batch once the gateway
 * is ready. Prevents interleaving of warning output with other boot messages.
 * <p>
 * Corresponds to TypeScript's infra/warnings.ts (process warning filter) plus
 * a startup-warning accumulator pattern used elsewhere in the TS codebase.
 */
public final class Warnings {

    private Warnings() {
    }

    private static final Logger log = LoggerFactory.getLogger(Warnings.class);
    private static final List<String> pendingWarnings = Collections.synchronizedList(new ArrayList<>());
    private static volatile boolean flushed = false;

    /**
     * Record a warning to be displayed later during startup completion.
     */
    public static void addWarning(String message) {
        if (message == null || message.isBlank()) {
            return;
        }
        if (flushed) {
            // Post-flush: log immediately
            log.warn("[openclaw] {}", message);
            return;
        }
        pendingWarnings.add(message.trim());
    }

    /**
     * Flush all accumulated warnings. Called once when the gateway is ready.
     */
    public static void flushWarnings() {
        flushed = true;
        if (pendingWarnings.isEmpty()) {
            return;
        }
        List<String> snapshot;
        synchronized (pendingWarnings) {
            snapshot = new ArrayList<>(pendingWarnings);
            pendingWarnings.clear();
        }
        for (String warning : snapshot) {
            log.warn("[openclaw] {}", warning);
        }
    }

    /**
     * Check if there are pending warnings.
     */
    public static boolean hasPendingWarnings() {
        return !pendingWarnings.isEmpty();
    }

    /**
     * Get the number of pending warnings.
     */
    public static int pendingCount() {
        return pendingWarnings.size();
    }

    /**
     * Reset state for testing.
     */
    public static void resetForTest() {
        pendingWarnings.clear();
        flushed = false;
    }
}
