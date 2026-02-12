package com.openclaw.agent.runtime.subscribe;

/**
 * Embedded agent facade — re-exports key types and operations from the
 * subscribe subsystem.
 * Corresponds to TypeScript pi-embedded.ts (which only re-exports).
 */
public final class EmbeddedAgent {

    private EmbeddedAgent() {
    }

    // ── Re-exports from RunManager ──────────────────────────────────

    public static boolean queueMessage(String sessionId, String text) {
        return RunManager.queueMessage(sessionId, text);
    }

    public static boolean abort(String sessionId) {
        return RunManager.abort(sessionId);
    }

    public static boolean isRunActive(String sessionId) {
        return RunManager.isRunActive(sessionId);
    }

    public static boolean isRunStreaming(String sessionId) {
        return RunManager.isRunStreaming(sessionId);
    }

    public static java.util.concurrent.CompletableFuture<Boolean> waitForRunEnd(String sessionId) {
        return RunManager.waitForRunEnd(sessionId);
    }

    public static java.util.concurrent.CompletableFuture<Boolean> waitForRunEnd(String sessionId, long timeoutMs) {
        return RunManager.waitForRunEnd(sessionId, timeoutMs);
    }
}
