package com.openclaw.agent.runtime;

/**
 * Lane management for concurrent session command routing.
 * Corresponds to TypeScript pi-embedded-runner/lanes.ts.
 */
public final class Lanes {

    public static final String MAIN = "main";

    private Lanes() {
    }

    /**
     * Resolve a session-scoped lane from a session key.
     */
    public static String resolveSessionLane(String key) {
        String cleaned = (key == null || key.isBlank()) ? MAIN : key.trim();
        return cleaned.startsWith("session:") ? cleaned : "session:" + cleaned;
    }

    /**
     * Resolve a global lane (falls back to {@link #MAIN}).
     */
    public static String resolveGlobalLane(String lane) {
        String cleaned = (lane == null) ? null : lane.trim();
        return (cleaned == null || cleaned.isEmpty()) ? MAIN : cleaned;
    }

    /**
     * Resolve an embedded session lane (alias for {@link #resolveSessionLane}).
     */
    public static String resolveEmbeddedSessionLane(String key) {
        return resolveSessionLane(key);
    }
}
