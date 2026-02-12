package com.openclaw.agent.runner;

/**
 * Session/global lane resolution for embedded agent runs.
 * Mirrors {@code agents/pi-embedded-runner/lanes.ts}.
 */
public final class RunnerLanes {

    private RunnerLanes() {
    }

    /** Default lane name when no lane is specified. */
    public static final String MAIN_LANE = "main";

    /** Resolve a session-scoped lane key. */
    public static String resolveSessionLane(String key) {
        String cleaned = (key == null || key.isBlank()) ? MAIN_LANE : key.trim();
        return cleaned.startsWith("session:") ? cleaned : "session:" + cleaned;
    }

    /** Resolve a global lane key. */
    public static String resolveGlobalLane(String lane) {
        String cleaned = (lane == null) ? null : lane.trim();
        return (cleaned == null || cleaned.isEmpty()) ? MAIN_LANE : cleaned;
    }

    /** Resolve an embedded session lane (alias for resolveSessionLane). */
    public static String resolveEmbeddedSessionLane(String key) {
        return resolveSessionLane(key);
    }
}
