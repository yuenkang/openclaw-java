package com.openclaw.agent.runner;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Miscellaneous runner utilities.
 * Mirrors {@code agents/pi-embedded-runner/utils.ts}.
 */
public final class RunnerUtils {

    private RunnerUtils() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /** Map a ThinkLevel string to the pi-agent-core ThinkingLevel format. */
    public static String mapThinkingLevel(String level) {
        if (level == null || level.isBlank())
            return "off";
        return level;
    }

    /** Describe an unknown error as a human-readable string. */
    public static String describeUnknownError(Throwable error) {
        if (error == null)
            return "Unknown error";
        String message = error.getMessage();
        return (message != null && !message.isBlank()) ? message : "Unknown error";
    }

    /** Describe an arbitrary object as a human-readable error string. */
    public static String describeUnknownError(Object error) {
        if (error == null)
            return "Unknown error";
        if (error instanceof Throwable t)
            return describeUnknownError(t);
        if (error instanceof String s)
            return s;
        try {
            return MAPPER.writeValueAsString(error);
        } catch (Exception e) {
            return "Unknown error";
        }
    }
}
