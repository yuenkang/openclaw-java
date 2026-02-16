package com.openclaw.common.logging;

import java.util.Map;

/**
 * Log level enumeration matching the TS logging/levels.ts definitions.
 * Provides normalization and SLF4J level mapping.
 *
 */
public enum LogLevel {
    SILENT,
    FATAL,
    ERROR,
    WARN,
    INFO,
    DEBUG,
    TRACE;

    private static final Map<String, LogLevel> ALIASES = Map.ofEntries(
            Map.entry("silent", SILENT),
            Map.entry("fatal", FATAL),
            Map.entry("error", ERROR),
            Map.entry("warn", WARN),
            Map.entry("warning", WARN),
            Map.entry("info", INFO),
            Map.entry("debug", DEBUG),
            Map.entry("trace", TRACE));

    /**
     * Normalize an arbitrary string to a LogLevel, falling back to the given
     * default.
     */
    public static LogLevel normalize(String level, LogLevel fallback) {
        if (level == null || level.isBlank()) {
            return fallback;
        }
        LogLevel resolved = ALIASES.get(level.trim().toLowerCase());
        return resolved != null ? resolved : fallback;
    }

    /**
     * Normalize with default fallback of INFO.
     */
    public static LogLevel normalize(String level) {
        return normalize(level, INFO);
    }

    /**
     * Convert to SLF4J level string for Logback configuration.
     */
    public String toSlf4jLevel() {
        return switch (this) {
            case SILENT -> "OFF";
            case FATAL -> "ERROR";
            default -> name();
        };
    }

    /**
     * Returns a numeric priority (lower = more severe), matching tslog ordering.
     * fatal=0, error=1, warn=2, info=3, debug=4, trace=5, silent=MAX.
     */
    public int priority() {
        return switch (this) {
            case FATAL -> 0;
            case ERROR -> 1;
            case WARN -> 2;
            case INFO -> 3;
            case DEBUG -> 4;
            case TRACE -> 5;
            case SILENT -> Integer.MAX_VALUE;
        };
    }

    /**
     * Check if this level is enabled given a configured minimum level.
     * A message at level X is enabled if X.priority() <= minLevel.priority().
     */
    public boolean isEnabledFor(LogLevel minLevel) {
        if (minLevel == SILENT) {
            return false;
        }
        return this.priority() <= minLevel.priority();
    }
}
