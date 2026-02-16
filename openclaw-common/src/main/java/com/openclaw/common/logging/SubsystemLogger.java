package com.openclaw.common.logging;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Subsystem-aware logger that wraps SLF4J and adds structured subsystem
 * context.
 * Translates TS logging/subsystem.ts — createSubsystemLogger().
 *
 * <p>
 * Usage:
 * 
 * <pre>
 * SubsystemLogger log = SubsystemLogger.create("gateway/session");
 * log.info("Session opened", Map.of("sessionId", "abc123"));
 * SubsystemLogger child = log.child("cleanup");
 * child.debug("Pruning expired entries");
 * </pre>
 *
 */
public class SubsystemLogger {

    private static final String MDC_SUBSYSTEM = "subsystem";
    private static final List<String> subsystemFilters = new CopyOnWriteArrayList<>();

    private final String subsystem;
    private final Logger logger;

    private SubsystemLogger(String subsystem) {
        this.subsystem = subsystem;
        // Use subsystem as the SLF4J logger name for per-subsystem control in
        // logback.xml
        this.logger = LoggerFactory.getLogger("openclaw." + subsystem);
    }

    /**
     * Create a subsystem logger.
     */
    public static SubsystemLogger create(String subsystem) {
        return new SubsystemLogger(subsystem);
    }

    /**
     * Create a child logger with extended subsystem path.
     */
    public SubsystemLogger child(String name) {
        return new SubsystemLogger(subsystem + "/" + name);
    }

    // -----------------------------------------------------------------------
    // Log methods
    // -----------------------------------------------------------------------

    public void trace(String message) {
        trace(message, null);
    }

    public void trace(String message, Map<String, Object> meta) {
        emit(LogLevel.TRACE, message, meta);
    }

    public void debug(String message) {
        debug(message, null);
    }

    public void debug(String message, Map<String, Object> meta) {
        emit(LogLevel.DEBUG, message, meta);
    }

    public void info(String message) {
        info(message, null);
    }

    public void info(String message, Map<String, Object> meta) {
        emit(LogLevel.INFO, message, meta);
    }

    public void warn(String message) {
        warn(message, null);
    }

    public void warn(String message, Map<String, Object> meta) {
        emit(LogLevel.WARN, message, meta);
    }

    public void error(String message) {
        emit(LogLevel.ERROR, message, null);
    }

    public void error(String message, Map<String, Object> meta) {
        emit(LogLevel.ERROR, message, meta);
    }

    public void error(String message, Throwable t) {
        if (!shouldLog())
            return;
        try {
            MDC.put(MDC_SUBSYSTEM, subsystem);
            logger.error(formatMessage(message, null), t);
        } finally {
            MDC.remove(MDC_SUBSYSTEM);
        }
    }

    public void fatal(String message) {
        fatal(message, null);
    }

    public void fatal(String message, Map<String, Object> meta) {
        emit(LogLevel.FATAL, message, meta);
    }

    // -----------------------------------------------------------------------
    // Subsystem filter (console-side only in TS; here it controls all output)
    // -----------------------------------------------------------------------

    /**
     * Set subsystem filters. Only subsystems matching one of the prefixes will log.
     * Pass null or empty to clear filters (all subsystems log).
     */
    public static void setSubsystemFilter(String... filters) {
        subsystemFilters.clear();
        if (filters != null) {
            Arrays.stream(filters)
                    .map(String::trim)
                    .filter(s -> !s.isEmpty())
                    .forEach(subsystemFilters::add);
        }
    }

    /**
     * Check if this subsystem should log based on current filters.
     */
    public boolean shouldLog() {
        if (subsystemFilters.isEmpty()) {
            return true;
        }
        return subsystemFilters.stream().anyMatch(
                prefix -> subsystem.equals(prefix) || subsystem.startsWith(prefix + "/"));
    }

    // -----------------------------------------------------------------------
    // Accessors
    // -----------------------------------------------------------------------

    public String getSubsystem() {
        return subsystem;
    }

    public Logger getSlf4jLogger() {
        return logger;
    }

    // -----------------------------------------------------------------------
    // Internals
    // -----------------------------------------------------------------------

    private void emit(LogLevel level, String message, Map<String, Object> meta) {
        if (!shouldLog())
            return;
        try {
            MDC.put(MDC_SUBSYSTEM, subsystem);
            String formatted = formatMessage(message, meta);
            switch (level) {
                case TRACE -> logger.trace(formatted);
                case DEBUG -> logger.debug(formatted);
                case INFO -> logger.info(formatted);
                case WARN -> logger.warn(formatted);
                case ERROR, FATAL -> logger.error(formatted);
                default -> logger.info(formatted);
            }
        } finally {
            MDC.remove(MDC_SUBSYSTEM);
        }
    }

    private String formatMessage(String message, Map<String, Object> meta) {
        if (meta == null || meta.isEmpty()) {
            return "[" + subsystem + "] " + message;
        }
        StringBuilder sb = new StringBuilder();
        sb.append("[").append(subsystem).append("] ").append(message);
        sb.append(" {");
        boolean first = true;
        for (var entry : meta.entrySet()) {
            if (!first)
                sb.append(", ");
            sb.append(entry.getKey()).append("=").append(entry.getValue());
            first = false;
        }
        sb.append("}");
        return sb.toString();
    }
}
