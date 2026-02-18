package com.openclaw.common.infra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.function.Consumer;

/**
 * Installs an uncaught-exception handler that classifies exceptions and
 * decides whether to crash, log, or ignore.
 * <p>
 * Corresponds to TypeScript's infra/unhandled-rejections.ts.
 *
 * <h3>Exception categories:</h3>
 * <ul>
 * <li><strong>FATAL</strong> — OOM, stack overflow, linkage errors → crash</li>
 * <li><strong>CONFIG</strong> — bad config, missing API key → log &amp;
 * optionally crash</li>
 * <li><strong>TRANSIENT</strong> — network timeout, DNS failure → warn &amp;
 * continue</li>
 * <li><strong>ABORT</strong> — interrupted, cancelled → ignore</li>
 * </ul>
 */
public final class UnhandledExceptions {

    private UnhandledExceptions() {
    }

    private static final Logger log = LoggerFactory.getLogger(UnhandledExceptions.class);
    private static volatile boolean installed = false;
    private static final List<Consumer<Thread.UncaughtExceptionHandler>> customHandlers = new CopyOnWriteArrayList<>();

    public enum Category {
        FATAL,
        CONFIG,
        TRANSIENT,
        ABORT,
        UNKNOWN
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Install the global uncaught-exception handler. Idempotent.
     */
    public static synchronized void install() {
        if (installed) {
            return;
        }
        installed = true;

        Thread.UncaughtExceptionHandler previous = Thread.getDefaultUncaughtExceptionHandler();

        Thread.setDefaultUncaughtExceptionHandler((thread, throwable) -> {
            Category category = classify(throwable);

            switch (category) {
                case FATAL -> {
                    log.error("[openclaw] FATAL uncaught exception on thread [{}]: {}",
                            thread.getName(), throwable.getMessage());
                    logStackTrace(throwable);
                    // Let the JVM terminate
                    if (previous != null) {
                        previous.uncaughtException(thread, throwable);
                    } else {
                        Runtime.getRuntime().halt(1);
                    }
                }
                case CONFIG -> {
                    log.error("[openclaw] Configuration error on thread [{}]: {}",
                            thread.getName(), throwable.getMessage());
                    logStackTrace(throwable);
                }
                case TRANSIENT -> {
                    log.warn("[openclaw] Transient error on thread [{}]: {} ({})",
                            thread.getName(), throwable.getClass().getSimpleName(),
                            throwable.getMessage());
                }
                case ABORT -> {
                    log.debug("[openclaw] Abort/interrupt on thread [{}]: {}",
                            thread.getName(), throwable.getMessage());
                }
                default -> {
                    log.error("[openclaw] Uncaught exception on thread [{}]: {}",
                            thread.getName(), throwable.getMessage());
                    logStackTrace(throwable);
                }
            }
        });

        log.debug("[openclaw] uncaught-exception handler installed");
    }

    /**
     * Classify a throwable into a category.
     */
    public static Category classify(Throwable t) {
        if (t == null) {
            return Category.UNKNOWN;
        }

        // Fatal: OOM, StackOverflow, LinkageError
        if (t instanceof OutOfMemoryError
                || t instanceof StackOverflowError
                || t instanceof LinkageError) {
            return Category.FATAL;
        }

        // Abort: InterruptedException, thread interrupted
        if (t instanceof InterruptedException) {
            return Category.ABORT;
        }

        String message = messageChain(t).toLowerCase();
        String className = t.getClass().getName().toLowerCase();

        // Abort patterns
        if (message.contains("interrupted")
                || message.contains("cancelled")
                || message.contains("abort")
                || className.contains("cancel")
                || className.contains("abort")) {
            return Category.ABORT;
        }

        // Config patterns
        if (message.contains("api key") || message.contains("apikey")
                || message.contains("invalid config")
                || message.contains("configuration error")
                || message.contains("missing required")
                || message.contains("authentication")
                || message.contains("unauthorized")
                || message.contains("401")) {
            return Category.CONFIG;
        }

        // Transient network patterns
        if (t instanceof java.net.SocketTimeoutException
                || t instanceof java.net.ConnectException
                || t instanceof java.net.UnknownHostException
                || t instanceof java.net.NoRouteToHostException
                || t instanceof javax.net.ssl.SSLException) {
            return Category.TRANSIENT;
        }
        if (message.contains("timeout")
                || message.contains("timed out")
                || message.contains("connection refused")
                || message.contains("connection reset")
                || message.contains("dns")
                || message.contains("econnrefused")
                || message.contains("econnreset")
                || message.contains("etimedout")
                || message.contains("enotfound")
                || message.contains("rate limit")
                || message.contains("too many requests")
                || message.contains("429")
                || message.contains("503")
                || message.contains("502")) {
            return Category.TRANSIENT;
        }

        return Category.UNKNOWN;
    }

    /**
     * Check if the handler has been installed.
     */
    public static boolean isInstalled() {
        return installed;
    }

    // =========================================================================
    // Internals
    // =========================================================================

    /**
     * Build a message string from the full cause chain.
     */
    static String messageChain(Throwable t) {
        StringBuilder sb = new StringBuilder();
        Throwable current = t;
        int depth = 0;
        while (current != null && depth < 10) {
            if (!sb.isEmpty()) {
                sb.append(" -> ");
            }
            sb.append(current.getClass().getSimpleName());
            if (current.getMessage() != null) {
                sb.append(": ").append(current.getMessage());
            }
            current = current.getCause();
            depth++;
        }
        return sb.toString();
    }

    private static void logStackTrace(Throwable t) {
        StringWriter sw = new StringWriter();
        t.printStackTrace(new PrintWriter(sw));
        log.error("{}", sw);
    }

    /**
     * Reset state for testing.
     */
    public static void resetForTest() {
        installed = false;
        Thread.setDefaultUncaughtExceptionHandler(null);
        customHandlers.clear();
    }
}
