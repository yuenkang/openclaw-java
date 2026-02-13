package com.openclaw.common.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Config path formatting and update logging utilities.
 * Corresponds to TypeScript's logging.ts.
 */
public final class ConfigLogging {

    private static final Logger log = LoggerFactory.getLogger(ConfigLogging.class);

    private ConfigLogging() {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Format a config path for display (abbreviate home directory with ~).
     */
    public static String formatConfigPath(String path) {
        if (path == null || path.isEmpty()) {
            path = ConfigPaths.resolveCanonicalConfigPath().toString();
        }
        return displayPath(path);
    }

    /**
     * Log that the configuration was updated.
     */
    public static void logConfigUpdated(String path, String suffix) {
        String formatted = formatConfigPath(path);
        String msg = "Updated " + formatted;
        if (suffix != null && !suffix.isEmpty()) {
            msg += " " + suffix;
        }
        log.info(msg);
    }

    /**
     * Log that the configuration was updated (no suffix).
     */
    public static void logConfigUpdated(String path) {
        logConfigUpdated(path, null);
    }

    /**
     * Log that the configuration was updated (default path).
     */
    public static void logConfigUpdated() {
        logConfigUpdated(null, null);
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    /**
     * Abbreviate the home directory in a path with ~.
     */
    private static String displayPath(String path) {
        String home = System.getProperty("user.home");
        if (home != null && path.startsWith(home)) {
            return "~" + path.substring(home.length());
        }
        return path;
    }
}
