package com.openclaw.common.infra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.Set;

/**
 * Environment variable utilities — logging, normalization, and boolean parsing.
 * Corresponds to TypeScript's infra/env.ts.
 */
public final class EnvUtils {

    private EnvUtils() {
    }

    private static final Logger log = LoggerFactory.getLogger(EnvUtils.class);
    private static final Set<String> loggedKeys = new HashSet<>();
    private static final Set<String> TRUTHY_VALUES = Set.of("1", "true", "yes", "on");
    private static final Set<String> FALSY_VALUES = Set.of("0", "false", "no", "off");

    /**
     * Log an accepted environment variable (only once per key).
     *
     * @param key         env variable name
     * @param description what it does
     * @param redact      whether to redact the value in logs
     */
    public static void logAcceptedEnvOption(String key, String description, boolean redact) {
        if (loggedKeys.contains(key)) {
            return;
        }
        String value = System.getenv(key);
        if (value == null || value.isBlank()) {
            return;
        }
        loggedKeys.add(key);
        String displayValue = redact ? "<redacted>" : formatValue(value);
        log.info("env: {}={} ({})", key, displayValue, description);
    }

    public static void logAcceptedEnvOption(String key, String description) {
        logAcceptedEnvOption(key, description, false);
    }

    private static String formatValue(String value) {
        String singleLine = value.replaceAll("\\s+", " ").trim();
        if (singleLine.length() <= 160) {
            return singleLine;
        }
        return singleLine.substring(0, 160) + "…";
    }

    /**
     * Check if an environment variable value is truthy.
     */
    public static boolean isTruthy(String value) {
        if (value == null || value.isBlank()) {
            return false;
        }
        return TRUTHY_VALUES.contains(value.trim().toLowerCase());
    }

    /**
     * Parse a boolean value (true/false/null for unknown).
     */
    public static Boolean parseBoolean(String value) {
        if (value == null || value.isBlank()) {
            return null;
        }
        String lower = value.trim().toLowerCase();
        if (TRUTHY_VALUES.contains(lower))
            return true;
        if (FALSY_VALUES.contains(lower))
            return false;
        return null;
    }

    /**
     * Get an environment variable with a default.
     */
    public static String getEnv(String key, String defaultValue) {
        String value = System.getenv(key);
        return (value != null && !value.isBlank()) ? value.trim() : defaultValue;
    }

    /**
     * Get an environment variable as boolean.
     */
    public static boolean getEnvBoolean(String key, boolean defaultValue) {
        Boolean parsed = parseBoolean(System.getenv(key));
        return parsed != null ? parsed : defaultValue;
    }

    /**
     * Reset logged keys (for testing).
     */
    public static void resetForTest() {
        loggedKeys.clear();
    }
}
