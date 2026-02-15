package com.openclaw.common.infra;

import java.util.regex.Pattern;

/**
 * Command/executable safety validation — checks if a value is safe to use
 * as an executable path or argument without shell injection risks.
 * Corresponds to TypeScript's infra/exec-safety.ts.
 */
public final class ExecSafety {

    private ExecSafety() {
    }

    private static final Pattern SHELL_METACHARS = Pattern.compile("[;&|`$<>]");
    private static final Pattern CONTROL_CHARS = Pattern.compile("[\\r\\n]");
    private static final Pattern QUOTE_CHARS = Pattern.compile("[\"']");
    private static final Pattern BARE_NAME = Pattern.compile("^[A-Za-z0-9._+\\-]+$");
    private static final Pattern WINDOWS_PATH_START = Pattern.compile("^[A-Za-z]:[\\\\/]");

    /**
     * Check if a value looks like a file path.
     */
    static boolean isLikelyPath(String value) {
        if (value.startsWith(".") || value.startsWith("~")) {
            return true;
        }
        if (value.contains("/") || value.contains("\\")) {
            return true;
        }
        return WINDOWS_PATH_START.matcher(value).find();
    }

    /**
     * Validate that a string is safe to use as an executable name or path.
     * Rejects shell metacharacters, control characters, and flags (leading "-").
     * Allows bare names (alphanumeric + ._+-) and file paths.
     *
     * @param value the value to check (nullable)
     * @return {@code true} if safe
     */
    public static boolean isSafeExecutableValue(String value) {
        if (value == null) {
            return false;
        }
        String trimmed = value.trim();
        if (trimmed.isEmpty()) {
            return false;
        }
        if (trimmed.contains("\0")) {
            return false;
        }
        if (CONTROL_CHARS.matcher(trimmed).find()) {
            return false;
        }
        if (SHELL_METACHARS.matcher(trimmed).find()) {
            return false;
        }
        if (QUOTE_CHARS.matcher(trimmed).find()) {
            return false;
        }
        if (isLikelyPath(trimmed)) {
            return true;
        }
        if (trimmed.startsWith("-")) {
            return false;
        }
        return BARE_NAME.matcher(trimmed).matches();
    }
}
