package com.openclaw.common.infra;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Runtime environment guard — version detection and validation.
 * In Java this validates the JVM version instead of Node.js.
 * Corresponds to TypeScript's infra/runtime-guard.ts.
 */
public final class RuntimeGuard {

    private RuntimeGuard() {
    }

    /** Minimum required Java version. */
    public static final int MIN_JAVA_MAJOR = 17;

    private static final Pattern SEMVER_RE = Pattern.compile("(\\d+)\\.(\\d+)\\.(\\d+)");

    // ── Semver ──────────────────────────────────────────────────────────

    public record Semver(int major, int minor, int patch) {
        @Override
        public String toString() {
            return major + "." + minor + "." + patch;
        }
    }

    /**
     * Parse a semver string like "17.0.12" or "22.12.0" into components.
     * Returns null if invalid.
     */
    public static Semver parseSemver(String version) {
        if (version == null || version.isEmpty()) {
            return null;
        }
        Matcher m = SEMVER_RE.matcher(version);
        if (!m.find()) {
            return null;
        }
        return new Semver(
                Integer.parseInt(m.group(1)),
                Integer.parseInt(m.group(2)),
                Integer.parseInt(m.group(3)));
    }

    /**
     * Check if version >= minimum.
     */
    public static boolean isAtLeast(Semver version, Semver minimum) {
        if (version == null)
            return false;
        if (version.major != minimum.major)
            return version.major > minimum.major;
        if (version.minor != minimum.minor)
            return version.minor > minimum.minor;
        return version.patch >= minimum.patch;
    }

    // ── Runtime detection ───────────────────────────────────────────────

    /**
     * Runtime environment details.
     */
    public record Details(String javaVersion, String javaVendor, String javaHome, String osName) {
    }

    /**
     * Detect the current Java runtime environment.
     */
    public static Details detectRuntime() {
        return new Details(
                System.getProperty("java.version", "unknown"),
                System.getProperty("java.vendor", "unknown"),
                System.getProperty("java.home", "unknown"),
                System.getProperty("os.name", "unknown"));
    }

    /**
     * Check if the Java runtime version satisfies the minimum requirement.
     */
    public static boolean runtimeSatisfies() {
        return runtimeSatisfies(detectRuntime());
    }

    public static boolean runtimeSatisfies(Details details) {
        try {
            int featureVersion = Runtime.version().feature();
            return featureVersion >= MIN_JAVA_MAJOR;
        } catch (Exception e) {
            // Fallback: parse java.version string
            Semver parsed = parseSemver(details.javaVersion());
            return parsed != null && parsed.major >= MIN_JAVA_MAJOR;
        }
    }

    /**
     * Assert that the current runtime satisfies minimum requirements.
     * Throws {@link UnsupportedOperationException} if not.
     */
    public static void assertSupportedRuntime() {
        Details details = detectRuntime();
        if (runtimeSatisfies(details)) {
            return;
        }
        throw new UnsupportedOperationException(String.format(
                "OpenClaw requires Java >= %d. Detected: %s (%s). JAVA_HOME: %s",
                MIN_JAVA_MAJOR,
                details.javaVersion(),
                details.javaVendor(),
                details.javaHome()));
    }
}
