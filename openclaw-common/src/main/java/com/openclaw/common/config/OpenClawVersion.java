package com.openclaw.common.config;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * OpenClaw version parsing and comparison utilities.
 * Corresponds to TypeScript's version.ts.
 */
public final class OpenClawVersion {

    private OpenClawVersion() {
    }

    private static final Pattern VERSION_RE = Pattern.compile("^v?(\\d+)\\.(\\d+)\\.(\\d+)(?:-(\\d+))?");

    // =========================================================================
    // Types
    // =========================================================================

    /**
     * Parsed version with major, minor, patch, and revision components.
     */
    public record Version(int major, int minor, int patch, int revision) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Parse a version string into its components.
     *
     * @param raw version string like "v1.2.3" or "1.2.3-4"
     * @return parsed version or null if invalid
     */
    public static Version parse(String raw) {
        if (raw == null || raw.isBlank()) {
            return null;
        }
        Matcher matcher = VERSION_RE.matcher(raw.trim());
        if (!matcher.find()) {
            return null;
        }
        int major = Integer.parseInt(matcher.group(1));
        int minor = Integer.parseInt(matcher.group(2));
        int patch = Integer.parseInt(matcher.group(3));
        int revision = matcher.group(4) != null ? Integer.parseInt(matcher.group(4)) : 0;
        return new Version(major, minor, patch, revision);
    }

    /**
     * Compare two version strings.
     *
     * @return negative if a < b, positive if a > b, 0 if equal, null if either is
     *         invalid
     */
    public static Integer compare(String a, String b) {
        Version parsedA = parse(a);
        Version parsedB = parse(b);
        if (parsedA == null || parsedB == null) {
            return null;
        }
        if (parsedA.major != parsedB.major) {
            return parsedA.major < parsedB.major ? -1 : 1;
        }
        if (parsedA.minor != parsedB.minor) {
            return parsedA.minor < parsedB.minor ? -1 : 1;
        }
        if (parsedA.patch != parsedB.patch) {
            return parsedA.patch < parsedB.patch ? -1 : 1;
        }
        if (parsedA.revision != parsedB.revision) {
            return parsedA.revision < parsedB.revision ? -1 : 1;
        }
        return 0;
    }
}
