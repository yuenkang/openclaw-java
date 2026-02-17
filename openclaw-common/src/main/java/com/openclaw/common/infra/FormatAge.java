package com.openclaw.common.infra;

/**
 * Format age/duration values into human-readable strings.
 * Standalone utility extracted from TypeScript's
 * {@code infra/channel-summary.ts#formatAge}.
 */
public class FormatAge {

    /**
     * Format an age in milliseconds into a human-readable string.
     * Examples: "just now", "5m ago", "3h ago", "2d ago"
     */
    public static String formatAge(long ms) {
        if (ms < 0)
            return "unknown";

        long minutes = Math.round(ms / 60_000.0);
        if (minutes < 1)
            return "just now";
        if (minutes < 60)
            return minutes + "m ago";

        long hours = Math.round(minutes / 60.0);
        if (hours < 48)
            return hours + "h ago";

        long days = Math.round(hours / 24.0);
        return days + "d ago";
    }

    /**
     * Format a duration in milliseconds into a compact form.
     * Examples: "1.2s", "45s", "5m 30s", "2h 15m"
     */
    public static String formatDurationCompact(long ms) {
        if (ms < 1000)
            return ms + "ms";
        if (ms < 60_000)
            return String.format("%.1fs", ms / 1000.0);

        long seconds = ms / 1000;
        if (seconds < 3600) {
            long m = seconds / 60;
            long s = seconds % 60;
            return s > 0 ? m + "m " + s + "s" : m + "m";
        }

        long hours = seconds / 3600;
        long mins = (seconds % 3600) / 60;
        return mins > 0 ? hours + "h " + mins + "m" : hours + "h";
    }
}
