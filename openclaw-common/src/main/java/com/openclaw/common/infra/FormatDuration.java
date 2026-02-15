package com.openclaw.common.infra;

/**
 * Duration formatting utilities.
 * Corresponds to TypeScript's infra/format-duration.ts.
 */
public final class FormatDuration {

    private FormatDuration() {
    }

    /**
     * Format milliseconds as seconds with configurable decimals.
     *
     * @param ms       milliseconds
     * @param decimals number of decimal places (default 1)
     * @param unitWord if true, use "seconds" instead of "s"
     * @return formatted string like "2.5s" or "2.5 seconds"
     */
    public static String formatSeconds(long ms, int decimals, boolean unitWord) {
        if (ms < 0)
            ms = 0;
        double seconds = ms / 1000.0;
        String formatted = String.format("%." + Math.max(0, decimals) + "f", seconds);
        // Trim trailing zeros after decimal
        if (formatted.contains(".")) {
            formatted = formatted.replaceAll("0+$", "").replaceAll("\\.$", "");
        }
        String unit = unitWord ? " seconds" : "s";
        return formatted + unit;
    }

    /**
     * Format milliseconds as seconds with default 1 decimal.
     */
    public static String formatSeconds(long ms) {
        return formatSeconds(ms, 1, false);
    }

    /**
     * Format duration in milliseconds. Shows "Xms" for <1s, otherwise seconds.
     *
     * @param ms       milliseconds
     * @param decimals decimal places for seconds format (default 2)
     * @param unitWord use "seconds" word instead of "s"
     * @return formatted string like "450ms" or "2.5s"
     */
    public static String formatMs(long ms, int decimals, boolean unitWord) {
        if (ms < 0)
            ms = 0;
        if (ms < 1000) {
            return ms + "ms";
        }
        return formatSeconds(ms, decimals, unitWord);
    }

    /**
     * Format duration in milliseconds with defaults.
     */
    public static String formatMs(long ms) {
        return formatMs(ms, 2, false);
    }
}
