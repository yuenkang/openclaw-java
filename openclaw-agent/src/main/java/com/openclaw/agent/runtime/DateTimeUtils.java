package com.openclaw.agent.runtime;

import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.TextStyle;
import java.util.Locale;

/**
 * Date/time utilities: timezone resolution, time formatting, timestamp
 * normalization.
 * Corresponds to TypeScript agents/date-time.ts.
 */
public final class DateTimeUtils {

    private DateTimeUtils() {
    }

    // ───── Timezone ─────

    /**
     * Resolve the user's timezone. Falls back to system default then UTC.
     */
    public static String resolveUserTimezone(String configured) {
        if (configured != null && !configured.isBlank()) {
            String trimmed = configured.trim();
            try {
                ZoneId.of(trimmed); // validate
                return trimmed;
            } catch (Exception ignored) {
                // invalid timezone, fall through
            }
        }
        ZoneId systemZone = ZoneId.systemDefault();
        return systemZone != null ? systemZone.getId() : "UTC";
    }

    // ───── Time Format ─────

    public enum TimeFormat {
        H12, H24
    }

    /**
     * Resolve 12 vs 24 hour time format.
     */
    public static TimeFormat resolveUserTimeFormat(String preference) {
        if ("12".equals(preference))
            return TimeFormat.H12;
        if ("24".equals(preference))
            return TimeFormat.H24;
        // Auto-detect: check system locale
        return detectSystemTimeFormat() ? TimeFormat.H24 : TimeFormat.H12;
    }

    private static boolean detectSystemTimeFormat() {
        try {
            ZonedDateTime sample = ZonedDateTime.of(2000, 1, 1, 13, 0, 0, 0, ZoneId.systemDefault());
            String formatted = DateTimeFormatter.ofPattern("h a", Locale.getDefault()).format(sample);
            // If locale formats 13:00 without "1" (i.e. shows "13"), it's 24h
            return !formatted.contains("1");
        } catch (Exception e) {
            return false;
        }
    }

    // ───── Timestamp Normalization ─────

    /**
     * Normalized timestamp result.
     */
    public record NormalizedTimestamp(long timestampMs, String timestampUtc) {
    }

    /**
     * Normalize a raw timestamp value (epoch seconds, epoch millis, ISO string,
     * numeric string).
     *
     * @return Normalized timestamp or null if unparseable
     */
    public static NormalizedTimestamp normalizeTimestamp(Object raw) {
        if (raw == null)
            return null;

        long timestampMs;

        if (raw instanceof Number num) {
            double val = num.doubleValue();
            if (!Double.isFinite(val))
                return null;
            timestampMs = val < 1_000_000_000_000L ? Math.round(val * 1000) : Math.round(val);
        } else if (raw instanceof String str) {
            String trimmed = str.trim();
            if (trimmed.isEmpty())
                return null;

            if (trimmed.matches("^\\d+(\\.\\d+)?$")) {
                double num = Double.parseDouble(trimmed);
                if (!Double.isFinite(num))
                    return null;
                if (trimmed.contains(".")) {
                    timestampMs = Math.round(num * 1000);
                } else if (trimmed.length() >= 13) {
                    timestampMs = Math.round(num);
                } else {
                    timestampMs = Math.round(num * 1000);
                }
            } else {
                try {
                    Instant parsed = Instant.parse(trimmed);
                    timestampMs = parsed.toEpochMilli();
                } catch (Exception e) {
                    return null;
                }
            }
        } else {
            return null;
        }

        String utcStr = Instant.ofEpochMilli(timestampMs).toString();
        return new NormalizedTimestamp(timestampMs, utcStr);
    }

    // ───── User Time Formatting ─────

    private static String ordinalSuffix(int day) {
        if (day >= 11 && day <= 13)
            return "th";
        return switch (day % 10) {
            case 1 -> "st";
            case 2 -> "nd";
            case 3 -> "rd";
            default -> "th";
        };
    }

    /**
     * Format a timestamp for user display.
     *
     * @param instant  The instant to format
     * @param timeZone IANA timezone id
     * @param format   12 or 24 hour format
     * @return Formatted string like "Wednesday, January 1st, 2025 — 13:00"
     */
    public static String formatUserTime(Instant instant, String timeZone, TimeFormat format) {
        try {
            ZoneId zone = ZoneId.of(timeZone);
            ZonedDateTime zdt = instant.atZone(zone);

            String weekday = zdt.getDayOfWeek().getDisplayName(TextStyle.FULL, Locale.US);
            String month = zdt.getMonth().getDisplayName(TextStyle.FULL, Locale.US);
            int day = zdt.getDayOfMonth();
            int year = zdt.getYear();

            String timePart;
            if (format == TimeFormat.H24) {
                timePart = String.format("%02d:%02d", zdt.getHour(), zdt.getMinute());
            } else {
                int hour = zdt.getHour() % 12;
                if (hour == 0)
                    hour = 12;
                String amPm = zdt.getHour() < 12 ? "AM" : "PM";
                timePart = String.format("%d:%02d %s", hour, zdt.getMinute(), amPm);
            }

            return String.format("%s, %s %d%s, %d — %s",
                    weekday, month, day, ordinalSuffix(day), year, timePart);
        } catch (Exception e) {
            return null;
        }
    }
}
