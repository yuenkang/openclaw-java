package com.openclaw.gateway.cron;

import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.regex.Pattern;

/**
 * Cron date/time parsing utilities.
 * Corresponds to TypeScript's cron/parse.ts.
 */
public final class CronParse {

    private CronParse() {
    }

    private static final Pattern ISO_TZ_RE = Pattern.compile("(Z|[+-]\\d{2}:?\\d{2})$", Pattern.CASE_INSENSITIVE);
    private static final Pattern ISO_DATE_RE = Pattern.compile("^\\d{4}-\\d{2}-\\d{2}$");
    private static final Pattern ISO_DATE_TIME_RE = Pattern.compile("^\\d{4}-\\d{2}-\\d{2}T");
    private static final Pattern NUMERIC_RE = Pattern.compile("^\\d+$");

    /**
     * Normalize a date string to a UTC ISO-8601 string.
     * - If the string already has a timezone, return as-is.
     * - If it's a date only (2024-01-01), append T00:00:00Z.
     * - If it's a date-time without TZ (2024-01-01T12:00:00), append Z.
     */
    static String normalizeUtcIso(String raw) {
        if (ISO_TZ_RE.matcher(raw).find())
            return raw;
        if (ISO_DATE_RE.matcher(raw).matches())
            return raw + "T00:00:00Z";
        if (ISO_DATE_TIME_RE.matcher(raw).find())
            return raw + "Z";
        return raw;
    }

    /**
     * Parse an absolute time input and return epoch milliseconds.
     * Accepts:
     * <ul>
     * <li>Numeric strings (interpreted as epoch ms)</li>
     * <li>ISO-8601 date/datetime strings</li>
     * </ul>
     *
     * @return epoch milliseconds, or null if parsing fails
     */
    public static Long parseAbsoluteTimeMs(String input) {
        if (input == null)
            return null;
        String raw = input.trim();
        if (raw.isEmpty())
            return null;

        // Pure numeric → epoch ms
        if (NUMERIC_RE.matcher(raw).matches()) {
            try {
                long n = Long.parseLong(raw);
                return n > 0 ? n : null;
            } catch (NumberFormatException e) {
                return null;
            }
        }

        // ISO date/time
        try {
            String normalized = normalizeUtcIso(raw);
            Instant instant = Instant.parse(normalized);
            return instant.toEpochMilli();
        } catch (DateTimeParseException e) {
            return null;
        }
    }
}
