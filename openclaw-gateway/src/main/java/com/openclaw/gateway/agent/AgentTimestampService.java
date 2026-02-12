package com.openclaw.gateway.agent;

import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Locale;
import java.util.regex.Pattern;

/**
 * Injects a compact timestamp prefix into agent messages.
 * Corresponds to TypeScript's server-methods/agent-timestamp.ts.
 *
 * <p>
 * Format: {@code [Wed 2026-02-12 15:30 CST] message}
 */
public class AgentTimestampService {

    /** Matches {@code [... YYYY-MM-DD HH:MM ...]} at start of message. */
    private static final Pattern TIMESTAMP_ENVELOPE = Pattern.compile("^\\[.*\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}");

    /** Matches {@code Current time: } injected by cron jobs. */
    private static final Pattern CRON_TIME = Pattern.compile("Current time: ");

    private static final DateTimeFormatter DATE_FMT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm", Locale.US);

    private static final DateTimeFormatter DOW_FMT = DateTimeFormatter.ofPattern("EEE", Locale.US);

    /**
     * Injects a timestamp prefix unless the message already has one.
     *
     * @param message  raw message text
     * @param timezone IANA timezone (e.g. "Asia/Shanghai"), defaults to "UTC"
     * @return message with timestamp prefix, or unchanged if already present
     */
    public String injectTimestamp(String message, String timezone) {
        if (message == null || message.isBlank())
            return message;
        if (TIMESTAMP_ENVELOPE.matcher(message).find())
            return message;
        if (CRON_TIME.matcher(message).find())
            return message;

        String tz = (timezone != null && !timezone.isBlank()) ? timezone : "UTC";
        try {
            ZoneId zone = ZoneId.of(tz);
            ZonedDateTime now = ZonedDateTime.now(zone);
            String dateStr = now.format(DATE_FMT);
            String dow = now.format(DOW_FMT);
            String zoneShort = now.getZone().getDisplayName(
                    java.time.format.TextStyle.SHORT, Locale.US);
            return "[" + dow + " " + dateStr + " " + zoneShort + "] " + message;
        } catch (Exception e) {
            // Fallback: no injection
            return message;
        }
    }

    /**
     * Convenience overload with explicit Instant.
     */
    public String injectTimestamp(String message, String timezone, Instant now) {
        if (message == null || message.isBlank())
            return message;
        if (TIMESTAMP_ENVELOPE.matcher(message).find())
            return message;
        if (CRON_TIME.matcher(message).find())
            return message;

        String tz = (timezone != null && !timezone.isBlank()) ? timezone : "UTC";
        try {
            ZoneId zone = ZoneId.of(tz);
            ZonedDateTime zdt = now.atZone(zone);
            String dateStr = zdt.format(DATE_FMT);
            String dow = zdt.format(DOW_FMT);
            String zoneShort = zdt.getZone().getDisplayName(
                    java.time.format.TextStyle.SHORT, Locale.US);
            return "[" + dow + " " + dateStr + " " + zoneShort + "] " + message;
        } catch (Exception e) {
            return message;
        }
    }
}
