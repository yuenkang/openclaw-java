package com.openclaw.autoreply.reply.queue;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parsing of {@code /queue} directives from message text.
 * Mirrors {@code auto-reply/reply/queue/directive.ts}.
 */
public final class QueueDirective {

    private QueueDirective() {
    }

    /** Parsed /queue directive result. */
    public record QueueDirectiveResult(
            String cleaned,
            String queueMode,
            boolean queueReset,
            String rawMode,
            boolean hasDirective,
            Integer debounceMs,
            Integer cap,
            String dropPolicy,
            String rawDebounce,
            String rawCap,
            String rawDrop,
            boolean hasOptions) {
    }

    /* ── helpers ──────────────────────────────────────────────── */

    static Integer parseQueueDebounce(String raw) {
        if (raw == null || raw.isBlank())
            return null;
        try {
            // Simplified: treat as plain ms integer (full parseDurationMs deferred)
            long ms = Long.parseLong(raw.trim());
            return ms >= 0 ? (int) Math.round((double) ms) : null;
        } catch (NumberFormatException e) {
            return null;
        }
    }

    static Integer parseQueueCap(String raw) {
        if (raw == null || raw.isBlank())
            return null;
        try {
            double num = Double.parseDouble(raw.trim());
            if (!Double.isFinite(num))
                return null;
            int cap = (int) Math.floor(num);
            return cap >= 1 ? cap : null;
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /* ── internal args parser ────────────────────────────────── */

    private record ArgParseResult(
            int consumed,
            String queueMode,
            boolean queueReset,
            String rawMode,
            Integer debounceMs,
            Integer cap,
            String dropPolicy,
            String rawDebounce,
            String rawCap,
            String rawDrop,
            boolean hasOptions) {
    }

    private static ArgParseResult parseQueueDirectiveArgs(String raw) {
        int i = 0;
        int len = raw.length();
        // skip leading whitespace
        while (i < len && Character.isWhitespace(raw.charAt(i)))
            i++;
        // skip optional colon
        if (i < len && raw.charAt(i) == ':') {
            i++;
            while (i < len && Character.isWhitespace(raw.charAt(i)))
                i++;
        }
        int consumed = i;
        String queueMode = null;
        boolean queueReset = false;
        String rawMode = null;
        Integer debounceMs = null;
        Integer cap = null;
        String dropPolicy = null;
        String rawDebounce = null;
        String rawCap = null;
        String rawDrop = null;
        boolean hasOptions = false;

        while (i < len) {
            // take token
            int start = i;
            while (i < len && !Character.isWhitespace(raw.charAt(i)))
                i++;
            if (start == i)
                break;
            String token = raw.substring(start, i);
            while (i < len && Character.isWhitespace(raw.charAt(i)))
                i++;

            String lowered = token.trim().toLowerCase();
            if (lowered.equals("default") || lowered.equals("reset") || lowered.equals("clear")) {
                queueReset = true;
                consumed = i;
                break;
            }
            if (lowered.startsWith("debounce:") || lowered.startsWith("debounce=")) {
                rawDebounce = token.split("[:=]", 2).length > 1 ? token.split("[:=]", 2)[1] : "";
                debounceMs = parseQueueDebounce(rawDebounce);
                hasOptions = true;
                consumed = i;
                continue;
            }
            if (lowered.startsWith("cap:") || lowered.startsWith("cap=")) {
                rawCap = token.split("[:=]", 2).length > 1 ? token.split("[:=]", 2)[1] : "";
                cap = parseQueueCap(rawCap);
                hasOptions = true;
                consumed = i;
                continue;
            }
            if (lowered.startsWith("drop:") || lowered.startsWith("drop=")) {
                rawDrop = token.split("[:=]", 2).length > 1 ? token.split("[:=]", 2)[1] : "";
                dropPolicy = QueueNormalize.normalizeQueueDropPolicy(rawDrop);
                hasOptions = true;
                consumed = i;
                continue;
            }
            String mode = QueueNormalize.normalizeQueueMode(token);
            if (mode != null) {
                queueMode = mode;
                rawMode = token;
                consumed = i;
                continue;
            }
            // Stop at first unrecognized token.
            break;
        }

        return new ArgParseResult(consumed, queueMode, queueReset, rawMode,
                debounceMs, cap, dropPolicy, rawDebounce, rawCap, rawDrop, hasOptions);
    }

    /* ── public API ──────────────────────────────────────────── */

    private static final Pattern QUEUE_RE = Pattern.compile("(?:^|\\s)/queue(?=$|\\s|:)", Pattern.CASE_INSENSITIVE);

    /**
     * Extract a {@code /queue} directive from message body text.
     */
    public static QueueDirectiveResult extractQueueDirective(String body) {
        if (body == null || body.isEmpty()) {
            return new QueueDirectiveResult("", null, false, null,
                    false, null, null, null, null, null, null, false);
        }
        Matcher m = QUEUE_RE.matcher(body);
        if (!m.find()) {
            return new QueueDirectiveResult(body.trim(), null, false, null,
                    false, null, null, null, null, null, null, false);
        }
        int matchStart = m.start();
        int slashOffset = m.group().indexOf("/queue");
        int start = matchStart + slashOffset;
        int argsStart = start + "/queue".length();
        String args = body.substring(argsStart);
        ArgParseResult parsed = parseQueueDirectiveArgs(args);
        String cleanedRaw = body.substring(0, start) + " " + body.substring(argsStart + parsed.consumed());
        String cleaned = cleanedRaw.replaceAll("\\s+", " ").trim();
        return new QueueDirectiveResult(
                cleaned, parsed.queueMode(), parsed.queueReset(), parsed.rawMode(),
                true, parsed.debounceMs(), parsed.cap(), parsed.dropPolicy(),
                parsed.rawDebounce(), parsed.rawCap(), parsed.rawDrop(), parsed.hasOptions());
    }
}
