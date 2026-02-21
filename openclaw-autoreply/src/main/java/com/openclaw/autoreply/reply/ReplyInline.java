package com.openclaw.autoreply.reply;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Inline simple command extraction and status stripping from message body.
 * Mirrors {@code auto-reply/reply/reply-inline.ts}.
 */
public final class ReplyInline {

    private ReplyInline() {
    }

    private static final Map<String, String> INLINE_SIMPLE_COMMAND_ALIASES = Map.of(
            "/help", "/help",
            "/commands", "/commands",
            "/whoami", "/whoami",
            "/id", "/whoami");

    private static final Pattern INLINE_SIMPLE_COMMAND_RE = Pattern
            .compile("(?:^|\\s)/(help|commands|whoami|id)(?=$|\\s|:)", Pattern.CASE_INSENSITIVE);

    private static final Pattern INLINE_STATUS_RE = Pattern.compile("(?:^|\\s)/status(?=$|\\s|:)(?:\\s*:\\s*)?",
            Pattern.CASE_INSENSITIVE);

    /** Result of extracting an inline simple command. */
    public record InlineCommandResult(String command, String cleaned) {
    }

    /**
     * Extract an inline simple command (/help, /commands, /whoami, /id) from body.
     * 
     * @return the command and cleaned body, or null if none found
     */
    public static InlineCommandResult extractInlineSimpleCommand(String body) {
        if (body == null || body.isEmpty())
            return null;
        Matcher m = INLINE_SIMPLE_COMMAND_RE.matcher(body);
        if (!m.find())
            return null;
        String alias = "/" + m.group(1).toLowerCase();
        String command = INLINE_SIMPLE_COMMAND_ALIASES.get(alias);
        if (command == null)
            return null;
        String cleaned = body.replace(m.group(0), " ").replaceAll("\\s+", " ").trim();
        return new InlineCommandResult(command, cleaned);
    }

    /** Result of stripping inline /status directives. */
    public record StripStatusResult(String cleaned, boolean didStrip) {
    }

    /**
     * Strip inline /status directives from body.
     */
    public static StripStatusResult stripInlineStatus(String body) {
        if (body == null || body.isBlank()) {
            return new StripStatusResult("", false);
        }
        String trimmed = body.trim();
        String cleaned = INLINE_STATUS_RE.matcher(trimmed).replaceAll(" ").replaceAll("\\s+", " ").trim();
        return new StripStatusResult(cleaned, !cleaned.equals(trimmed));
    }
}
