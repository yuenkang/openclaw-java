package com.openclaw.agent.autoreply;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Command body normalization utilities.
 * Extracts the core normalizeCommandBody from commands-registry.ts
 * for use by other modules (GroupActivation, SendPolicy, etc.).
 * Full command registry (with command definitions, args parsing, etc.)
 * will be added in a later batch.
 * Mirrors the {@code normalizeCommandBody} function from
 * {@code auto-reply/commands-registry.ts}.
 */
public final class CommandNormalizer {

    private CommandNormalizer() {
    }

    private static final Pattern COLON_CMD_RE = Pattern.compile("^/([^\\s:]+)\\s*:(.*)$");
    private static final Pattern MENTION_CMD_RE = Pattern.compile("^/([^\\s@]+)@([^\\s]+)(.*)$");
    private static final Pattern TOKEN_CMD_RE = Pattern.compile("^/([^\\s]+)(?:\\s+([\\s\\S]+))?$");

    /**
     * Normalize a raw command body.
     * Handles colon syntax (/cmd:value → /cmd value),
     * bot mention stripping (/cmd@bot args → /cmd args),
     * and basic whitespace normalization.
     */
    public static String normalizeCommandBody(String raw) {
        return normalizeCommandBody(raw, null);
    }

    public static String normalizeCommandBody(String raw, String botUsername) {
        if (raw == null)
            return "";
        String trimmed = raw.trim();
        if (!trimmed.startsWith("/"))
            return trimmed;

        // Take single line only
        int newline = trimmed.indexOf('\n');
        String singleLine = newline == -1 ? trimmed : trimmed.substring(0, newline).trim();

        // Colon syntax: /cmd:value → /cmd value
        Matcher colonMatch = COLON_CMD_RE.matcher(singleLine);
        String normalized;
        if (colonMatch.matches()) {
            String command = colonMatch.group(1);
            String rest = colonMatch.group(2).stripLeading();
            normalized = rest.isEmpty() ? "/" + command : "/" + command + " " + rest;
        } else {
            normalized = singleLine;
        }

        // Strip bot mention: /cmd@botname args → /cmd args
        if (botUsername != null && !botUsername.isBlank()) {
            String normalizedBotUsername = botUsername.trim().toLowerCase();
            Matcher mentionMatch = MENTION_CMD_RE.matcher(normalized);
            if (mentionMatch.matches() &&
                    mentionMatch.group(2).toLowerCase().equals(normalizedBotUsername)) {
                String tail = mentionMatch.group(3) != null ? mentionMatch.group(3) : "";
                normalized = "/" + mentionMatch.group(1) + tail;
            }
        }

        return normalized;
    }

    /** Check if a normalized command body starts with "/" (is likely a command). */
    public static boolean isCommandMessage(String raw) {
        return normalizeCommandBody(raw).startsWith("/");
    }
}
