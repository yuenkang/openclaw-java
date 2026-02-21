package com.openclaw.autoreply;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Send policy override parsing and normalization.
 * Mirrors {@code auto-reply/send-policy.ts}.
 */
public final class SendPolicy {

    private SendPolicy() {
    }

    /** Normalize a raw send policy string to "allow" or "deny". */
    public static String normalizeSendPolicyOverride(String raw) {
        if (raw == null)
            return null;
        String value = raw.trim().toLowerCase();
        if ("allow".equals(value) || "on".equals(value))
            return "allow";
        if ("deny".equals(value) || "off".equals(value))
            return "deny";
        return null;
    }

    /** Result of parsing a /send command. */
    public record SendPolicyCommandResult(boolean hasCommand, String mode) {
    }

    private static final Pattern SEND_RE = Pattern.compile("^/send(?:\\s+([a-zA-Z]+))?\\s*$", Pattern.CASE_INSENSITIVE);

    /**
     * Parse a /send command from raw input.
     */
    public static SendPolicyCommandResult parseSendPolicyCommand(String raw) {
        if (raw == null || raw.isBlank()) {
            return new SendPolicyCommandResult(false, null);
        }
        String normalized = CommandNormalizer.normalizeCommandBody(raw.trim());
        Matcher m = SEND_RE.matcher(normalized);
        if (!m.matches()) {
            return new SendPolicyCommandResult(false, null);
        }
        String token = m.group(1) != null ? m.group(1).trim().toLowerCase() : null;
        if (token == null) {
            return new SendPolicyCommandResult(true, null);
        }
        if ("inherit".equals(token) || "default".equals(token) || "reset".equals(token)) {
            return new SendPolicyCommandResult(true, "inherit");
        }
        String mode = normalizeSendPolicyOverride(token);
        return new SendPolicyCommandResult(true, mode);
    }
}
