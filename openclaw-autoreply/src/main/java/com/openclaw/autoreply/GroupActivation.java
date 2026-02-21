package com.openclaw.autoreply;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Group activation mode parsing and normalization.
 * Mirrors {@code auto-reply/group-activation.ts}.
 */
public final class GroupActivation {

    private GroupActivation() {
    }

    /** Normalize a raw group activation string to "mention" or "always". */
    public static String normalizeGroupActivation(String raw) {
        if (raw == null)
            return null;
        String value = raw.trim().toLowerCase();
        if ("mention".equals(value))
            return "mention";
        if ("always".equals(value))
            return "always";
        return null;
    }

    /** Result of parsing a /activation command. */
    public record ActivationCommandResult(boolean hasCommand, String mode) {
    }

    private static final Pattern ACTIVATION_RE = Pattern.compile("^/activation(?:\\s+([a-zA-Z]+))?\\s*$",
            Pattern.CASE_INSENSITIVE);

    /**
     * Parse a /activation command from raw input.
     * The raw input should already be normalized (command body only).
     */
    public static ActivationCommandResult parseActivationCommand(String raw) {
        if (raw == null || raw.isBlank()) {
            return new ActivationCommandResult(false, null);
        }
        // Normalize command body (strip emoji prefix, etc.)
        String normalized = CommandNormalizer.normalizeCommandBody(raw.trim());
        Matcher m = ACTIVATION_RE.matcher(normalized);
        if (!m.matches()) {
            return new ActivationCommandResult(false, null);
        }
        String mode = m.group(1) != null ? normalizeGroupActivation(m.group(1)) : null;
        return new ActivationCommandResult(true, mode);
    }
}
