package com.openclaw.agent.runtime;

import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Thinking level fallback resolution.
 * Corresponds to TypeScript pi-embedded-helpers/thinking.ts.
 *
 * <p>
 * When an LLM API rejects a thinking level, this utility parses the error
 * message for supported values and picks the best fallback.
 * </p>
 */
public final class ThinkingFallback {

    /** Standard think levels. */
    public static final List<String> THINK_LEVELS = List.of("none", "low", "medium", "high");

    private static final Pattern SUPPORTED_VALUES_RE = Pattern.compile("supported values (?:are:\\s*|:\\s*)([^\\n.]+)",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern QUOTED_RE = Pattern.compile("['\"]([^'\"]+)['\"]");

    private ThinkingFallback() {
    }

    /**
     * Normalize a raw think level string to a canonical form.
     *
     * @return Normalized level or null if unrecognized
     */
    public static String normalizeThinkLevel(String raw) {
        if (raw == null)
            return null;
        String lower = raw.trim().toLowerCase();
        return THINK_LEVELS.contains(lower) ? lower : null;
    }

    /**
     * Parse an error message for supported thinking values.
     */
    static List<String> extractSupportedValues(String raw) {
        if (raw == null || raw.isBlank())
            return List.of();

        Matcher m = SUPPORTED_VALUES_RE.matcher(raw);
        if (!m.find())
            return List.of();

        String fragment = m.group(1);

        // Try quoted values first
        Matcher qm = QUOTED_RE.matcher(fragment);
        List<String> quoted = qm.results()
                .map(mr -> mr.group(1).trim())
                .filter(s -> !s.isEmpty())
                .collect(Collectors.toList());
        if (!quoted.isEmpty())
            return quoted;

        // Fall back to comma/and-separated
        return Pattern.compile(",|\\band\\b", Pattern.CASE_INSENSITIVE)
                .splitAsStream(fragment)
                .map(s -> s.replaceAll("^[^a-zA-Z]+|[^a-zA-Z]+$", "").trim())
                .filter(s -> !s.isEmpty())
                .collect(Collectors.toList());
    }

    /**
     * Pick a fallback thinking level from an error message, skipping
     * already-attempted levels.
     *
     * @param message   Error message from the LLM API
     * @param attempted Set of levels already tried
     * @return Fallback level or null if none found
     */
    public static String pickFallbackThinkingLevel(String message, Set<String> attempted) {
        if (message == null || message.isBlank())
            return null;

        List<String> supported = extractSupportedValues(message);
        if (supported.isEmpty())
            return null;

        for (String entry : supported) {
            String normalized = normalizeThinkLevel(entry);
            if (normalized == null)
                continue;
            if (attempted.contains(normalized))
                continue;
            return normalized;
        }

        return null;
    }
}
