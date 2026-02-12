package com.openclaw.agent.embedded;

import java.util.regex.Pattern;

/**
 * Thinking-level fallback resolution when a provider rejects a requested level.
 * Mirrors {@code agents/pi-embedded-helpers/thinking.ts}.
 */
public final class ThinkingHelpers {

    private ThinkingHelpers() {
    }

    /** Recognized thinking levels. */
    public enum ThinkLevel {
        NONE, LOW, MEDIUM, HIGH;

        public String toValue() {
            return name().toLowerCase();
        }

        public static ThinkLevel fromValue(String value) {
            if (value == null)
                return null;
            return switch (value.toLowerCase().trim()) {
                case "none", "off", "disabled" -> NONE;
                case "low", "min", "minimal" -> LOW;
                case "medium", "med", "moderate" -> MEDIUM;
                case "high", "max", "maximum" -> HIGH;
                default -> null;
            };
        }
    }

    private static final Pattern SUPPORTED_VALUES_RE1 = Pattern.compile("supported values are:\\s*([^\\n.]+)",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern SUPPORTED_VALUES_RE2 = Pattern.compile("supported values:\\s*([^\\n.]+)",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern QUOTED_RE = Pattern.compile("['\"]([^'\"]+)['\"]");

    /**
     * Given an error message from a provider, extract supported values and
     * pick the first one not yet attempted.
     */
    public static ThinkLevel pickFallbackThinkingLevel(String message, java.util.Set<ThinkLevel> attempted) {
        if (message == null || message.isBlank())
            return null;
        String raw = message.trim();

        String[] supported = extractSupportedValues(raw);
        if (supported.length == 0)
            return null;

        for (String entry : supported) {
            ThinkLevel level = ThinkLevel.fromValue(entry);
            if (level == null)
                continue;
            if (attempted.contains(level))
                continue;
            return level;
        }
        return null;
    }

    static String[] extractSupportedValues(String raw) {
        java.util.regex.Matcher m = SUPPORTED_VALUES_RE1.matcher(raw);
        if (!m.find()) {
            m = SUPPORTED_VALUES_RE2.matcher(raw);
            if (!m.find())
                return new String[0];
        }
        String fragment = m.group(1);

        // Try quoted values first
        java.util.regex.Matcher qm = QUOTED_RE.matcher(fragment);
        java.util.List<String> quoted = new java.util.ArrayList<>();
        while (qm.find()) {
            String v = qm.group(1);
            if (v != null && !v.isBlank())
                quoted.add(v.trim());
        }
        if (!quoted.isEmpty())
            return quoted.toArray(new String[0]);

        // Fall back to comma/and splitting
        String[] parts = fragment.split("[,]|\\band\\b");
        java.util.List<String> result = new java.util.ArrayList<>();
        for (String part : parts) {
            String cleaned = part.replaceAll("^[^a-zA-Z]+|[^a-zA-Z]+$", "").trim();
            if (!cleaned.isEmpty())
                result.add(cleaned);
        }
        return result.toArray(new String[0]);
    }
}
