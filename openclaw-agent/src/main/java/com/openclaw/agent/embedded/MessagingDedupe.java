package com.openclaw.agent.embedded;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Messaging text deduplication.
 * Mirrors {@code agents/pi-embedded-helpers/messaging-dedupe.ts}.
 */
public final class MessagingDedupe {

    private MessagingDedupe() {
    }

    private static final int MIN_DUPLICATE_TEXT_LENGTH = 10;

    // Unicode property escapes are not available in Java regex; use broad Unicode
    // ranges instead.
    private static final Pattern EMOJI_PATTERN = Pattern.compile(
            "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}"
                    + "\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{1FA70}-\\x{1FAFF}"
                    + "\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}\\x{FE00}-\\x{FE0F}\\x{200D}"
                    + "\\x{20E3}\\x{E0020}-\\x{E007F}]");
    private static final Pattern MULTI_SPACE = Pattern.compile("\\s+");

    /**
     * Normalize text for duplicate comparison:
     * trim, lowercase, strip emoji, collapse spaces.
     */
    public static String normalizeTextForComparison(String text) {
        if (text == null)
            return "";
        String trimmed = text.trim().toLowerCase();
        String noEmoji = EMOJI_PATTERN.matcher(trimmed).replaceAll("");
        return MULTI_SPACE.matcher(noEmoji).replaceAll(" ").trim();
    }

    /**
     * Check if a normalized text duplicates any previously sent text.
     */
    public static boolean isMessagingToolDuplicateNormalized(
            String normalized, String[] normalizedSentTexts) {
        if (normalizedSentTexts == null || normalizedSentTexts.length == 0)
            return false;
        if (normalized == null || normalized.length() < MIN_DUPLICATE_TEXT_LENGTH)
            return false;
        for (String sent : normalizedSentTexts) {
            if (sent == null || sent.length() < MIN_DUPLICATE_TEXT_LENGTH)
                continue;
            if (normalized.contains(sent) || sent.contains(normalized))
                return true;
        }
        return false;
    }

    /**
     * Check if a raw text duplicates any previously sent text.
     */
    public static boolean isMessagingToolDuplicate(String text, String[] sentTexts) {
        if (sentTexts == null || sentTexts.length == 0)
            return false;
        String normalized = normalizeTextForComparison(text);
        if (normalized.length() < MIN_DUPLICATE_TEXT_LENGTH)
            return false;
        String[] normalizedSent = new String[sentTexts.length];
        for (int i = 0; i < sentTexts.length; i++) {
            normalizedSent[i] = normalizeTextForComparison(sentTexts[i]);
        }
        return isMessagingToolDuplicateNormalized(normalized, normalizedSent);
    }
}
