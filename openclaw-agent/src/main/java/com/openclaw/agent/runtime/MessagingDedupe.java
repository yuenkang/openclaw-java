package com.openclaw.agent.runtime;

import java.util.List;
import java.util.regex.Pattern;

/**
 * Message deduplication for channel delivery.
 * Corresponds to TypeScript pi-embedded-helpers/messaging-dedupe.ts.
 */
public final class MessagingDedupe {

    private static final int MIN_DUPLICATE_TEXT_LENGTH = 10;
    // Match Emoji_Presentation and Extended_Pictographic
    private static final Pattern EMOJI_RE = Pattern.compile("[\\p{So}\\p{Cn}]", Pattern.UNICODE_CHARACTER_CLASS);

    private MessagingDedupe() {
    }

    /**
     * Normalize text for duplicate comparison.
     * Trims, lowercases, strips emoji, collapses whitespace.
     */
    public static String normalizeTextForComparison(String text) {
        if (text == null)
            return "";
        return EMOJI_RE.matcher(text.trim().toLowerCase())
                .replaceAll("")
                .replaceAll("\\s+", " ")
                .trim();
    }

    /**
     * Check if normalized text is a duplicate of any previously sent text.
     */
    public static boolean isDuplicateNormalized(
            String normalized, List<String> normalizedSentTexts) {
        if (normalizedSentTexts == null || normalizedSentTexts.isEmpty())
            return false;
        if (normalized == null || normalized.length() < MIN_DUPLICATE_TEXT_LENGTH)
            return false;

        return normalizedSentTexts.stream().anyMatch(sent -> {
            if (sent == null || sent.length() < MIN_DUPLICATE_TEXT_LENGTH)
                return false;
            return normalized.contains(sent) || sent.contains(normalized);
        });
    }

    /**
     * Check if a text is a duplicate of any previously sent text.
     * Normalizes both the input and the sent texts before comparing.
     */
    public static boolean isDuplicate(String text, List<String> sentTexts) {
        if (sentTexts == null || sentTexts.isEmpty())
            return false;
        String normalized = normalizeTextForComparison(text);
        if (normalized.length() < MIN_DUPLICATE_TEXT_LENGTH)
            return false;

        List<String> normalizedSent = sentTexts.stream()
                .map(MessagingDedupe::normalizeTextForComparison)
                .toList();
        return isDuplicateNormalized(normalized, normalizedSent);
    }
}
