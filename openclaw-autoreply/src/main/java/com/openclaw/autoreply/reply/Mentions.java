package com.openclaw.autoreply.reply;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Mention pattern matching, structural prefix stripping, and mention stripping.
 * Mirrors core functions from {@code auto-reply/reply/mentions.ts}.
 */
public final class Mentions {

    private Mentions() {
    }

    /** Marker used by context/history to identify the current message. */
    public static final String CURRENT_MESSAGE_MARKER = "[Current message - respond to this]";

    private static final String BACKSPACE_CHAR = "\u0008";

    /**
     * Derive mention patterns from an identity name and emoji.
     */
    public static List<String> deriveMentionPatterns(String name, String emoji) {
        List<String> patterns = new ArrayList<>();
        String trimmedName = name != null ? name.trim() : "";
        if (!trimmedName.isEmpty()) {
            String[] parts = trimmedName.split("\\s+");
            StringBuilder sb = new StringBuilder("\\b@?");
            for (int i = 0; i < parts.length; i++) {
                if (i > 0)
                    sb.append("\\s+");
                sb.append(Pattern.quote(parts[i]));
            }
            sb.append("\\b");
            patterns.add(sb.toString());
        }
        String trimmedEmoji = emoji != null ? emoji.trim() : "";
        if (!trimmedEmoji.isEmpty()) {
            patterns.add(Pattern.quote(trimmedEmoji));
        }
        return patterns;
    }

    /** Normalize a mention pattern by replacing backspace chars with \\b. */
    public static String normalizeMentionPattern(String pattern) {
        if (!pattern.contains(BACKSPACE_CHAR))
            return pattern;
        return pattern.replace(BACKSPACE_CHAR, "\\b");
    }

    /** Normalize a list of mention patterns. */
    public static List<String> normalizeMentionPatterns(List<String> patterns) {
        return patterns.stream().map(Mentions::normalizeMentionPattern).toList();
    }

    /** Build compiled regex patterns from string patterns. */
    public static List<Pattern> buildMentionRegexes(List<String> patterns) {
        List<String> normalized = normalizeMentionPatterns(patterns);
        List<Pattern> result = new ArrayList<>();
        for (String p : normalized) {
            try {
                result.add(Pattern.compile(p, Pattern.CASE_INSENSITIVE));
            } catch (Exception ignored) {
                // skip invalid regex
            }
        }
        return result;
    }

    /** Normalize text for mention matching (strip zero-width chars, lowercase). */
    public static String normalizeMentionText(String text) {
        if (text == null)
            return "";
        return text.replaceAll("[\\u200b-\\u200f\\u202a-\\u202e\\u2060-\\u206f]", "").toLowerCase();
    }

    /** Check if text matches any of the mention patterns. */
    public static boolean matchesMentionPatterns(String text, List<Pattern> mentionRegexes) {
        if (mentionRegexes.isEmpty())
            return false;
        String cleaned = normalizeMentionText(text);
        if (cleaned.isEmpty())
            return false;
        return mentionRegexes.stream().anyMatch(re -> re.matcher(cleaned).find());
    }

    /** Explicit mention signal data. */
    public record ExplicitMentionSignal(boolean hasAnyMention, boolean isExplicitlyMentioned,
            boolean canResolveExplicit) {
    }

    /**
     * Match mentions considering explicit mention signal.
     */
    public static boolean matchesMentionWithExplicit(String text, List<Pattern> mentionRegexes,
            ExplicitMentionSignal explicit) {
        String cleaned = normalizeMentionText(text);
        boolean isExplicit = explicit != null && explicit.isExplicitlyMentioned();
        boolean canResolve = explicit != null && explicit.canResolveExplicit();
        boolean hasAny = explicit != null && explicit.hasAnyMention();

        if (hasAny && canResolve) {
            return isExplicit || mentionRegexes.stream().anyMatch(re -> re.matcher(cleaned).find());
        }
        if (cleaned.isEmpty())
            return isExplicit;
        return isExplicit || mentionRegexes.stream().anyMatch(re -> re.matcher(cleaned).find());
    }

    /**
     * Strip structural prefixes (timestamps, sender labels, wrapper labels)
     * so directive detection still works on batched/context messages.
     */
    public static String stripStructuralPrefixes(String text) {
        String input = text;
        if (input.contains(CURRENT_MESSAGE_MARKER)) {
            input = input.substring(input.indexOf(CURRENT_MESSAGE_MARKER) + CURRENT_MESSAGE_MARKER.length())
                    .stripLeading();
        }
        return input
                .replaceAll("\\[[^\\]]+\\]\\s*", "")
                .replaceAll("(?m)^[ \\t]*[A-Za-z0-9+()\\-_. ]+:\\s*", "")
                .replace("\\n", " ")
                .replaceAll("\\s+", " ")
                .trim();
    }

    /**
     * Strip mention patterns from text.
     *
     * @param text            input text
     * @param mentionPatterns list of regex pattern strings
     * @return cleaned text with mentions replaced by spaces
     */
    public static String stripMentions(String text, List<String> mentionPatterns) {
        String result = text;
        for (String p : mentionPatterns) {
            try {
                result = result.replaceAll("(?i)" + p, " ");
            } catch (Exception ignored) {
                // skip invalid regex
            }
        }
        // Strip generic @123456789 patterns
        result = result.replaceAll("@[0-9+]{5,}", " ");
        return result.replaceAll("\\s+", " ").trim();
    }
}
