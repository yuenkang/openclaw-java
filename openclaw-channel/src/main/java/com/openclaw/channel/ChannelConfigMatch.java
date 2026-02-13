package com.openclaw.channel;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Channel configuration matching and resolution.
 * Corresponds to TypeScript's channels/channel-config.ts.
 */
public final class ChannelConfigMatch {

    private ChannelConfigMatch() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    /** Result of a channel entry match. */
    public record EntryMatch(String key, String accountId, Map<String, Object> entry,
            String matchSource) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    private static final Pattern SLUG_SEPARATOR = Pattern.compile("[\\s_]+");
    private static final Pattern SLUG_INVALID = Pattern.compile("[^a-z0-9-]");
    private static final Pattern SLUG_MULTI_DASH = Pattern.compile("-{2,}");

    /**
     * Normalize a string to a URL-safe slug.
     */
    public static String normalizeSlug(String raw) {
        if (raw == null)
            return "";
        String text = raw.trim().toLowerCase();
        text = SLUG_SEPARATOR.matcher(text).replaceAll("-");
        text = SLUG_INVALID.matcher(text).replaceAll("");
        text = SLUG_MULTI_DASH.matcher(text).replaceAll("-");
        if (text.startsWith("-"))
            text = text.substring(1);
        if (text.endsWith("-"))
            text = text.substring(0, text.length() - 1);
        return text;
    }

    /**
     * Build a prioritized list of key candidates for matching.
     * Includes the raw value, its slug form, and various formats.
     */
    public static List<String> buildKeyCandidates(String raw) {
        List<String> candidates = new ArrayList<>();
        if (raw == null || raw.isBlank()) {
            return candidates;
        }
        String trimmed = raw.trim();
        candidates.add(trimmed);

        String lower = trimmed.toLowerCase();
        if (!candidates.contains(lower)) {
            candidates.add(lower);
        }

        String slug = normalizeSlug(trimmed);
        if (!slug.isEmpty() && !candidates.contains(slug)) {
            candidates.add(slug);
        }

        // With common prefix variations
        if (!trimmed.startsWith("#")) {
            String hashed = "#" + lower;
            if (!candidates.contains(hashed)) {
                candidates.add(hashed);
            }
        }

        return candidates;
    }

    /**
     * Find the best matching entry from a config section using key candidates.
     *
     * @param section    the channel config section (key → entry map)
     * @param candidates prioritized key candidates to try
     * @return matched entry or null
     */
    @SuppressWarnings("unchecked")
    public static EntryMatch findMatch(Map<String, Object> section,
            List<String> candidates) {
        if (section == null || candidates == null) {
            return null;
        }
        for (String candidate : candidates) {
            Object entry = section.get(candidate);
            if (entry instanceof Map) {
                return new EntryMatch(candidate, null, (Map<String, Object>) entry, "key");
            }
        }
        // Try wildcard
        Object wildcard = section.get("*");
        if (wildcard instanceof Map) {
            return new EntryMatch("*", null, (Map<String, Object>) wildcard, "wildcard");
        }
        return null;
    }

    /**
     * Resolve a channel entry match with account ID fallback.
     */
    @SuppressWarnings("unchecked")
    public static EntryMatch resolveMatch(Map<String, Object> section,
            String groupId,
            String groupChannel,
            String accountId) {
        // Build candidates from group channel and group ID
        List<String> candidates = new ArrayList<>();
        if (groupChannel != null && !groupChannel.isBlank()) {
            candidates.addAll(buildKeyCandidates(groupChannel));
        }
        if (groupId != null && !groupId.isBlank()) {
            candidates.addAll(buildKeyCandidates(groupId));
        }

        EntryMatch match = findMatch(section, candidates);
        if (match != null) {
            return new EntryMatch(match.key(), accountId, match.entry(), match.matchSource());
        }
        return null;
    }
}
