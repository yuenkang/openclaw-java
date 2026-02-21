package com.openclaw.autoreply;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Model directive extraction from message body.
 * Detects /model directives and alias shortcuts.
 * Mirrors {@code auto-reply/model.ts}.
 */
public final class ModelDirective {

    private ModelDirective() {
    }

    /** Result of extracting a model directive from message text. */
    public record ExtractResult(
            String cleaned,
            String rawModel,
            String rawProfile,
            boolean hasDirective) {
    }

    private static final Pattern MODEL_DIRECTIVE_RE = Pattern.compile(
            "(?:^|\\s)/model(?=$|\\s|:)\\s*:?\\s*([A-Za-z0-9_.:@-]+(?:/[A-Za-z0-9_.:@-]+)*)?",
            Pattern.CASE_INSENSITIVE);

    /**
     * Extract a /model directive from a message body.
     */
    public static ExtractResult extractModelDirective(String body) {
        return extractModelDirective(body, List.of());
    }

    public static ExtractResult extractModelDirective(String body, List<String> aliases) {
        if (body == null || body.isBlank()) {
            return new ExtractResult("", null, null, false);
        }

        Matcher modelMatch = MODEL_DIRECTIVE_RE.matcher(body);
        boolean hasModelMatch = modelMatch.find();

        Matcher aliasMatch = null;
        boolean hasAliasMatch = false;
        if (!hasModelMatch && aliases != null && !aliases.isEmpty()) {
            List<String> cleanAliases = aliases.stream()
                    .map(String::trim)
                    .filter(s -> !s.isEmpty())
                    .toList();
            if (!cleanAliases.isEmpty()) {
                String aliasGroup = cleanAliases.stream()
                        .map(Pattern::quote)
                        .reduce((a, b) -> a + "|" + b)
                        .orElse("");
                Pattern aliasRe = Pattern.compile(
                        "(?:^|\\s)/(" + aliasGroup + ")(?=$|\\s|:)(?:\\s*:\\s*)?",
                        Pattern.CASE_INSENSITIVE);
                aliasMatch = aliasRe.matcher(body);
                hasAliasMatch = aliasMatch.find();
            }
        }

        Matcher match = hasModelMatch ? modelMatch : (hasAliasMatch ? aliasMatch : null);
        if (match == null) {
            return new ExtractResult(body.trim(), null, null, false);
        }

        String raw = hasModelMatch
                ? (match.group(1) != null ? match.group(1).trim() : null)
                : (match.group(1) != null ? match.group(1).trim() : null);

        String rawModel = raw;
        String rawProfile = null;
        if (raw != null && raw.contains("@")) {
            String[] parts = raw.split("@", 2);
            rawModel = parts[0].trim();
            rawProfile = parts.length > 1 ? parts[1].trim() : null;
            if (rawProfile != null && rawProfile.isEmpty())
                rawProfile = null;
        }

        String cleaned = body.replace(match.group(0), " ").replaceAll("\\s+", " ").trim();
        return new ExtractResult(cleaned, rawModel, rawProfile, true);
    }
}
