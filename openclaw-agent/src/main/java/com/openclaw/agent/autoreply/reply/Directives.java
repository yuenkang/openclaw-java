package com.openclaw.agent.autoreply.reply;

import com.openclaw.agent.autoreply.ThinkingLevels;

import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Inline directive extraction from message body (/thinking, /verbose, /notice,
 * /elevated, /reasoning, /status).
 * Mirrors {@code auto-reply/reply/directives.ts}.
 */
public final class Directives {

    private Directives() {
    }

    /** Result of extracting a level directive. */
    public record ExtractedLevel<T>(String cleaned, T level, String rawLevel, boolean hasDirective) {
    }

    private static String escapeRegExp(String value) {
        return value.replaceAll("[.*+?^${}()|\\[\\]\\\\]", "\\\\$0");
    }

    private record MatchResult(int start, int end, String rawLevel) {
    }

    private static MatchResult matchLevelDirective(String body, List<String> names) {
        String namePattern = names.stream().map(Directives::escapeRegExp).reduce((a, b) -> a + "|" + b).orElse("");
        Pattern re = Pattern.compile("(?:^|\\s)/(?:" + namePattern + ")(?=$|\\s|:)", Pattern.CASE_INSENSITIVE);
        Matcher m = re.matcher(body);
        if (!m.find())
            return null;
        int start = m.start();
        int end = m.end();
        int i = end;
        while (i < body.length() && Character.isWhitespace(body.charAt(i)))
            i++;
        if (i < body.length() && body.charAt(i) == ':') {
            i++;
            while (i < body.length() && Character.isWhitespace(body.charAt(i)))
                i++;
        }
        int argStart = i;
        while (i < body.length() && (Character.isLetter(body.charAt(i)) || body.charAt(i) == '-'))
            i++;
        String rawLevel = i > argStart ? body.substring(argStart, i) : null;
        return new MatchResult(start, i, rawLevel);
    }

    private static <T> ExtractedLevel<T> extractLevelDirective(String body, List<String> names,
            Function<String, T> normalize) {
        MatchResult match = matchLevelDirective(body, names);
        if (match == null)
            return new ExtractedLevel<>(body.trim(), null, null, false);
        T level = normalize.apply(match.rawLevel);
        String cleaned = (body.substring(0, match.start) + " " + body.substring(match.end))
                .replaceAll("\\s+", " ").trim();
        return new ExtractedLevel<>(cleaned, level, match.rawLevel, true);
    }

    private static ExtractedLevel<Void> extractSimpleDirective(String body, List<String> names) {
        String namePattern = names.stream().map(Directives::escapeRegExp).reduce((a, b) -> a + "|" + b).orElse("");
        Pattern re = Pattern.compile("(?:^|\\s)/(?:" + namePattern + ")(?=$|\\s|:)(?:\\s*:\\s*)?",
                Pattern.CASE_INSENSITIVE);
        Matcher match = re.matcher(body);
        boolean found = match.find();
        String cleaned = found ? match.replaceFirst(" ").replaceAll("\\s+", " ").trim() : body.trim();
        return new ExtractedLevel<>(cleaned, null, null, found);
    }

    // ── Public extractors ───────────────────────────────────────────

    public static ExtractedLevel<String> extractThinkDirective(String body) {
        if (body == null || body.isEmpty())
            return new ExtractedLevel<>("", null, null, false);
        return extractLevelDirective(body, List.of("thinking", "think", "t"), ThinkingLevels::normalizeThinkLevel);
    }

    public static ExtractedLevel<String> extractVerboseDirective(String body) {
        if (body == null || body.isEmpty())
            return new ExtractedLevel<>("", null, null, false);
        return extractLevelDirective(body, List.of("verbose", "v"), ThinkingLevels::normalizeVerboseLevel);
    }

    public static ExtractedLevel<String> extractNoticeDirective(String body) {
        if (body == null || body.isEmpty())
            return new ExtractedLevel<>("", null, null, false);
        return extractLevelDirective(body, List.of("notice", "notices"), ThinkingLevels::normalizeNoticeLevel);
    }

    public static ExtractedLevel<String> extractElevatedDirective(String body) {
        if (body == null || body.isEmpty())
            return new ExtractedLevel<>("", null, null, false);
        return extractLevelDirective(body, List.of("elevated", "elev"), ThinkingLevels::normalizeElevatedLevel);
    }

    public static ExtractedLevel<String> extractReasoningDirective(String body) {
        if (body == null || body.isEmpty())
            return new ExtractedLevel<>("", null, null, false);
        return extractLevelDirective(body, List.of("reasoning", "reason"), ThinkingLevels::normalizeReasoningLevel);
    }

    /** Result of extracting a simple directive (no level). */
    public record SimpleDirectiveResult(String cleaned, boolean hasDirective) {
    }

    public static SimpleDirectiveResult extractStatusDirective(String body) {
        if (body == null || body.isEmpty())
            return new SimpleDirectiveResult("", false);
        ExtractedLevel<Void> result = extractSimpleDirective(body, List.of("status"));
        return new SimpleDirectiveResult(result.cleaned(), result.hasDirective());
    }
}
