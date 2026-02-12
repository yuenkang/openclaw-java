package com.openclaw.agent.extensions;

import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Compaction safeguard extension — intercepts session_before_compact events,
 * summarizes outgoing context, and collects tool-failure metadata.
 * Mirrors pi-extensions/compaction-safeguard.ts.
 */
@Slf4j
public final class CompactionSafeguard {

    private CompactionSafeguard() {
    }

    private static final String FALLBACK_SUMMARY = "Summary unavailable due to context limits. Older messages were truncated.";
    private static final int MAX_TOOL_FAILURES = 8;
    private static final int MAX_TOOL_FAILURE_CHARS = 240;

    // --- Tool failure extraction ---

    public record ToolFailure(String toolCallId, String toolName, String summary, String meta) {
    }

    static String normalizeFailureText(String text) {
        return text.replaceAll("\\s+", " ").strip();
    }

    static String truncateFailureText(String text, int maxChars) {
        if (text.length() <= maxChars)
            return text;
        return text.substring(0, Math.max(0, maxChars - 3)) + "...";
    }

    @SuppressWarnings("unchecked")
    static String formatToolFailureMeta(Object details) {
        if (!(details instanceof Map<?, ?> record))
            return null;
        List<String> parts = new ArrayList<>();
        if (record.get("status") instanceof String s)
            parts.add("status=" + s);
        if (record.get("exitCode") instanceof Number n && Double.isFinite(n.doubleValue())) {
            parts.add("exitCode=" + n.intValue());
        }
        return parts.isEmpty() ? null : String.join(" ", parts);
    }

    @SuppressWarnings("unchecked")
    static String extractToolResultText(Object content) {
        if (!(content instanceof List<?> list))
            return "";
        StringBuilder sb = new StringBuilder();
        for (Object block : list) {
            if (block instanceof Map<?, ?> m) {
                if ("text".equals(m.get("type")) && m.get("text") instanceof String t) {
                    if (!sb.isEmpty())
                        sb.append('\n');
                    sb.append(t);
                }
            }
        }
        return sb.toString();
    }

    @SuppressWarnings("unchecked")
    public static List<ToolFailure> collectToolFailures(List<Map<String, Object>> messages) {
        List<ToolFailure> failures = new ArrayList<>();
        Set<String> seen = new HashSet<>();

        for (Map<String, Object> message : messages) {
            if (!"toolResult".equals(message.get("role")))
                continue;
            if (!Boolean.TRUE.equals(message.get("isError")))
                continue;
            String toolCallId = message.get("toolCallId") instanceof String s ? s : "";
            if (toolCallId.isEmpty() || seen.contains(toolCallId))
                continue;
            seen.add(toolCallId);

            String toolName = message.get("toolName") instanceof String s && !s.isBlank()
                    ? s
                    : "tool";
            String rawText = extractToolResultText(message.get("content"));
            String meta = formatToolFailureMeta(message.get("details"));
            String normalized = normalizeFailureText(rawText);
            String summary = truncateFailureText(
                    normalized.isEmpty() ? (meta != null ? "failed" : "failed (no output)") : normalized,
                    MAX_TOOL_FAILURE_CHARS);

            failures.add(new ToolFailure(toolCallId, toolName, summary, meta));
        }
        return failures;
    }

    public static String formatToolFailuresSection(List<ToolFailure> failures) {
        if (failures.isEmpty())
            return "";
        List<String> lines = failures.stream()
                .limit(MAX_TOOL_FAILURES)
                .map(f -> {
                    String meta = f.meta() != null ? " (" + f.meta() + ")" : "";
                    return "- " + f.toolName() + meta + ": " + f.summary();
                })
                .collect(Collectors.toCollection(ArrayList::new));

        if (failures.size() > MAX_TOOL_FAILURES) {
            lines.add("- ...and " + (failures.size() - MAX_TOOL_FAILURES) + " more");
        }
        return "\n\n## Tool Failures\n" + String.join("\n", lines);
    }

    // --- File operations ---

    public record FileLists(List<String> readFiles, List<String> modifiedFiles) {
    }

    public static FileLists computeFileLists(
            Set<String> read, Set<String> edited, Set<String> written) {
        Set<String> modified = new TreeSet<>();
        modified.addAll(edited);
        modified.addAll(written);
        List<String> readFiles = read.stream()
                .filter(f -> !modified.contains(f))
                .sorted()
                .toList();
        return new FileLists(readFiles, new ArrayList<>(modified));
    }

    public static String formatFileOperations(List<String> readFiles, List<String> modifiedFiles) {
        List<String> sections = new ArrayList<>();
        if (!readFiles.isEmpty()) {
            sections.add("<read-files>\n" + String.join("\n", readFiles) + "\n</read-files>");
        }
        if (!modifiedFiles.isEmpty()) {
            sections.add("<modified-files>\n" + String.join("\n", modifiedFiles) + "\n</modified-files>");
        }
        return sections.isEmpty() ? "" : "\n\n" + String.join("\n\n", sections);
    }

    /**
     * Build a fallback summary with tool failures and file operations.
     */
    public static String buildFallbackSummary(
            List<ToolFailure> toolFailures,
            String fileOpsSummary) {
        return FALLBACK_SUMMARY
                + formatToolFailuresSection(toolFailures)
                + fileOpsSummary;
    }
}
