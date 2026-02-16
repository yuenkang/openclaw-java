package com.openclaw.common.markdown;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Detects code span positions in Markdown text, useful for knowing which parts
 * of a message should not be further processed (e.g., link expansion,
 * formatting).
 * Translates TS markdown/code-spans.ts.
 *
 */
public final class MarkdownCodeSpans {

    private MarkdownCodeSpans() {
    }

    /**
     * A range within the text that is inside a code span or code block.
     */
    public record CodeRange(int start, int end) {
        /**
         * Check if a position falls within this code range.
         */
        public boolean contains(int position) {
            return position >= start && position < end;
        }
    }

    // -----------------------------------------------------------------------
    // Patterns
    // -----------------------------------------------------------------------

    // Fenced code blocks: ```...```
    private static final Pattern FENCED_BLOCK = Pattern.compile(
            "```(?:[a-zA-Z0-9_+-]*)\\s*\\n[\\s\\S]*?```", Pattern.MULTILINE);

    // Inline code: `...` (single backtick, no nesting)
    private static final Pattern INLINE_CODE = Pattern.compile("`([^`]+)`");

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /**
     * Find all code span/block ranges in the given markdown text.
     *
     * @return sorted list of non-overlapping code ranges
     */
    public static List<CodeRange> findCodeRanges(String markdown) {
        if (markdown == null || markdown.isEmpty()) {
            return Collections.emptyList();
        }

        List<CodeRange> ranges = new ArrayList<>();

        // Fenced code blocks first (higher priority, may contain backticks)
        Matcher fenced = FENCED_BLOCK.matcher(markdown);
        while (fenced.find()) {
            ranges.add(new CodeRange(fenced.start(), fenced.end()));
        }

        // Inline code (skip if overlaps with a fenced block)
        Matcher inline = INLINE_CODE.matcher(markdown);
        while (inline.find()) {
            int start = inline.start();
            int end = inline.end();
            boolean overlaps = ranges.stream().anyMatch(r -> start < r.end() && end > r.start());
            if (!overlaps) {
                ranges.add(new CodeRange(start, end));
            }
        }

        // Sort by start position
        ranges.sort((a, b) -> a.start() - b.start());
        return Collections.unmodifiableList(ranges);
    }

    /**
     * Check if a specific position in the text is inside a code span/block.
     */
    public static boolean isInsideCode(String markdown, int position) {
        List<CodeRange> ranges = findCodeRanges(markdown);
        for (CodeRange range : ranges) {
            if (range.contains(position))
                return true;
            if (range.start() > position)
                break; // sorted, no need to check further
        }
        return false;
    }

    /**
     * Check if any part of the given range [start, end) is inside a code
     * span/block.
     */
    public static boolean overlapsCode(String markdown, int start, int end) {
        List<CodeRange> ranges = findCodeRanges(markdown);
        for (CodeRange range : ranges) {
            if (start < range.end() && end > range.start())
                return true;
            if (range.start() >= end)
                break;
        }
        return false;
    }
}
