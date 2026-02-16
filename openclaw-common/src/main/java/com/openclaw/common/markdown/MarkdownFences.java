package com.openclaw.common.markdown;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Fence-span utilities for Markdown code blocks.
 * Translates TS markdown/fences.ts — parseFenceSpans / findFenceSpanAt /
 * isSafeFenceBreak.
 *
 */
public final class MarkdownFences {

    private MarkdownFences() {
    }

    /**
     * A fenced code block span within a Markdown buffer.
     */
    public record FenceSpan(int start, int end, String openLine, String marker, String indent) {
    }

    private static final Pattern FENCE_LINE = Pattern.compile("^( {0,3})(`{3,}|~{3,})(.*)$");

    /**
     * Parse all fenced code block spans from the given buffer.
     */
    public static List<FenceSpan> parseFenceSpans(String buffer) {
        if (buffer == null || buffer.isEmpty()) {
            return List.of();
        }

        List<FenceSpan> spans = new ArrayList<>();
        int openStart = -1;
        char openMarkerChar = 0;
        int openMarkerLen = 0;
        String openLine = null;
        String openMarker = null;
        String openIndent = null;

        int offset = 0;
        while (offset <= buffer.length()) {
            int nextNewline = buffer.indexOf('\n', offset);
            int lineEnd = nextNewline == -1 ? buffer.length() : nextNewline;
            String line = buffer.substring(offset, lineEnd);

            Matcher m = FENCE_LINE.matcher(line);
            if (m.matches()) {
                String indent = m.group(1);
                String marker = m.group(2);
                char markerChar = marker.charAt(0);
                int markerLen = marker.length();

                if (openStart == -1) {
                    // Open a new fence
                    openStart = offset;
                    openMarkerChar = markerChar;
                    openMarkerLen = markerLen;
                    openLine = line;
                    openMarker = marker;
                    openIndent = indent;
                } else if (openMarkerChar == markerChar && markerLen >= openMarkerLen) {
                    // Close the fence
                    spans.add(new FenceSpan(openStart, lineEnd, openLine, openMarker, openIndent));
                    openStart = -1;
                }
            }

            if (nextNewline == -1)
                break;
            offset = nextNewline + 1;
        }

        // Unclosed fence extends to end of buffer
        if (openStart != -1) {
            spans.add(new FenceSpan(openStart, buffer.length(), openLine, openMarker, openIndent));
        }

        return spans;
    }

    /**
     * Find the fence span containing the given index, or null if not inside a
     * fence.
     */
    public static FenceSpan findFenceSpanAt(List<FenceSpan> spans, int index) {
        for (var span : spans) {
            if (index > span.start() && index < span.end()) {
                return span;
            }
        }
        return null;
    }

    /**
     * Return true if the given index is NOT inside a fenced code block, making
     * it safe to break at.
     */
    public static boolean isSafeFenceBreak(List<FenceSpan> spans, int index) {
        return findFenceSpanAt(spans, index) == null;
    }
}
