package com.openclaw.common.markdown;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Intermediate Representation (IR) for parsed Markdown content.
 * Contains plain text with associated style spans and link spans.
 * Translates TS markdown/ir.ts type definitions.
 *
 */
public final class MarkdownIR {

    // -----------------------------------------------------------------------
    // Style types
    // -----------------------------------------------------------------------

    public enum Style {
        BOLD,
        ITALIC,
        CODE,
        CODE_BLOCK,
        STRIKETHROUGH,
        SPOILER
    }

    // -----------------------------------------------------------------------
    // Span types
    // -----------------------------------------------------------------------

    /**
     * A style applied to a range of text positions.
     */
    public record StyleSpan(int start, int end, Style style) {
        public StyleSpan {
            if (start < 0 || end < start) {
                throw new IllegalArgumentException("Invalid span range: [" + start + ", " + end + ")");
            }
        }
    }

    /**
     * A hyperlink applied to a range of text positions.
     */
    public record LinkSpan(int start, int end, String href) {
        public LinkSpan {
            if (start < 0 || end < start) {
                throw new IllegalArgumentException("Invalid span range: [" + start + ", " + end + ")");
            }
        }
    }

    // -----------------------------------------------------------------------
    // IR data
    // -----------------------------------------------------------------------

    private final String text;
    private final List<StyleSpan> styles;
    private final List<LinkSpan> links;

    public MarkdownIR(String text, List<StyleSpan> styles, List<LinkSpan> links) {
        this.text = text != null ? text : "";
        this.styles = styles != null ? List.copyOf(styles) : List.of();
        this.links = links != null ? List.copyOf(links) : List.of();
    }

    public String text() {
        return text;
    }

    public List<StyleSpan> styles() {
        return styles;
    }

    public List<LinkSpan> links() {
        return links;
    }

    // -----------------------------------------------------------------------
    // Builder
    // -----------------------------------------------------------------------

    /**
     * Builder for constructing MarkdownIR incrementally.
     */
    public static class Builder {
        private final StringBuilder text = new StringBuilder();
        private final List<StyleSpan> styles = new ArrayList<>();
        private final List<LinkSpan> links = new ArrayList<>();

        public int position() {
            return text.length();
        }

        public Builder append(String value) {
            if (value != null)
                text.append(value);
            return this;
        }

        public Builder appendChar(char ch) {
            text.append(ch);
            return this;
        }

        public Builder addStyle(int start, int end, Style style) {
            if (start < end) {
                styles.add(new StyleSpan(start, end, style));
            }
            return this;
        }

        public Builder addLink(int start, int end, String href) {
            if (start < end && href != null) {
                links.add(new LinkSpan(start, end, href));
            }
            return this;
        }

        public MarkdownIR build() {
            return new MarkdownIR(text.toString(), styles, links);
        }
    }

    public static Builder builder() {
        return new Builder();
    }

    // -----------------------------------------------------------------------
    // Convenience
    // -----------------------------------------------------------------------

    /**
     * Create a plain-text IR with no styles or links.
     */
    public static MarkdownIR plainText(String text) {
        return new MarkdownIR(text, List.of(), List.of());
    }

    // -----------------------------------------------------------------------
    // Span utilities — mirrors TS ir.ts mergeStyleSpans / clamp / slice
    // -----------------------------------------------------------------------

    /**
     * Merge adjacent or overlapping style spans of the same style.
     */
    public static List<StyleSpan> mergeStyleSpans(List<StyleSpan> spans) {
        if (spans == null || spans.size() <= 1) {
            return spans == null ? List.of() : spans;
        }
        List<StyleSpan> sorted = new ArrayList<>(spans);
        sorted.sort((a, b) -> {
            if (a.start() != b.start())
                return a.start() - b.start();
            if (a.end() != b.end())
                return a.end() - b.end();
            return a.style().name().compareTo(b.style().name());
        });
        List<StyleSpan> merged = new ArrayList<>();
        StyleSpan current = sorted.get(0);
        for (int i = 1; i < sorted.size(); i++) {
            StyleSpan next = sorted.get(i);
            if (next.style() == current.style() && next.start() <= current.end()) {
                current = new StyleSpan(current.start(), Math.max(current.end(), next.end()), current.style());
            } else {
                merged.add(current);
                current = next;
            }
        }
        merged.add(current);
        return merged;
    }

    /**
     * Clamp style spans to [0, maxLength), dropping any that become empty.
     */
    public static List<StyleSpan> clampStyleSpans(List<StyleSpan> spans, int maxLength) {
        List<StyleSpan> clamped = new ArrayList<>();
        for (var span : spans) {
            int s = Math.max(0, Math.min(span.start(), maxLength));
            int e = Math.max(s, Math.min(span.end(), maxLength));
            if (e > s) {
                clamped.add(new StyleSpan(s, e, span.style()));
            }
        }
        return clamped;
    }

    /**
     * Clamp link spans to [0, maxLength), dropping any that become empty.
     */
    public static List<LinkSpan> clampLinkSpans(List<LinkSpan> spans, int maxLength) {
        List<LinkSpan> clamped = new ArrayList<>();
        for (var span : spans) {
            int s = Math.max(0, Math.min(span.start(), maxLength));
            int e = Math.max(s, Math.min(span.end(), maxLength));
            if (e > s) {
                clamped.add(new LinkSpan(s, e, span.href()));
            }
        }
        return clamped;
    }

    /**
     * Slice style spans to [start, end) and re-map offsets to 0-based.
     */
    public static List<StyleSpan> sliceStyleSpans(List<StyleSpan> spans, int start, int end) {
        if (spans == null || spans.isEmpty())
            return List.of();
        List<StyleSpan> sliced = new ArrayList<>();
        for (var span : spans) {
            int s = Math.max(span.start(), start);
            int e = Math.min(span.end(), end);
            if (e > s) {
                sliced.add(new StyleSpan(s - start, e - start, span.style()));
            }
        }
        return mergeStyleSpans(sliced);
    }

    /**
     * Slice link spans to [start, end) and re-map offsets to 0-based.
     */
    public static List<LinkSpan> sliceLinkSpans(List<LinkSpan> spans, int start, int end) {
        if (spans == null || spans.isEmpty())
            return List.of();
        List<LinkSpan> sliced = new ArrayList<>();
        for (var span : spans) {
            int s = Math.max(span.start(), start);
            int e = Math.min(span.end(), end);
            if (e > s) {
                sliced.add(new LinkSpan(s - start, e - start, span.href()));
            }
        }
        return sliced;
    }

    // -----------------------------------------------------------------------
    // Text chunking — mirrors TS chunkText / chunkMarkdownIR
    // -----------------------------------------------------------------------

    /**
     * Split text into chunks of at most {@code limit} characters, preferring
     * newline or whitespace boundaries that are NOT inside a fenced code block.
     */
    public static List<String> chunkText(String text, int limit) {
        if (text == null || text.isEmpty())
            return List.of();
        if (limit <= 0 || text.length() <= limit)
            return List.of(text);

        // Parse fence spans once for the entire text
        var fenceSpans = MarkdownFences.parseFenceSpans(text);

        List<String> chunks = new ArrayList<>();
        String remaining = text;
        int globalOffset = 0;

        while (remaining.length() > limit) {
            String window = remaining.substring(0, limit);
            int bestBreak = -1;
            int lastNewline = -1, lastWhitespace = -1;

            for (int i = 0; i < window.length(); i++) {
                char c = window.charAt(i);
                if (c == '\n')
                    lastNewline = i;
                else if (Character.isWhitespace(c))
                    lastWhitespace = i;
            }

            // Prefer a newline break that is outside a fence
            if (lastNewline > 0 && MarkdownFences.isSafeFenceBreak(fenceSpans, globalOffset + lastNewline)) {
                bestBreak = lastNewline;
            }
            // Fall back to whitespace outside a fence
            if (bestBreak <= 0 && lastWhitespace > 0
                    && MarkdownFences.isSafeFenceBreak(fenceSpans, globalOffset + lastWhitespace)) {
                bestBreak = lastWhitespace;
            }
            // If no safe break found, use any newline/whitespace (or hard limit)
            if (bestBreak <= 0) {
                bestBreak = lastNewline > 0 ? lastNewline : lastWhitespace;
            }
            if (bestBreak <= 0)
                bestBreak = limit;

            String chunk = remaining.substring(0, bestBreak).stripTrailing();
            if (!chunk.isEmpty())
                chunks.add(chunk);

            boolean brokeOnSep = bestBreak < remaining.length() &&
                    Character.isWhitespace(remaining.charAt(bestBreak));
            int nextStart = Math.min(remaining.length(), bestBreak + (brokeOnSep ? 1 : 0));
            globalOffset += nextStart;
            remaining = remaining.substring(nextStart).stripLeading();
        }
        if (!remaining.isEmpty())
            chunks.add(remaining);
        return chunks;
    }

    /**
     * Split a MarkdownIR into chunks of at most {@code limit} characters,
     * preserving style and link span alignment.
     */
    public static List<MarkdownIR> chunkMarkdownIR(MarkdownIR ir, int limit) {
        if (ir.text().isEmpty())
            return List.of();
        if (limit <= 0 || ir.text().length() <= limit)
            return List.of(ir);

        List<String> textChunks = chunkText(ir.text(), limit);
        List<MarkdownIR> results = new ArrayList<>();
        int cursor = 0;

        for (int i = 0; i < textChunks.size(); i++) {
            String chunk = textChunks.get(i);
            if (chunk == null || chunk.isEmpty())
                continue;

            // Skip whitespace between chunks (the chunker trims)
            if (i > 0) {
                while (cursor < ir.text().length() &&
                        Character.isWhitespace(ir.text().charAt(cursor))) {
                    cursor++;
                }
            }
            int start = cursor;
            int end = Math.min(ir.text().length(), start + chunk.length());
            results.add(new MarkdownIR(
                    chunk,
                    sliceStyleSpans(ir.styles(), start, end),
                    sliceLinkSpans(ir.links(), start, end)));
            cursor = end;
        }
        return results;
    }

    @Override
    public String toString() {
        return "MarkdownIR{text=" + (text.length() > 50 ? text.substring(0, 50) + "…" : text)
                + ", styles=" + styles.size()
                + ", links=" + links.size() + "}";
    }
}
