package com.openclaw.common.markdown;

import com.openclaw.common.markdown.MarkdownIR.*;

import java.util.*;
import java.util.function.Function;

/**
 * Renders {@link MarkdownIR} into target formats by inserting style markers and
 * links.
 * Used by channel adapters (Telegram HTML, Discord Markdown, plain text, etc.)
 * Translates TS markdown/render.ts — renderMarkdownWithMarkers().
 *
 */
public final class MarkdownRenderer {

    private MarkdownRenderer() {
    }

    // -----------------------------------------------------------------------
    // Types
    // -----------------------------------------------------------------------

    /**
     * Opening/closing markers for a style.
     */
    public record StyleMarker(String open, String close) {
    }

    /**
     * Rendered link with open/close markers.
     */
    public record RenderLink(int start, int end, String open, String close) {
    }

    /**
     * Rendering options for converting IR → target format.
     */
    public record RenderOptions(
            Map<Style, StyleMarker> styleMarkers,
            Function<String, String> escapeText,
            Function<LinkSpan, RenderLink> buildLink) {
    }

    // -----------------------------------------------------------------------
    // Style ordering
    // -----------------------------------------------------------------------

    private static final List<Style> STYLE_ORDER = List.of(
            Style.CODE_BLOCK, Style.CODE, Style.BOLD, Style.ITALIC,
            Style.STRIKETHROUGH, Style.SPOILER);

    private static int styleRank(Style style) {
        int idx = STYLE_ORDER.indexOf(style);
        return idx >= 0 ? idx : STYLE_ORDER.size();
    }

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /**
     * Render Markdown IR to a string using the given style markers and escape
     * rules.
     */
    public static String render(MarkdownIR ir, RenderOptions options) {
        String text = ir.text();
        if (text == null || text.isEmpty())
            return "";

        Map<Style, StyleMarker> markers = options.styleMarkers();

        // Filter styles to only those with markers
        List<StyleSpan> styled = ir.styles().stream()
                .filter(s -> markers.containsKey(s.style()))
                .sorted((a, b) -> {
                    if (a.start() != b.start())
                        return a.start() - b.start();
                    if (a.end() != b.end())
                        return b.end() - a.end();
                    return styleRank(a.style()) - styleRank(b.style());
                })
                .toList();

        // Collect all boundary positions
        TreeSet<Integer> boundaries = new TreeSet<>();
        boundaries.add(0);
        boundaries.add(text.length());

        Map<Integer, List<StyleSpan>> startsAt = new LinkedHashMap<>();
        for (StyleSpan span : styled) {
            if (span.start() == span.end())
                continue;
            boundaries.add(span.start());
            boundaries.add(span.end());
            startsAt.computeIfAbsent(span.start(), k -> new ArrayList<>()).add(span);
        }

        // Sort opening spans at each position
        for (List<StyleSpan> bucket : startsAt.values()) {
            bucket.sort((a, b) -> {
                if (a.end() != b.end())
                    return b.end() - a.end();
                return styleRank(a.style()) - styleRank(b.style());
            });
        }

        // Process links
        Map<Integer, List<RenderLink>> linkStarts = new LinkedHashMap<>();
        if (options.buildLink() != null) {
            for (LinkSpan link : ir.links()) {
                if (link.start() == link.end())
                    continue;
                RenderLink rl = options.buildLink().apply(link);
                if (rl == null)
                    continue;
                boundaries.add(rl.start());
                boundaries.add(rl.end());
                linkStarts.computeIfAbsent(rl.start(), k -> new ArrayList<>()).add(rl);
            }
        }

        // Iterate over boundary points
        List<Integer> points = new ArrayList<>(boundaries);
        Deque<CloseEntry> stack = new ArrayDeque<>();
        StringBuilder out = new StringBuilder();

        for (int i = 0; i < points.size(); i++) {
            int pos = points.get(i);

            // Close elements at this position (LIFO)
            while (!stack.isEmpty() && stack.peek().end == pos) {
                out.append(stack.pop().close);
            }

            // Collect opening items at this position
            List<OpenItem> openingItems = new ArrayList<>();

            List<RenderLink> openLinks = linkStarts.get(pos);
            if (openLinks != null) {
                for (int idx = 0; idx < openLinks.size(); idx++) {
                    RenderLink rl = openLinks.get(idx);
                    openingItems.add(new OpenItem(rl.end(), rl.open(), rl.close(), true, null, idx));
                }
            }

            List<StyleSpan> openStyles = startsAt.get(pos);
            if (openStyles != null) {
                for (int idx = 0; idx < openStyles.size(); idx++) {
                    StyleSpan span = openStyles.get(idx);
                    StyleMarker marker = markers.get(span.style());
                    if (marker == null)
                        continue;
                    openingItems.add(new OpenItem(span.end(), marker.open(), marker.close(),
                            false, span.style(), idx));
                }
            }

            if (!openingItems.isEmpty()) {
                // Sort: larger end first, links before styles, by rank
                openingItems.sort((a, b) -> {
                    if (a.end != b.end)
                        return b.end - a.end;
                    if (a.isLink != b.isLink)
                        return a.isLink ? -1 : 1;
                    if (!a.isLink && !b.isLink) {
                        return styleRank(a.style) - styleRank(b.style);
                    }
                    return a.index - b.index;
                });

                for (OpenItem item : openingItems) {
                    out.append(item.open);
                    stack.push(new CloseEntry(item.close, item.end));
                }
            }

            // Emit text segment
            Integer next = (i + 1 < points.size()) ? points.get(i + 1) : null;
            if (next == null)
                break;
            if (next > pos) {
                String segment = text.substring(pos, next);
                out.append(options.escapeText().apply(segment));
            }
        }

        return out.toString();
    }

    // -----------------------------------------------------------------------
    // Preset render options
    // -----------------------------------------------------------------------

    /**
     * Plain text rendering (strips all formatting).
     */
    public static String renderPlainText(MarkdownIR ir) {
        return ir.text();
    }

    /**
     * Telegram HTML rendering.
     */
    public static RenderOptions telegramHtmlOptions() {
        Map<Style, StyleMarker> markers = Map.of(
                Style.BOLD, new StyleMarker("<b>", "</b>"),
                Style.ITALIC, new StyleMarker("<i>", "</i>"),
                Style.CODE, new StyleMarker("<code>", "</code>"),
                Style.CODE_BLOCK, new StyleMarker("<pre>", "</pre>"),
                Style.STRIKETHROUGH, new StyleMarker("<s>", "</s>"),
                Style.SPOILER, new StyleMarker("<tg-spoiler>", "</tg-spoiler>"));
        return new RenderOptions(markers, MarkdownRenderer::escapeHtml,
                link -> new RenderLink(link.start(), link.end(),
                        "<a href=\"" + escapeHtml(link.href()) + "\">", "</a>"));
    }

    /**
     * Discord Markdown rendering.
     */
    public static RenderOptions discordMarkdownOptions() {
        Map<Style, StyleMarker> markers = Map.of(
                Style.BOLD, new StyleMarker("**", "**"),
                Style.ITALIC, new StyleMarker("*", "*"),
                Style.CODE, new StyleMarker("`", "`"),
                Style.CODE_BLOCK, new StyleMarker("```\n", "\n```"),
                Style.STRIKETHROUGH, new StyleMarker("~~", "~~"),
                Style.SPOILER, new StyleMarker("||", "||"));
        return new RenderOptions(markers, Function.identity(),
                link -> new RenderLink(link.start(), link.end(),
                        "[", "](" + link.href() + ")"));
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    private static String escapeHtml(String text) {
        return text.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;");
    }

    private record CloseEntry(String close, int end) {
    }

    private record OpenItem(int end, String open, String close, boolean isLink,
            Style style, int index) {
    }
}
