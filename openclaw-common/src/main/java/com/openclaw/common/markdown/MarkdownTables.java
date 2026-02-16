package com.openclaw.common.markdown;

import com.openclaw.common.markdown.MarkdownIR.*;

import java.util.Map;
import java.util.function.Function;

/**
 * Utility for converting Markdown tables to an alternative format
 * (bullet-lists or code-style) via the IR pipeline.
 * Translates TS markdown/tables.ts — convertMarkdownTables().
 *
 */
public final class MarkdownTables {

    private MarkdownTables() {
    }

    private static final Map<Style, MarkdownRenderer.StyleMarker> MARKDOWN_STYLE_MARKERS = Map.of(
            Style.BOLD, new MarkdownRenderer.StyleMarker("**", "**"),
            Style.ITALIC, new MarkdownRenderer.StyleMarker("_", "_"),
            Style.STRIKETHROUGH, new MarkdownRenderer.StyleMarker("~~", "~~"),
            Style.CODE, new MarkdownRenderer.StyleMarker("`", "`"),
            Style.CODE_BLOCK, new MarkdownRenderer.StyleMarker("```\n", "```"));

    /**
     * Convert any Markdown tables in the input to the requested mode
     * (bullets or code), returning the result as Markdown text without tables.
     * If there are no tables, the original markdown is returned unchanged.
     *
     * @param markdown source markdown text
     * @param mode     table mode (OFF returns input unchanged)
     * @return markdown with tables converted
     */
    public static String convertMarkdownTables(String markdown, MarkdownParser.TableMode mode) {
        if (markdown == null || markdown.isEmpty() || mode == MarkdownParser.TableMode.OFF) {
            return markdown;
        }

        MarkdownParser.ParseResult result = MarkdownParser.parseWithMeta(
                markdown,
                new MarkdownParser.ParseOptions(false, "none", "", mode, false));

        if (!result.hasTables()) {
            return markdown;
        }

        Function<LinkSpan, MarkdownRenderer.RenderLink> buildLink = link -> {
            String href = link.href().trim();
            if (href.isEmpty())
                return null;
            String label = result.ir().text().substring(link.start(), link.end());
            if (label.isEmpty())
                return null;
            return new MarkdownRenderer.RenderLink(link.start(), link.end(),
                    "[", "](" + href + ")");
        };

        MarkdownRenderer.RenderOptions opts = new MarkdownRenderer.RenderOptions(
                MARKDOWN_STYLE_MARKERS,
                text -> text, // no escaping — already markdown
                buildLink);

        return MarkdownRenderer.render(result.ir(), opts);
    }
}
