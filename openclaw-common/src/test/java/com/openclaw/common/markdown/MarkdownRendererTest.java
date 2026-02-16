package com.openclaw.common.markdown;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for Markdown parsing and rendering.
 */
class MarkdownRendererTest {

    // -----------------------------------------------------------------------
    // MarkdownParser
    // -----------------------------------------------------------------------

    @Test
    void parse_plainText() {
        var ir = MarkdownParser.parse("Hello world");
        assertEquals("Hello world", ir.text());
        assertTrue(ir.styles().isEmpty());
        assertTrue(ir.links().isEmpty());
    }

    @Test
    void parse_bold() {
        var ir = MarkdownParser.parse("Hello **world**");
        assertEquals("Hello world", ir.text());
        assertEquals(1, ir.styles().size());
        assertEquals(MarkdownIR.Style.BOLD, ir.styles().get(0).style());
        assertEquals(6, ir.styles().get(0).start());
        assertEquals(11, ir.styles().get(0).end());
    }

    @Test
    void parse_italic() {
        var ir = MarkdownParser.parse("Hello *world*");
        assertEquals("Hello world", ir.text());
        assertTrue(ir.styles().stream().anyMatch(s -> s.style() == MarkdownIR.Style.ITALIC));
    }

    @Test
    void parse_inlineCode() {
        var ir = MarkdownParser.parse("Use `code` here");
        assertEquals("Use code here", ir.text());
        assertTrue(ir.styles().stream().anyMatch(s -> s.style() == MarkdownIR.Style.CODE));
    }

    @Test
    void parse_strikethrough() {
        var ir = MarkdownParser.parse("~~deleted~~");
        assertEquals("deleted", ir.text());
        assertTrue(ir.styles().stream().anyMatch(s -> s.style() == MarkdownIR.Style.STRIKETHROUGH));
    }

    @Test
    void parse_link() {
        var ir = MarkdownParser.parse("Click [here](https://example.com)");
        assertEquals("Click here", ir.text());
        assertEquals(1, ir.links().size());
        assertEquals("https://example.com", ir.links().get(0).href());
    }

    @Test
    void parse_heading_bold() {
        var ir = MarkdownParser.parse("# Title");
        assertEquals("Title", ir.text());
        assertTrue(ir.styles().stream().anyMatch(s -> s.style() == MarkdownIR.Style.BOLD));
    }

    @Test
    void parse_unorderedList() {
        var ir = MarkdownParser.parse("- item one\n- item two");
        assertTrue(ir.text().contains("• item one"));
        assertTrue(ir.text().contains("• item two"));
    }

    @Test
    void parse_orderedList() {
        var ir = MarkdownParser.parse("1. first\n2. second");
        assertTrue(ir.text().contains("1. first"));
        assertTrue(ir.text().contains("2. second"));
    }

    @Test
    void parse_blockquote() {
        var ir = MarkdownParser.parse("> quoted text");
        assertTrue(ir.text().contains("│ quoted text"));
    }

    @Test
    void parse_codeBlock() {
        var ir = MarkdownParser.parse("```python\nprint('hello')\n```");
        assertTrue(ir.text().contains("print('hello')"));
        assertTrue(ir.styles().stream().anyMatch(s -> s.style() == MarkdownIR.Style.CODE_BLOCK));
    }

    @Test
    void parse_nullAndEmpty() {
        assertEquals("", MarkdownParser.parse(null).text());
        assertEquals("", MarkdownParser.parse("").text());
    }

    // -----------------------------------------------------------------------
    // MarkdownRenderer
    // -----------------------------------------------------------------------

    @Test
    void render_telegramHtml_bold() {
        var ir = new MarkdownIR("Hello world",
                List.of(new MarkdownIR.StyleSpan(6, 11, MarkdownIR.Style.BOLD)),
                List.of());
        String html = MarkdownRenderer.render(ir, MarkdownRenderer.telegramHtmlOptions());
        assertEquals("Hello <b>world</b>", html);
    }

    @Test
    void render_telegramHtml_escapes() {
        var ir = MarkdownIR.plainText("a < b > c & d");
        String html = MarkdownRenderer.render(ir, MarkdownRenderer.telegramHtmlOptions());
        assertEquals("a &lt; b &gt; c &amp; d", html);
    }

    @Test
    void render_telegramHtml_link() {
        var ir = new MarkdownIR("Click here",
                List.of(),
                List.of(new MarkdownIR.LinkSpan(6, 10, "https://example.com")));
        String html = MarkdownRenderer.render(ir, MarkdownRenderer.telegramHtmlOptions());
        assertTrue(html.contains("<a href=\"https://example.com\">here</a>"));
    }

    @Test
    void render_discordMarkdown_bold() {
        var ir = new MarkdownIR("Hello world",
                List.of(new MarkdownIR.StyleSpan(6, 11, MarkdownIR.Style.BOLD)),
                List.of());
        String md = MarkdownRenderer.render(ir, MarkdownRenderer.discordMarkdownOptions());
        assertEquals("Hello **world**", md);
    }

    @Test
    void render_plainText() {
        var ir = new MarkdownIR("Hello world",
                List.of(new MarkdownIR.StyleSpan(6, 11, MarkdownIR.Style.BOLD)),
                List.of());
        assertEquals("Hello world", MarkdownRenderer.renderPlainText(ir));
    }

    @Test
    void render_empty() {
        assertEquals("", MarkdownRenderer.render(MarkdownIR.plainText(""),
                MarkdownRenderer.telegramHtmlOptions()));
    }

    // -----------------------------------------------------------------------
    // MarkdownFrontmatter
    // -----------------------------------------------------------------------

    @Test
    void frontmatter_extract() {
        String md = "---\ntitle: Hello\nauthor: Alice\n---\nBody content";
        var result = MarkdownFrontmatter.extract(md);
        assertTrue(result.hasFrontmatter());
        assertEquals("Hello", result.data().get("title"));
        assertEquals("Alice", result.data().get("author"));
        assertEquals("Body content", result.content());
    }

    @Test
    void frontmatter_noFrontmatter() {
        String md = "Just body content";
        var result = MarkdownFrontmatter.extract(md);
        assertFalse(result.hasFrontmatter());
        assertEquals("Just body content", result.content());
    }

    @Test
    void frontmatter_quotedValues() {
        String md = "---\ntitle: \"Hello World\"\n---\nBody";
        var result = MarkdownFrontmatter.extract(md);
        assertEquals("Hello World", result.data().get("title"));
    }

    @Test
    void frontmatter_hasFrontmatter() {
        assertTrue(MarkdownFrontmatter.hasFrontmatter("---\nkey: val\n---\nContent"));
        assertFalse(MarkdownFrontmatter.hasFrontmatter("No frontmatter here"));
    }

    // -----------------------------------------------------------------------
    // MarkdownCodeSpans
    // -----------------------------------------------------------------------

    @Test
    void codeSpans_inlineCode() {
        String md = "Use `code` here";
        var ranges = MarkdownCodeSpans.findCodeRanges(md);
        assertEquals(1, ranges.size());
        assertTrue(MarkdownCodeSpans.isInsideCode(md, 5)); // inside `code`
        assertFalse(MarkdownCodeSpans.isInsideCode(md, 0)); // before code
    }

    @Test
    void codeSpans_fencedBlock() {
        String md = "Before\n```\ncode block\n```\nAfter";
        var ranges = MarkdownCodeSpans.findCodeRanges(md);
        assertFalse(ranges.isEmpty());
    }

    @Test
    void codeSpans_overlaps() {
        String md = "Hello `world` there";
        assertTrue(MarkdownCodeSpans.overlapsCode(md, 5, 12));
        assertFalse(MarkdownCodeSpans.overlapsCode(md, 0, 4));
    }
}
