package com.openclaw.channel.telegram;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link TelegramFormat}.
 */
class TelegramFormatTest {

    // =========================================================================
    // escapeHtml
    // =========================================================================

    @Test
    void escapeHtml_escapesSpecialCharacters() {
        assertEquals("&amp;&lt;&gt;", TelegramFormat.escapeHtml("&<>"));
    }

    @Test
    void escapeHtml_nullReturnsEmpty() {
        assertEquals("", TelegramFormat.escapeHtml(null));
    }

    @Test
    void escapeHtml_plainTextUnchanged() {
        assertEquals("Hello World", TelegramFormat.escapeHtml("Hello World"));
    }

    @Test
    void escapeHtml_mixedContent() {
        assertEquals("a &amp; b &lt;c&gt; d",
                TelegramFormat.escapeHtml("a & b <c> d"));
    }

    // =========================================================================
    // escapeHtmlAttr
    // =========================================================================

    @Test
    void escapeHtmlAttr_alsoEscapesQuotes() {
        assertEquals("&amp;&lt;&gt;&quot;",
                TelegramFormat.escapeHtmlAttr("&<>\""));
    }

    // =========================================================================
    // buildLink
    // =========================================================================

    @Test
    void buildLink_buildsAnchorTag() {
        assertEquals(
                "<a href=\"https://example.com\">Click</a>",
                TelegramFormat.buildLink("https://example.com", "Click"));
    }

    @Test
    void buildLink_blankHrefReturnsTextOnly() {
        assertEquals("Click", TelegramFormat.buildLink("", "Click"));
        assertEquals("Click", TelegramFormat.buildLink(null, "Click"));
    }

    @Test
    void buildLink_escapesHrefAndText() {
        assertEquals(
                "<a href=\"https://x.com/a&amp;b\">a &amp; b</a>",
                TelegramFormat.buildLink("https://x.com/a&b", "a & b"));
    }

    // =========================================================================
    // Inline formatting
    // =========================================================================

    @Test
    void bold_wrapsInBTags() {
        assertEquals("<b>hello</b>", TelegramFormat.bold("hello"));
    }

    @Test
    void italic_wrapsInITags() {
        assertEquals("<i>hello</i>", TelegramFormat.italic("hello"));
    }

    @Test
    void strikethrough_wrapsInSTags() {
        assertEquals("<s>hello</s>", TelegramFormat.strikethrough("hello"));
    }

    // =========================================================================
    // code / codeBlock
    // =========================================================================

    @Test
    void code_wrapsAndEscapes() {
        assertEquals("<code>a &amp; b</code>", TelegramFormat.code("a & b"));
    }

    @Test
    void codeBlock_noLanguage() {
        assertEquals("<pre><code>fn()</code></pre>",
                TelegramFormat.codeBlock("fn()"));
    }

    @Test
    void codeBlock_withLanguage() {
        assertEquals(
                "<pre><code class=\"language-java\">int x;</code></pre>",
                TelegramFormat.codeBlock("int x;", "java"));
    }

    @Test
    void codeBlock_blankLanguageFallsBackToNoLanguage() {
        assertEquals("<pre><code>x</code></pre>",
                TelegramFormat.codeBlock("x", ""));
    }
}
