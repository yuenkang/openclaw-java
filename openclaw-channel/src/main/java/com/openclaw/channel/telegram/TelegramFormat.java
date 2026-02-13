package com.openclaw.channel.telegram;

/**
 * Telegram HTML formatting utilities for Markdown-to-HTML conversion.
 * Corresponds to TypeScript's telegram/format.ts.
 *
 * Note: Full Markdown IR parsing depends on the markdown module.
 * This class provides the HTML escaping and simple formatting utilities.
 */
public final class TelegramFormat {

    private TelegramFormat() {
    }

    /**
     * Escape HTML special characters for Telegram HTML mode.
     */
    public static String escapeHtml(String text) {
        if (text == null)
            return "";
        return text.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;");
    }

    /**
     * Escape HTML for attribute values.
     */
    public static String escapeHtmlAttr(String text) {
        return escapeHtml(text).replace("\"", "&quot;");
    }

    /**
     * Wrap text in a Telegram HTML link tag.
     */
    public static String buildLink(String href, String text) {
        if (href == null || href.isBlank())
            return text;
        return "<a href=\"" + escapeHtmlAttr(href.trim()) + "\">" + escapeHtml(text) + "</a>";
    }

    /**
     * Wrap text in bold HTML tags.
     */
    public static String bold(String text) {
        return "<b>" + text + "</b>";
    }

    /**
     * Wrap text in italic HTML tags.
     */
    public static String italic(String text) {
        return "<i>" + text + "</i>";
    }

    /**
     * Wrap text in strikethrough HTML tags.
     */
    public static String strikethrough(String text) {
        return "<s>" + text + "</s>";
    }

    /**
     * Wrap text in inline code HTML tags.
     */
    public static String code(String text) {
        return "<code>" + escapeHtml(text) + "</code>";
    }

    /**
     * Wrap text in a code block.
     */
    public static String codeBlock(String text) {
        return "<pre><code>" + escapeHtml(text) + "</code></pre>";
    }

    /**
     * Wrap text in a code block with language hint.
     */
    public static String codeBlock(String text, String language) {
        if (language == null || language.isBlank())
            return codeBlock(text);
        return "<pre><code class=\"language-" + escapeHtmlAttr(language) + "\">"
                + escapeHtml(text) + "</code></pre>";
    }
}
