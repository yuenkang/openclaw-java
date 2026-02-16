package com.openclaw.channel.telegram;

import com.openclaw.common.markdown.MarkdownIR;
import com.openclaw.common.markdown.MarkdownParser;
import com.openclaw.common.markdown.MarkdownParser.ParseOptions;
import com.openclaw.common.markdown.MarkdownParser.ParseResult;
import com.openclaw.common.markdown.MarkdownParser.TableMode;
import com.openclaw.common.markdown.MarkdownRenderer;
import com.openclaw.common.markdown.MarkdownRenderer.RenderOptions;
import com.openclaw.common.markdown.MarkdownTables;

import java.util.List;

/**
 * Telegram HTML formatting utilities — full Markdown IR pipeline.
 * Corresponds to TypeScript's telegram/format.ts.
 *
 * <p>
 * Pipeline: markdown → {@link MarkdownParser#parse} → {@link MarkdownIR} →
 * {@link MarkdownRenderer#render} (with Telegram HTML markers) → HTML string.
 */
public final class TelegramFormat {

    private TelegramFormat() {
    }

    /** Telegram text message limit. */
    public static final int TELEGRAM_TEXT_LIMIT = 4096;

    // ── low-level HTML helpers (kept for backward compat) ────────────

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

    public static String bold(String text) {
        return "<b>" + text + "</b>";
    }

    public static String italic(String text) {
        return "<i>" + text + "</i>";
    }

    public static String strikethrough(String text) {
        return "<s>" + text + "</s>";
    }

    public static String code(String text) {
        return "<code>" + escapeHtml(text) + "</code>";
    }

    public static String codeBlock(String text) {
        return "<pre><code>" + escapeHtml(text) + "</code></pre>";
    }

    public static String codeBlock(String text, String language) {
        if (language == null || language.isBlank())
            return codeBlock(text);
        return "<pre><code class=\"language-" + escapeHtmlAttr(language) + "\">"
                + escapeHtml(text) + "</code></pre>";
    }

    // ── IR-based pipeline (mirrors TS telegram/format.ts) ────────────

    /**
     * Render a {@link MarkdownIR} to Telegram HTML.
     */
    public static String renderTelegramHtml(MarkdownIR ir) {
        RenderOptions opts = MarkdownRenderer.telegramHtmlOptions();
        return MarkdownRenderer.render(ir, opts);
    }

    /**
     * Convert markdown text to Telegram HTML.
     * Mirrors TS {@code markdownToTelegramHtml()}.
     *
     * @param markdown  raw markdown
     * @param tableMode "bullets", "code", or null for default
     */
    public static String markdownToTelegramHtml(String markdown, String tableMode) {
        if (markdown == null || markdown.isEmpty())
            return "";

        // Auto-detect tables: if no explicit tableMode, check with parseWithMeta
        // and convert tables to bullets via MarkdownTables
        TableMode tm = resolveTableMode(tableMode);
        String processed = markdown;
        if (tm == TableMode.OFF) {
            ParseResult meta = MarkdownParser.parseWithMeta(markdown,
                    new ParseOptions(true, "none", "", TableMode.BULLETS, false));
            if (meta.hasTables()) {
                processed = MarkdownTables.convertMarkdownTables(markdown, TableMode.BULLETS);
            }
        }

        MarkdownIR ir = MarkdownParser.parse(processed, new ParseOptions(
                true, "none", "", tm, false));
        return renderTelegramHtml(ir);
    }

    /** Convenience overload with default table mode. */
    public static String markdownToTelegramHtml(String markdown) {
        return markdownToTelegramHtml(markdown, null);
    }

    /**
     * A formatted chunk with both HTML and plain-text representations.
     */
    public record FormattedChunk(String html, String text) {
    }

    /**
     * Convert markdown text to chunked Telegram HTML.
     * Uses IR-level chunking for fence-aware, span-preserving splits.
     * Mirrors TS {@code markdownToTelegramChunks()}.
     *
     * @param markdown  raw markdown
     * @param limit     character limit per chunk
     * @param tableMode "bullets", "code", or null
     */
    public static List<FormattedChunk> markdownToTelegramChunks(
            String markdown, int limit, String tableMode) {
        if (markdown == null || markdown.isEmpty())
            return List.of();

        TableMode tm = resolveTableMode(tableMode);
        MarkdownIR ir = MarkdownParser.parse(markdown, new ParseOptions(
                true, "none", "", tm, false));

        List<MarkdownIR> chunks = MarkdownIR.chunkMarkdownIR(ir, limit);
        return chunks.stream()
                .map(chunk -> new FormattedChunk(renderTelegramHtml(chunk), chunk.text()))
                .toList();
    }

    /** Convenience: return only HTML strings. */
    public static List<String> markdownToTelegramHtmlChunks(
            String markdown, int limit, String tableMode) {
        return markdownToTelegramChunks(markdown, limit, tableMode).stream()
                .map(FormattedChunk::html)
                .toList();
    }

    /**
     * Render text respecting textMode (markdown or html pass-through).
     * Mirrors TS {@code renderTelegramHtmlText()}.
     */
    public static String renderTelegramHtmlText(String text, String textMode, String tableMode) {
        if ("html".equals(textMode))
            return text;
        return markdownToTelegramHtml(text, tableMode);
    }

    // ── helpers ──────────────────────────────────────────────

    private static TableMode resolveTableMode(String mode) {
        if (mode == null)
            return TableMode.OFF;
        return switch (mode.toLowerCase()) {
            case "bullets" -> TableMode.BULLETS;
            case "code" -> TableMode.CODE;
            default -> TableMode.OFF;
        };
    }
}
