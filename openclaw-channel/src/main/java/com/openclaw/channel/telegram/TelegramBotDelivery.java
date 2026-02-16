package com.openclaw.channel.telegram;

import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Telegram message delivery: sends final agent replies to Telegram chats.
 * Handles markdown→HTML conversion, IR-based chunking, and reply threading.
 * Corresponds to TypeScript's telegram/bot/delivery.ts.
 */
@Slf4j
public class TelegramBotDelivery {

    private static final int TELEGRAM_TEXT_LIMIT = TelegramFormat.TELEGRAM_TEXT_LIMIT;

    /**
     * Deliver a reply payload to a Telegram chat.
     * Converts each text part from markdown to Telegram HTML, then chunks via the
     * IR pipeline for fence-aware, span-preserving splits.
     */
    public static void deliverReplies(
            String token, String chatId,
            List<String> textParts,
            String replyToMessageId,
            Integer messageThreadId,
            String replyToMode) {
        deliverReplies(token, chatId, textParts, replyToMessageId,
                messageThreadId, replyToMode, null);
    }

    /**
     * Deliver a reply payload with optional table mode.
     */
    public static void deliverReplies(
            String token, String chatId,
            List<String> textParts,
            String replyToMessageId,
            Integer messageThreadId,
            String replyToMode,
            String tableMode) {

        if (textParts == null || textParts.isEmpty())
            return;

        boolean firstMessage = true;
        for (String text : textParts) {
            if (text == null || text.isBlank())
                continue;

            // Use the IR pipeline: markdown → IR → chunk → render HTML
            List<String> htmlChunks = TelegramFormat.markdownToTelegramHtmlChunks(
                    text, TELEGRAM_TEXT_LIMIT, tableMode);

            if (htmlChunks.isEmpty()) {
                // Fallback: send as plain text if IR pipeline returns empty
                htmlChunks = splitText(text, TELEGRAM_TEXT_LIMIT);
            }

            for (String chunk : htmlChunks) {
                String replyId = firstMessage ? replyToMessageId : null;
                firstMessage = false;
                TelegramSend.sendMessage(token, chatId, chunk,
                        replyId, messageThreadId, replyToMode);
            }
        }
    }

    /**
     * Deliver a single reply with optional inline keyboard.
     */
    public static void deliverReply(
            String token, String chatId, String text,
            String replyToMessageId, Integer messageThreadId,
            String replyToMode, String replyMarkup) {

        if (text == null || text.isBlank())
            return;

        List<String> htmlChunks = TelegramFormat.markdownToTelegramHtmlChunks(
                text, TELEGRAM_TEXT_LIMIT, null);

        if (htmlChunks.isEmpty()) {
            htmlChunks = splitText(text, TELEGRAM_TEXT_LIMIT);
        }

        for (int i = 0; i < htmlChunks.size(); i++) {
            String replyId = (i == 0) ? replyToMessageId : null;
            // Only attach keyboard to the last chunk
            String keyboard = (i == htmlChunks.size() - 1) ? replyMarkup : null;
            TelegramSend.sendMessage(token, chatId, htmlChunks.get(i),
                    replyId, messageThreadId, replyToMode, keyboard);
        }
    }

    /**
     * Fallback text splitter at paragraph or newline boundaries.
     * Used only when the IR pipeline returns empty results.
     */
    public static List<String> splitText(String text, int maxLength) {
        if (text.length() <= maxLength) {
            return List.of(text);
        }

        List<String> chunks = new ArrayList<>();
        int start = 0;

        while (start < text.length()) {
            int end = Math.min(start + maxLength, text.length());

            if (end < text.length()) {
                // Try to split at a paragraph break
                int lastParagraph = text.lastIndexOf("\n\n", end);
                if (lastParagraph > start) {
                    end = lastParagraph;
                } else {
                    // Try to split at a newline
                    int lastNewline = text.lastIndexOf("\n", end);
                    if (lastNewline > start) {
                        end = lastNewline;
                    }
                }
            }

            chunks.add(text.substring(start, end).trim());
            start = end;
            // Skip whitespace
            while (start < text.length() && Character.isWhitespace(text.charAt(start))) {
                start++;
            }
        }

        return chunks;
    }
}
