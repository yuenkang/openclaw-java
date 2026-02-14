package com.openclaw.channel.telegram;

import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Telegram message delivery: sends final agent replies to Telegram chats.
 * Handles text splitting, reply threading, and embed attachments.
 * Corresponds to TypeScript's telegram/bot/delivery.ts.
 */
@Slf4j
public class TelegramBotDelivery {

    private static final int TELEGRAM_TEXT_LIMIT = 4096;

    /**
     * Deliver a reply payload to a Telegram chat.
     */
    public static void deliverReplies(
            String token, String chatId,
            List<String> textParts,
            String replyToMessageId,
            Integer messageThreadId,
            String replyToMode) {

        if (textParts == null || textParts.isEmpty())
            return;

        for (int i = 0; i < textParts.size(); i++) {
            String text = textParts.get(i);
            if (text == null || text.isBlank())
                continue;

            // Split text into chunks if it exceeds the Telegram limit
            List<String> chunks = splitText(text, TELEGRAM_TEXT_LIMIT);

            for (int j = 0; j < chunks.size(); j++) {
                String replyId = (i == 0 && j == 0) ? replyToMessageId : null;
                TelegramSend.sendMessage(token, chatId, chunks.get(j),
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
            String replyToMode, Map<String, Object> inlineKeyboard) {

        if (text == null || text.isBlank())
            return;

        List<String> chunks = splitText(text, TELEGRAM_TEXT_LIMIT);
        for (int i = 0; i < chunks.size(); i++) {
            String replyId = (i == 0) ? replyToMessageId : null;
            Map<String, Object> keyboard = (i == chunks.size() - 1) ? inlineKeyboard : null;
            TelegramSend.sendMessage(token, chatId, chunks.get(i),
                    replyId, messageThreadId, replyToMode, keyboard);
        }
    }

    /**
     * Split text into chunks at paragraph or sentence boundaries.
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
