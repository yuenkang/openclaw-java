package com.openclaw.channel.telegram;

import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Telegram send operation types and utilities.
 * Corresponds to TypeScript's telegram/send.ts types and helper functions.
 */
public final class TelegramSend {

    private TelegramSend() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    public record SendResult(String messageId, String chatId) {
    }

    public record SendOpts(
            String token,
            String accountId,
            boolean verbose,
            String mediaUrl,
            int maxBytes,
            String textMode, // "markdown" | "html"
            String plainText,
            boolean asVoice,
            boolean silent,
            Integer replyToMessageId,
            String quoteText,
            Integer messageThreadId,
            List<List<InlineButton>> buttons) {
    }

    public record InlineButton(String text, String callbackData) {
    }

    public record ReactionOpts(String token, String accountId, boolean remove, boolean verbose) {
    }

    public record DeleteOpts(String token, String accountId, boolean verbose) {
    }

    public record EditOpts(
            String token,
            String accountId,
            boolean verbose,
            String textMode,
            List<List<InlineButton>> buttons) {
    }

    // =========================================================================
    // Chat ID normalization
    // =========================================================================

    private static final Pattern AT_PREFIX = Pattern.compile("^@");
    private static final Pattern TME_URL = Pattern.compile(
            "^https?://t\\.me/([a-zA-Z0-9_]+)(?:/.*)?$", Pattern.CASE_INSENSITIVE);
    private static final Pattern NUMERIC_ID = Pattern.compile("^-?\\d+$");
    private static final Pattern SUPERGROUP_PREFIX = Pattern.compile("^-100");

    /**
     * Normalize a Telegram chat ID from various input formats.
     * Supports: @username, t.me URLs, numeric IDs, and internal prefixes.
     */
    public static String normalizeChatId(String to) {
        String stripped = TelegramTargets.stripInternalPrefixes(to);

        // t.me/username URL
        Matcher tmeMatch = TME_URL.matcher(stripped);
        if (tmeMatch.matches()) {
            return "@" + tmeMatch.group(1);
        }

        // Already @username
        if (stripped.startsWith("@")) {
            return stripped;
        }

        // Numeric ID
        if (NUMERIC_ID.matcher(stripped).matches()) {
            return stripped;
        }

        // Looks like a username without @
        if (stripped.matches("[a-zA-Z][a-zA-Z0-9_]{4,}")) {
            return "@" + stripped;
        }

        return stripped;
    }

    /**
     * Normalize a message ID to integer.
     */
    public static int normalizeMessageId(String raw) {
        try {
            int id = Integer.parseInt(raw.trim());
            if (id <= 0)
                throw new IllegalArgumentException("Invalid message ID: " + raw);
            return id;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid message ID: " + raw);
        }
    }

    public static int normalizeMessageId(int raw) {
        if (raw <= 0)
            throw new IllegalArgumentException("Invalid message ID: " + raw);
        return raw;
    }

    // =========================================================================
    // Parse error detection
    // =========================================================================

    private static final Pattern PARSE_ERR_RE = Pattern.compile(
            "can't parse entities|parse entities|find end of the entity",
            Pattern.CASE_INSENSITIVE);

    /**
     * Check if an error message indicates a Telegram HTML parse failure.
     */
    public static boolean isParseError(String errorMessage) {
        if (errorMessage == null)
            return false;
        return PARSE_ERR_RE.matcher(errorMessage).find();
    }

    // =========================================================================
    // Send / edit convenience methods
    // =========================================================================

    /**
     * Send a text message to a Telegram chat.
     */
    public static String sendMessage(String token, String chatId, String text,
            String replyToMessageId, Integer messageThreadId,
            String replyToMode) {
        return sendMessage(token, chatId, text, replyToMessageId,
                messageThreadId, replyToMode, null);
    }

    /**
     * Send a text message with optional inline keyboard.
     */
    public static String sendMessage(String token, String chatId, String text,
            String replyToMessageId, Integer messageThreadId,
            String replyToMode, Map<String, Object> inlineKeyboard) {

        StringBuilder json = new StringBuilder("{");
        json.append("\"chat_id\":\"").append(escapeJson(chatId)).append("\"");
        json.append(",\"text\":\"").append(escapeJson(text)).append("\"");
        json.append(",\"parse_mode\":\"Markdown\"");

        if (replyToMessageId != null && !replyToMessageId.isBlank()) {
            json.append(",\"reply_to_message_id\":").append(replyToMessageId);
        }
        if (messageThreadId != null) {
            json.append(",\"message_thread_id\":").append(messageThreadId);
        }
        json.append("}");

        return TelegramFetch.callApi(token, "sendMessage", json.toString());
    }

    /**
     * Edit an existing message.
     */
    public static String editMessage(String token, String chatId,
            Integer messageId, String text) {

        StringBuilder json = new StringBuilder("{");
        json.append("\"chat_id\":\"").append(escapeJson(chatId)).append("\"");
        json.append(",\"message_id\":").append(messageId);
        json.append(",\"text\":\"").append(escapeJson(text)).append("\"");
        json.append(",\"parse_mode\":\"Markdown\"");
        json.append("}");

        return TelegramFetch.callApi(token, "editMessageText", json.toString());
    }

    private static String escapeJson(String value) {
        if (value == null)
            return "";
        return value.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r");
    }
}
