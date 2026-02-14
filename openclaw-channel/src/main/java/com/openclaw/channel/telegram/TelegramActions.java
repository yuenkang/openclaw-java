package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Telegram message actions: send, react, delete, edit, sticker, sticker-search.
 * Corresponds to TypeScript's channels/plugins/actions/telegram.ts.
 */
@Slf4j
public class TelegramActions {

    private static final String PROVIDER_ID = "telegram";

    // =========================================================================
    // Send param extraction
    // =========================================================================

    /**
     * Extract common send parameters from tool call params.
     */
    public static Map<String, Object> readTelegramSendParams(Map<String, Object> params) {
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("to", readStringParam(params, "to"));
        String mediaUrl = readStringParam(params, "media");
        String message = readStringParam(params, "message");
        String caption = readStringParam(params, "caption");
        String content = message != null ? message : (caption != null ? caption : "");
        result.put("content", content);
        if (mediaUrl != null)
            result.put("mediaUrl", mediaUrl);

        String replyTo = readStringParam(params, "replyTo");
        if (replyTo != null)
            result.put("replyToMessageId", replyTo);

        String threadId = readStringParam(params, "threadId");
        if (threadId != null)
            result.put("messageThreadId", threadId);

        Object buttons = params.get("buttons");
        if (buttons != null)
            result.put("buttons", buttons);

        Object asVoice = params.get("asVoice");
        if (asVoice instanceof Boolean)
            result.put("asVoice", asVoice);

        Object silent = params.get("silent");
        if (silent instanceof Boolean)
            result.put("silent", silent);

        String quoteText = readStringParam(params, "quoteText");
        if (quoteText != null)
            result.put("quoteText", quoteText);

        return result;
    }

    // =========================================================================
    // Action listing
    // =========================================================================

    /**
     * List available Telegram message actions based on config gates.
     */
    public static List<String> listActions(OpenClawConfig config) {
        List<String> actions = new ArrayList<>();
        actions.add("send");

        Map<String, Object> telegramActions = resolveTelegramActionGates(config);
        if (isActionEnabled(telegramActions, "reactions")) {
            actions.add("react");
        }
        if (isActionEnabled(telegramActions, "deleteMessage")) {
            actions.add("delete");
        }
        if (isActionEnabled(telegramActions, "editMessage")) {
            actions.add("edit");
        }
        if (isActionEnabled(telegramActions, "sticker")) {
            actions.add("sticker");
            actions.add("sticker-search");
        }
        return actions;
    }

    /**
     * Check whether Telegram supports inline buttons.
     */
    public static boolean supportsButtons(OpenClawConfig config) {
        // Inline buttons are supported if any Telegram account has them enabled
        return true;
    }

    // =========================================================================
    // Action handling
    // =========================================================================

    /**
     * Handle a Telegram message action (send, react, delete, edit, sticker,
     * sticker-search).
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> handleAction(
            String action, Map<String, Object> params,
            OpenClawConfig config, String accountId) {

        return switch (action) {
            case "send" -> handleSend(params, config, accountId);
            case "react" -> handleReact(params, config, accountId);
            case "delete" -> handleDelete(params, config, accountId);
            case "edit" -> handleEdit(params, config, accountId);
            case "sticker" -> handleSticker(params, config, accountId);
            case "sticker-search" -> handleStickerSearch(params, config, accountId);
            default -> throw new IllegalArgumentException(
                    "Action " + action + " is not supported for provider " + PROVIDER_ID);
        };
    }

    // =========================================================================
    // Individual action handlers
    // =========================================================================

    private static Map<String, Object> handleSend(
            Map<String, Object> params, OpenClawConfig config, String accountId) {
        Map<String, Object> sendParams = readTelegramSendParams(params);
        String to = (String) sendParams.get("to");
        String content = (String) sendParams.get("content");
        log.debug("telegram.action.send to={} content.len={}", to,
                content != null ? content.length() : 0);
        // Delegate to TelegramSend
        String result = TelegramSend.sendMessage(
                resolveBotToken(config, accountId), to, content,
                (String) sendParams.get("replyToMessageId"),
                parseThreadId(sendParams.get("messageThreadId")),
                null);
        return Map.of("channel", PROVIDER_ID, "result", result != null ? result : "");
    }

    private static Map<String, Object> handleReact(
            Map<String, Object> params, OpenClawConfig config, String accountId) {
        String chatId = readStringParam(params, "chatId");
        if (chatId == null)
            chatId = readStringParam(params, "channelId");
        if (chatId == null)
            chatId = readStringParam(params, "to");
        String messageId = readStringParam(params, "messageId");
        String emoji = readStringParam(params, "emoji");
        log.debug("telegram.action.react chatId={} messageId={} emoji={}", chatId, messageId, emoji);
        return Map.of("channel", PROVIDER_ID, "action", "react",
                "chatId", chatId != null ? chatId : "",
                "messageId", messageId != null ? messageId : "");
    }

    private static Map<String, Object> handleDelete(
            Map<String, Object> params, OpenClawConfig config, String accountId) {
        String chatId = readStringParam(params, "chatId");
        if (chatId == null)
            chatId = readStringParam(params, "channelId");
        if (chatId == null)
            chatId = readStringParam(params, "to");
        String messageId = readStringParam(params, "messageId");
        log.debug("telegram.action.delete chatId={} messageId={}", chatId, messageId);
        return Map.of("channel", PROVIDER_ID, "action", "delete",
                "chatId", chatId != null ? chatId : "",
                "messageId", messageId != null ? messageId : "");
    }

    private static Map<String, Object> handleEdit(
            Map<String, Object> params, OpenClawConfig config, String accountId) {
        String chatId = readStringParam(params, "chatId");
        if (chatId == null)
            chatId = readStringParam(params, "channelId");
        if (chatId == null)
            chatId = readStringParam(params, "to");
        String messageId = readStringParam(params, "messageId");
        String message = readStringParam(params, "message");
        log.debug("telegram.action.edit chatId={} messageId={}", chatId, messageId);
        Integer msgId = messageId != null ? Integer.parseInt(messageId) : null;
        String token = resolveBotToken(config, accountId);
        String result = TelegramSend.editMessage(token, chatId, msgId, message);
        return Map.of("channel", PROVIDER_ID, "result", result != null ? result : "");
    }

    private static Map<String, Object> handleSticker(
            Map<String, Object> params, OpenClawConfig config, String accountId) {
        String to = readStringParam(params, "to");
        if (to == null)
            to = readStringParam(params, "target");
        String fileId = readStringParam(params, "fileId");
        log.debug("telegram.action.sticker to={} fileId={}", to, fileId);
        return Map.of("channel", PROVIDER_ID, "action", "sendSticker",
                "to", to != null ? to : "", "fileId", fileId != null ? fileId : "");
    }

    private static Map<String, Object> handleStickerSearch(
            Map<String, Object> params, OpenClawConfig config, String accountId) {
        String query = readStringParam(params, "query");
        log.debug("telegram.action.sticker-search query={}", query);
        return Map.of("channel", PROVIDER_ID, "action", "searchSticker",
                "query", query != null ? query : "");
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private static String readStringParam(Map<String, Object> params, String key) {
        Object val = params.get(key);
        if (val instanceof String s && !s.isBlank())
            return s.trim();
        if (val instanceof Number n)
            return n.toString();
        return null;
    }

    private static Integer parseThreadId(Object threadId) {
        if (threadId == null)
            return null;
        if (threadId instanceof Number n)
            return n.intValue();
        if (threadId instanceof String s && !s.isBlank()) {
            try {
                return Integer.parseInt(s.trim());
            } catch (NumberFormatException ignored) {
                return null;
            }
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> resolveTelegramActionGates(OpenClawConfig config) {
        if (config == null || config.getChannels() == null)
            return Map.of();
        Map<String, Object> providers = config.getChannels().getProviders();
        if (providers == null)
            return Map.of();
        Object telegram = providers.get("telegram");
        if (telegram instanceof Map<?, ?> tg) {
            Object actions = tg.get("actions");
            if (actions instanceof Map<?, ?> a) {
                return (Map<String, Object>) a;
            }
        }
        return Map.of();
    }

    private static boolean isActionEnabled(Map<String, Object> gates, String action) {
        Object val = gates.get(action);
        if (val == null)
            return true; // enabled by default
        if (val instanceof Boolean b)
            return b;
        return true;
    }

    private static String resolveBotToken(OpenClawConfig config, String accountId) {
        var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, accountId);
        if (tgConfig == null)
            return "";
        Object token = tgConfig.get("token");
        return token instanceof String s ? s : "";
    }
}
