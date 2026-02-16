package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.function.BiConsumer;

/**
 * Telegram handler registration for messages, callbacks, and media groups.
 * Corresponds to TypeScript's telegram/bot-handlers.ts.
 */
@Slf4j
public class TelegramBotHandlers {

    // =========================================================================
    // Handler parameters
    // =========================================================================

    @Data
    @Builder
    public static class RegisterHandlerParams {
        private OpenClawConfig config;
        private String accountId;
        private TelegramBot.TelegramBotContext botContext;
        private int mediaMaxBytes;
        @Builder.Default
        private List<String> groupAllowFrom = new ArrayList<>();
        private BiConsumer<Map<String, Object>, List<TelegramBotMessageContext.TelegramMediaRef>> processMessage;
    }

    // =========================================================================
    // Text fragment debouncing
    // =========================================================================

    /**
     * Text fragments are debounced per session key to merge rapid consecutive
     * messages.
     */
    @Data
    @Builder
    public static class TextFragmentEntry {
        private String key;
        @Builder.Default
        private List<Map<String, Object>> messages = new ArrayList<>();
        private long lastUpdatedMs;
    }

    // =========================================================================
    // Handler registration
    // =========================================================================

    /**
     * Register all Telegram message handlers on the bot context.
     */
    public static void registerHandlers(RegisterHandlerParams params) {
        log.info("Registering Telegram handlers for account: {}", params.getAccountId());

        // Media group map for batching
        Map<String, TelegramBotUpdates.MediaGroupEntry> mediaGroups = params.getBotContext().getMediaGroups();

        // Text fragment map for debouncing
        Map<String, TextFragmentEntry> textFragments = new HashMap<>();

        log.debug("Telegram handler registration complete for account: {}", params.getAccountId());
    }

    /**
     * Process a text message update.
     */
    public static void handleTextMessage(
            RegisterHandlerParams params,
            Map<String, Object> update) {

        String updateKey = TelegramBotUpdates.buildUpdateKey(update);
        if (updateKey != null && params.getBotContext().getUpdateDedupe().isDuplicate(updateKey)) {
            log.debug("Skipping duplicate update: {}", updateKey);
            return;
        }

        Map<String, Object> message = extractMessage(update);
        if (message == null)
            return;

        // NOTE: Unlike an earlier implementation that had a blanket isSenderAllowed
        // check here, the TS version does NOT do a global access check in bot-handlers.
        // Instead, DM access control (dmPolicy/pairing) happens in
        // buildTelegramMessageContext,
        // and group access control uses groupPolicy logic.
        // The Java dispatch layer handles the DM allowFrom check.

        // Process the message
        if (params.getProcessMessage() != null) {
            params.getProcessMessage().accept(message, Collections.emptyList());
        }
    }

    /**
     * Handle a media group update.
     */
    public static void handleMediaGroup(
            RegisterHandlerParams params,
            Map<String, Object> update,
            String mediaGroupId) {

        Map<String, TelegramBotUpdates.MediaGroupEntry> mediaGroups = params.getBotContext().getMediaGroups();

        TelegramBotUpdates.MediaGroupEntry entry = mediaGroups.computeIfAbsent(
                mediaGroupId,
                id -> new TelegramBotUpdates.MediaGroupEntry(id));
        entry.addMessage(extractMessage(update));

        // After a timeout, process the entire group
        long elapsed = System.currentTimeMillis() - entry.getLastUpdatedMs();
        if (elapsed >= TelegramBotUpdates.MEDIA_GROUP_TIMEOUT_MS) {
            flushMediaGroup(params, entry);
            mediaGroups.remove(mediaGroupId);
        }
    }

    /**
     * Flush a media group (all media collected).
     */
    private static void flushMediaGroup(
            RegisterHandlerParams params,
            TelegramBotUpdates.MediaGroupEntry entry) {

        if (entry.getMessages().isEmpty())
            return;

        Map<String, Object> primaryMessage = entry.getMessages().get(0);
        List<TelegramBotMessageContext.TelegramMediaRef> allMedia = new ArrayList<>();

        for (Map<String, Object> msg : entry.getMessages()) {
            TelegramBotMessageContext.TelegramMediaRef ref = TelegramBotMessageContext.extractMediaRef(msg);
            if (ref != null) {
                allMedia.add(ref);
            }
        }

        if (params.getProcessMessage() != null) {
            params.getProcessMessage().accept(primaryMessage, allMedia);
        }
    }

    /**
     * Handle a callback query update.
     * Delegates to TelegramBotMessageDispatch for inline button handling.
     */
    @SuppressWarnings("unchecked")
    public static void handleCallbackQuery(
            RegisterHandlerParams params,
            Map<String, Object> update) {

        String updateKey = TelegramBotUpdates.buildUpdateKey(update);
        if (updateKey != null && params.getBotContext().getUpdateDedupe().isDuplicate(updateKey)) {
            log.debug("Skipping duplicate callback query: {}", updateKey);
            return;
        }

        Map<String, Object> callbackQuery = (Map<String, Object>) update.get("callback_query");
        if (callbackQuery == null)
            return;

        String data = (String) callbackQuery.get("data");
        if (data == null)
            return;

        // Extract chat_id and message_id from the callback query
        Map<String, Object> message = (Map<String, Object>) callbackQuery.get("message");
        if (message == null)
            return;

        Map<String, Object> chat = (Map<String, Object>) message.get("chat");
        if (chat == null)
            return;

        String chatId = String.valueOf(chat.get("id"));
        Integer messageId = message.get("message_id") instanceof Number n ? n.intValue() : null;
        if (messageId == null)
            return;

        String token = params.getBotContext().getOptions().getToken();

        log.debug("Processing callback query: data={} chatId={} messageId={}", data, chatId, messageId);

        // Delegate to TelegramBotMessageDispatch
        boolean handled = TelegramBotMessageDispatch.handleCallbackQuery(
                token, chatId, messageId, data, params.getConfig());

        // Answer the callback query to remove the loading spinner
        String callbackQueryId = (String) callbackQuery.get("id");
        if (callbackQueryId != null) {
            try {
                TelegramFetch.callApi(token, "answerCallbackQuery",
                        "{\"callback_query_id\":\"" + callbackQueryId + "\"}");
            } catch (Exception e) {
                log.debug("Failed to answer callback query: {}", e.getMessage());
            }
        }
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static Map<String, Object> extractMessage(Map<String, Object> update) {
        Object msg = update.get("message");
        if (msg instanceof Map)
            return (Map<String, Object>) msg;
        msg = update.get("edited_message");
        if (msg instanceof Map)
            return (Map<String, Object>) msg;
        return null;
    }

    /**
     * Resolve session model override for a Telegram chat.
     */
    public static String resolveTelegramSessionModel(
            OpenClawConfig config, String chatId, boolean isGroup, boolean isForum,
            Integer messageThreadId) {

        var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, null);
        if (tgConfig == null)
            return null;

        if (isGroup) {
            var groupConfig = TelegramBotHelpers.resolveGroupConfig(tgConfig, chatId);
            if (groupConfig != null) {
                Object model = groupConfig.get("model");
                if (model instanceof String s && !s.isBlank())
                    return s;
            }

            if (isForum && messageThreadId != null) {
                var topicConfig = TelegramBotHelpers.resolveTopicConfig(
                        tgConfig, chatId, messageThreadId);
                if (topicConfig != null) {
                    Object model = topicConfig.get("model");
                    if (model instanceof String s && !s.isBlank())
                        return s;
                }
            }
        }

        return null;
    }
}
