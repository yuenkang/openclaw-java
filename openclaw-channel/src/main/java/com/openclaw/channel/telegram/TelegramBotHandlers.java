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

        // Check access
        boolean allowed = TelegramBotAccess.isSenderAllowed(
                params.getConfig(), params.getAccountId(), message);
        if (!allowed) {
            log.debug("Sender not allowed for message in account: {}", params.getAccountId());
            return;
        }

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
     */
    public static void handleCallbackQuery(
            RegisterHandlerParams params,
            Map<String, Object> update) {

        String updateKey = TelegramBotUpdates.buildUpdateKey(update);
        if (updateKey != null && params.getBotContext().getUpdateDedupe().isDuplicate(updateKey)) {
            log.debug("Skipping duplicate callback query: {}", updateKey);
            return;
        }

        @SuppressWarnings("unchecked")
        Map<String, Object> callbackQuery = (Map<String, Object>) update.get("callback_query");
        if (callbackQuery == null)
            return;

        String data = (String) callbackQuery.get("data");
        if (data == null)
            return;

        log.debug("Processing callback query: data={}", data);
        // Callback query processing delegated to native commands or model buttons
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
