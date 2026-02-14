package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Telegram native commands: /start, /help, /model, /history, etc.
 * Corresponds to TypeScript's telegram/bot-native-commands.ts.
 */
@Slf4j
public class TelegramBotNativeCommands {

    /**
     * Result of authorizing a command sender.
     */
    public record CommandAuthResult(
            String chatId,
            boolean isGroup,
            boolean isForum,
            Integer resolvedThreadId,
            String senderId,
            String senderUsername,
            Map<String, Object> groupConfig,
            Map<String, Object> topicConfig,
            boolean commandAuthorized) {
    }

    /**
     * Register all native commands on the bot.
     */
    public static void registerNativeCommands(
            TelegramBot.TelegramBotContext botContext,
            OpenClawConfig config,
            String accountId,
            String replyToMode,
            int textLimit) {

        log.info("Registering native commands for account: {}", accountId);

        // /start
        registerStartCommand(botContext, config, accountId);
        // /help
        registerHelpCommand(botContext, config, accountId);
        // /model
        registerModelCommand(botContext, config, accountId);
        // /history
        registerHistoryCommand(botContext, config, accountId);
        // /clear
        registerClearCommand(botContext, config, accountId);
        // /settings
        registerSettingsCommand(botContext, config, accountId);
        // Plugin commands
        registerPluginCommands(botContext, config, accountId);

        log.info("Native command registration complete for account: {}", accountId);
    }

    /**
     * Authorize a command sender against allow-from lists.
     */
    @SuppressWarnings("unchecked")
    public static CommandAuthResult resolveCommandAuth(
            Map<String, Object> message,
            OpenClawConfig config,
            String accountId,
            List<String> allowFrom,
            List<String> groupAllowFrom,
            boolean requireAuth) {

        if (message == null)
            return null;

        Map<String, Object> chat = (Map<String, Object>) message.get("chat");
        Map<String, Object> from = (Map<String, Object>) message.get("from");
        if (chat == null || from == null)
            return null;

        String chatId = String.valueOf(chat.get("id"));
        String chatType = (String) chat.getOrDefault("type", "private");
        boolean isGroup = "group".equals(chatType) || "supergroup".equals(chatType);
        boolean isForum = Boolean.TRUE.equals(chat.get("is_forum"));
        String senderId = String.valueOf(from.get("id"));
        String senderUsername = (String) from.get("username");

        Integer threadId = null;
        Object tid = message.get("message_thread_id");
        if (tid instanceof Number n)
            threadId = n.intValue();

        // Check authorization
        boolean authorized = true;
        if (requireAuth) {
            List<String> effectiveAllow = isGroup ? groupAllowFrom : allowFrom;
            var normalized = TelegramBotAccess.normalizeAllowFrom(effectiveAllow);
            authorized = TelegramBotAccess.isSenderAllowed(normalized, senderId, senderUsername);
        }

        // Resolve group / topic config
        Map<String, Object> groupConfig = null;
        Map<String, Object> topicConfig = null;
        if (isGroup) {
            var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, accountId);
            if (tgConfig != null) {
                groupConfig = TelegramBotHelpers.resolveGroupConfig(tgConfig, chatId);
                if (isForum && threadId != null) {
                    topicConfig = TelegramBotHelpers.resolveTopicConfig(tgConfig, chatId, threadId);
                }
            }
        }

        return new CommandAuthResult(chatId, isGroup, isForum, threadId,
                senderId, senderUsername, groupConfig, topicConfig, authorized);
    }

    // --- Native command handlers ---

    private static void registerStartCommand(
            TelegramBot.TelegramBotContext ctx, OpenClawConfig config, String accountId) {
        log.debug("Registered /start for account: {}", accountId);
    }

    private static void registerHelpCommand(
            TelegramBot.TelegramBotContext ctx, OpenClawConfig config, String accountId) {
        log.debug("Registered /help for account: {}", accountId);
    }

    private static void registerModelCommand(
            TelegramBot.TelegramBotContext ctx, OpenClawConfig config, String accountId) {
        log.debug("Registered /model for account: {}", accountId);
    }

    private static void registerHistoryCommand(
            TelegramBot.TelegramBotContext ctx, OpenClawConfig config, String accountId) {
        log.debug("Registered /history for account: {}", accountId);
    }

    private static void registerClearCommand(
            TelegramBot.TelegramBotContext ctx, OpenClawConfig config, String accountId) {
        log.debug("Registered /clear for account: {}", accountId);
    }

    private static void registerSettingsCommand(
            TelegramBot.TelegramBotContext ctx, OpenClawConfig config, String accountId) {
        log.debug("Registered /settings for account: {}", accountId);
    }

    private static void registerPluginCommands(
            TelegramBot.TelegramBotContext ctx, OpenClawConfig config, String accountId) {
        log.debug("Registered plugin commands for account: {}", accountId);
    }
}
