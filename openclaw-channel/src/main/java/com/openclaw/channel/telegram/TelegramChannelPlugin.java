package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

/**
 * Telegram channel plugin: registers the Telegram channel with the gateway.
 * Acts as the entry point for Telegram integration.
 * Corresponds to TypeScript's telegram/index.ts.
 */
@Slf4j
public class TelegramChannelPlugin {

    public static final String CHANNEL_ID = "telegram";
    public static final String DISPLAY_NAME = "Telegram";

    /**
     * Initialize the Telegram channel plugin.
     */
    public static TelegramBot.TelegramBotContext initialize(OpenClawConfig config) {
        log.info("Initializing Telegram channel plugin");

        // Resolve the token from config
        var channels = config.getChannels();
        if (channels == null || channels.getProviders() == null) {
            log.warn("No channel providers configured, skipping Telegram");
            return null;
        }

        var providers = channels.getProviders();
        Object telegramProvider = providers.get("telegram");
        if (telegramProvider == null) {
            log.info("Telegram not configured, skipping");
            return null;
        }

        // Extract token
        String token = resolveToken(config);
        if (token == null || token.isBlank()) {
            log.warn("No Telegram bot token found, skipping Telegram initialization");
            return null;
        }

        String accountId = resolveAccountId(config);

        // Apply proxy if configured
        TelegramProxy.applyProxySettings();

        // Create bot options
        TelegramBot.TelegramBotOptions options = TelegramBot.TelegramBotOptions.builder()
                .token(token)
                .accountId(accountId)
                .config(config)
                .build();

        // Create the bot
        TelegramBot.TelegramBotContext botContext = TelegramBot.createTelegramBot(options);

        // Register native commands
        TelegramBotNativeCommands.registerNativeCommands(
                botContext, config, accountId, "reply", 4096);

        log.info("Telegram channel plugin initialized for account: {}", accountId);
        return botContext;
    }

    /**
     * Start the Telegram bot.
     */
    public static void start(TelegramBot.TelegramBotContext context) {
        if (context == null)
            return;
        TelegramBot.startPolling(context);
    }

    /**
     * Stop the Telegram bot.
     */
    public static void stop(TelegramBot.TelegramBotContext context) {
        if (context == null)
            return;
        TelegramBot.stop(context);
    }

    /**
     * Get the Telegram channel identifier.
     */
    public static String getChannelId() {
        return CHANNEL_ID;
    }

    // --- Private helpers ---

    @SuppressWarnings("unchecked")
    private static String resolveToken(OpenClawConfig config) {
        var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, null);
        if (tgConfig == null)
            return null;

        Object token = tgConfig.get("token");
        if (token instanceof String s)
            return s;

        // Check env
        String envToken = System.getenv("TELEGRAM_BOT_TOKEN");
        if (envToken != null && !envToken.isBlank())
            return envToken;

        return null;
    }

    private static String resolveAccountId(OpenClawConfig config) {
        var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, null);
        if (tgConfig == null)
            return "default";

        Object accountId = tgConfig.get("accountId");
        if (accountId instanceof String s && !s.isBlank())
            return s;

        return "default";
    }
}
