package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

/**
 * Telegram bot main entry point.
 * Creates, configures, and starts the Telegram bot (polling or webhook mode).
 * Corresponds to TypeScript's telegram/bot.ts.
 */
@Slf4j
public class TelegramBot {

    // =========================================================================
    // Bot options
    // =========================================================================

    @Data
    @Builder
    public static class TelegramBotOptions {
        private String token;
        private String accountId;
        private boolean requireMention;
        @Builder.Default
        private List<String> allowFrom = new ArrayList<>();
        @Builder.Default
        private List<String> groupAllowFrom = new ArrayList<>();
        @Builder.Default
        private int mediaMaxMb = 20;
        @Builder.Default
        private String replyToMode = "reply";
        private OpenClawConfig config;
        private Integer lastUpdateId;
        private Consumer<Integer> onUpdateId;
    }

    // =========================================================================
    // Bot context
    // =========================================================================

    @Data
    @Builder
    public static class TelegramBotContext {
        private TelegramBotOptions options;
        private OpenClawConfig config;
        private String botUsername;
        private long botId;
        private TelegramBotUpdates.UpdateDedupe updateDedupe;
        @Builder.Default
        private Map<String, TelegramBotUpdates.MediaGroupEntry> mediaGroups = new ConcurrentHashMap<>();
        @Builder.Default
        private boolean running = false;
    }

    // =========================================================================
    // Group policy resolution
    // =========================================================================

    /**
     * Resolve group policy for a given chat.
     */
    public static String resolveGroupPolicy(OpenClawConfig config, String chatId) {
        if (config == null)
            return "open";
        var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, null);
        if (tgConfig == null)
            return "open";

        var groups = tgConfig.get("groups");
        if (groups instanceof Map<?, ?> groupsMap) {
            var chatConfig = groupsMap.get(String.valueOf(chatId));
            if (chatConfig instanceof Map<?, ?> cc) {
                Object policy = cc.get("policy");
                if (policy instanceof String s)
                    return s;
            }
        }
        return "open";
    }

    /**
     * Create and start a Telegram bot.
     */
    public static TelegramBotContext createTelegramBot(TelegramBotOptions opts) {
        log.info("Creating Telegram bot for account: {}", opts.getAccountId());

        TelegramBotUpdates.UpdateDedupe dedupe = TelegramBotUpdates.createUpdateDedupe();

        TelegramBotContext ctx = TelegramBotContext.builder()
                .options(opts)
                .config(opts.getConfig())
                .updateDedupe(dedupe)
                .build();

        log.info("Telegram bot created for account: {}", opts.getAccountId());
        return ctx;
    }

    /**
     * Start the bot in polling mode.
     */
    public static void startPolling(TelegramBotContext ctx) {
        ctx.setRunning(true);
        log.info("Telegram bot polling started for account: {}",
                ctx.getOptions().getAccountId());
        // Polling loop implemented via TelegramFetch
    }

    /**
     * Stop the bot.
     */
    public static void stop(TelegramBotContext ctx) {
        ctx.setRunning(false);
        log.info("Telegram bot stopped for account: {}",
                ctx.getOptions().getAccountId());
    }

    /**
     * Resolve whether topics/forum mode is enabled for the bot.
     */
    public static boolean resolveBotTopicsEnabled(OpenClawConfig config) {
        if (config == null)
            return false;
        var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, null);
        if (tgConfig == null)
            return false;
        Object topics = tgConfig.get("topics");
        if (topics instanceof Boolean b)
            return b;
        Object topicsEnabled = tgConfig.get("topicsEnabled");
        if (topicsEnabled instanceof Boolean b)
            return b;
        return false;
    }

    /**
     * Get the sequential key for an update (used to serialize per-chat processing).
     */
    public static String getSequentialKey(Map<String, Object> update) {
        Object message = update.get("message");
        if (message instanceof Map<?, ?> msg) {
            Object chat = msg.get("chat");
            if (chat instanceof Map<?, ?> chatMap) {
                Object id = chatMap.get("id");
                if (id != null)
                    return "chat:" + id;
            }
        }
        return "global";
    }
}
