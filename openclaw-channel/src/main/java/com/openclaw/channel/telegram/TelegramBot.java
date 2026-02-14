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
        /** Stored handler params (with processMessage callback) for use in pollLoop. */
        private TelegramBotHandlers.RegisterHandlerParams handlerParams;
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
     * Spawns a daemon thread that long-polls Telegram for updates.
     */
    public static void startPolling(TelegramBotContext ctx) {
        ctx.setRunning(true);
        String accountId = ctx.getOptions().getAccountId();
        log.info("Telegram bot polling started for account: {}", accountId);

        // Register handlers — store params in context for reuse in pollLoop
        TelegramBotHandlers.RegisterHandlerParams handlerParams = TelegramBotHandlers.RegisterHandlerParams.builder()
                .config(ctx.getConfig())
                .accountId(accountId)
                .botContext(ctx)
                .mediaMaxBytes(ctx.getOptions().getMediaMaxMb() * 1024 * 1024)
                .groupAllowFrom(ctx.getOptions().getGroupAllowFrom())
                .processMessage((message, media) -> {
                    // Build context and dispatch
                    var msgCtx = TelegramBotMessageContext.buildMessageContext(
                            message, media,
                            TelegramBotHelpers.resolveAllowFrom(
                                    TelegramBotHelpers.resolveTelegramAccountConfig(
                                            ctx.getConfig(), accountId)),
                            Map.of("botUsername", ctx.getBotUsername() != null ? ctx.getBotUsername() : "",
                                    "requireMention", ctx.getOptions().isRequireMention()),
                            ctx.getConfig(), accountId);
                    if (msgCtx != null) {
                        TelegramBotMessageDispatch.dispatch(
                                msgCtx, ctx.getConfig(), accountId,
                                ctx.getOptions().getReplyToMode(),
                                TelegramBotHelpers.resolveTelegramStreamMode(
                                        ctx.getConfig(), accountId),
                                4096,
                                ctx.getOptions().getToken());
                    }
                })
                .build();
        ctx.setHandlerParams(handlerParams);
        TelegramBotHandlers.registerHandlers(handlerParams);

        // Resolve bot username via getMe
        try {
            String getMeResponse = TelegramFetch.getMe(ctx.getOptions().getToken());
            if (getMeResponse != null) {
                var mapper = new com.fasterxml.jackson.databind.ObjectMapper();
                var root = mapper.readTree(getMeResponse);
                if (root.path("ok").asBoolean(false)) {
                    var result = root.path("result");
                    ctx.setBotUsername(result.path("username").asText(""));
                    ctx.setBotId(result.path("id").asLong(0));
                    log.info("Telegram bot identity: @{} (id={})",
                            ctx.getBotUsername(), ctx.getBotId());
                }
            }
        } catch (Exception e) {
            log.warn("Failed to resolve bot identity via getMe: {}", e.getMessage());
        }

        // Start polling thread
        Thread pollThread = new Thread(() -> pollLoop(ctx), "telegram-poll-" + accountId);
        pollThread.setDaemon(true);
        pollThread.start();
    }

    /**
     * Long-polling loop: fetches updates from Telegram and dispatches them.
     */
    @SuppressWarnings("unchecked")
    private static void pollLoop(TelegramBotContext ctx) {
        String token = ctx.getOptions().getToken();
        String accountId = ctx.getOptions().getAccountId();
        Integer offset = ctx.getOptions().getLastUpdateId();
        int errorBackoffMs = 1000;
        var mapper = new com.fasterxml.jackson.databind.ObjectMapper();

        while (ctx.isRunning()) {
            try {
                String response = TelegramFetch.getUpdates(token, offset, 30);
                if (response == null) {
                    // Network error — back off
                    log.warn("Telegram getUpdates returned null for account: {}, backing off {}ms",
                            accountId, errorBackoffMs);
                    Thread.sleep(errorBackoffMs);
                    errorBackoffMs = Math.min(errorBackoffMs * 2, 30_000);
                    continue;
                }

                // Reset backoff on success
                errorBackoffMs = 1000;

                var root = mapper.readTree(response);
                if (!root.path("ok").asBoolean(false)) {
                    log.warn("Telegram getUpdates returned ok=false for account: {}: {}",
                            accountId, root.path("description").asText(""));
                    Thread.sleep(5000);
                    continue;
                }

                var result = root.path("result");
                if (!result.isArray() || result.isEmpty()) {
                    continue;
                }

                for (var updateNode : result) {
                    try {
                        Map<String, Object> update = mapper.convertValue(updateNode,
                                new com.fasterxml.jackson.core.type.TypeReference<Map<String, Object>>() {
                                });

                        // Track offset
                        Integer updateId = TelegramBotUpdates.resolveUpdateId(update);
                        if (updateId != null) {
                            offset = updateId + 1;
                            if (ctx.getOptions().getOnUpdateId() != null) {
                                ctx.getOptions().getOnUpdateId().accept(updateId);
                            }
                        }

                        // Route update using stored handler params (includes processMessage callback)
                        TelegramBotHandlers.RegisterHandlerParams hp = ctx.getHandlerParams();
                        if (update.containsKey("message") || update.containsKey("edited_message")) {
                            Map<String, Object> msg = (Map<String, Object>) update.getOrDefault(
                                    "message", update.get("edited_message"));
                            if (msg != null) {
                                String mediaGroupId = msg.get("media_group_id") instanceof String s ? s : null;
                                if (mediaGroupId != null) {
                                    TelegramBotHandlers.handleMediaGroup(hp, update, mediaGroupId);
                                } else {
                                    TelegramBotHandlers.handleTextMessage(hp, update);
                                }
                            }
                        } else if (update.containsKey("callback_query")) {
                            TelegramBotHandlers.handleCallbackQuery(hp, update);
                        }
                    } catch (Exception e) {
                        log.error("Error processing Telegram update for account {}: {}",
                                accountId, e.getMessage(), e);
                    }
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.info("Telegram polling interrupted for account: {}", accountId);
                break;
            } catch (Exception e) {
                log.error("Telegram polling error for account {}: {}", accountId, e.getMessage(), e);
                try {
                    Thread.sleep(errorBackoffMs);
                    errorBackoffMs = Math.min(errorBackoffMs * 2, 30_000);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }
        log.info("Telegram polling loop ended for account: {}", accountId);
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
