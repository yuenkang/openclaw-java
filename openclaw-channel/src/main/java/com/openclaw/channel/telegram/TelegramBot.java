package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.function.Supplier;
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
        /**
         * Supplier for fresh config with runtime overrides applied. Falls back to
         * static config.
         */
        private Supplier<OpenClawConfig> configSupplier;
        private String botUsername;
        private long botId;
        private TelegramBotUpdates.UpdateDedupe updateDedupe;
        @Builder.Default
        private Map<String, TelegramBotUpdates.MediaGroupEntry> mediaGroups = new ConcurrentHashMap<>();
        @Builder.Default
        private boolean running = false;
        /** Stored handler params (with processMessage callback) for use in pollLoop. */
        private TelegramBotHandlers.RegisterHandlerParams handlerParams;

        /** Get the latest config (from supplier if available, otherwise static). */
        public OpenClawConfig getLatestConfig() {
            if (configSupplier != null) {
                return configSupplier.get();
            }
            return config;
        }
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

    /** Return the first non-null List. */
    @SafeVarargs
    static List<String> firstDefinedList(List<String>... values) {
        for (List<String> v : values) {
            if (v != null)
                return v;
        }
        return null;
    }

    /** Return the first non-null String. */
    static String firstDefinedString(String... values) {
        for (String v : values) {
            if (v != null)
                return v;
        }
        return null;
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
                    // Reload config on each message to pick up runtime overrides
                    OpenClawConfig latestConfig = ctx.getLatestConfig();

                    // --- Group policy filtering (matches TS bot-handlers.ts L695-773) ---
                    @SuppressWarnings("unchecked")
                    Map<String, Object> chat = message.get("chat") instanceof Map
                            ? (Map<String, Object>) message.get("chat")
                            : null;
                    String chatId = chat != null ? String.valueOf(chat.get("id")) : "";
                    String chatType = chat != null
                            ? (String) chat.getOrDefault("type", "private")
                            : "private";
                    boolean isGroup = "group".equals(chatType) || "supergroup".equals(chatType);
                    boolean isForum = chat != null && Boolean.TRUE.equals(chat.get("is_forum"));
                    Integer messageThreadId = message.get("message_thread_id") instanceof Number n
                            ? n.intValue()
                            : null;

                    if (isGroup) {
                        var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(
                                latestConfig, accountId);
                        var groupConfig = TelegramBotHelpers.resolveGroupConfig(tgConfig, chatId);
                        Integer resolvedThreadId = TelegramBotHelpers.resolveForumThreadId(
                                isForum, messageThreadId);
                        var topicConfig = resolvedThreadId != null
                                ? TelegramBotHelpers.resolveTopicConfig(
                                        tgConfig, chatId, resolvedThreadId)
                                : null;

                        // 1. Group/topic enabled check
                        if (groupConfig != null && Boolean.FALSE.equals(groupConfig.get("enabled"))) {
                            log.debug("Blocked group {} (group disabled)", chatId);
                            return;
                        }
                        if (topicConfig != null && Boolean.FALSE.equals(topicConfig.get("enabled"))) {
                            log.debug("Blocked topic {} in group {} (topic disabled)",
                                    resolvedThreadId, chatId);
                            return;
                        }

                        // 2. Per-group/topic allowFrom override
                        @SuppressWarnings("unchecked")
                        List<String> groupAllowOverride = firstDefinedList(
                                topicConfig != null ? (List<String>) topicConfig.get("allowFrom") : null,
                                groupConfig != null ? (List<String>) groupConfig.get("allowFrom") : null);
                        List<String> effectiveGroupAllowFrom = groupAllowOverride != null
                                ? groupAllowOverride
                                : ctx.getOptions().getGroupAllowFrom();

                        // Merge with ownerAllowFrom (storeAllowFrom)
                        List<String> ownerAllowFrom = List.of();
                        if (latestConfig.getCommands() != null
                                && latestConfig.getCommands().getOwnerAllowFrom() != null) {
                            ownerAllowFrom = latestConfig.getCommands().getOwnerAllowFrom().stream()
                                    .map(String::valueOf).toList();
                        }
                        var effectiveGroupAllow = TelegramBotAccess.normalizeAllowFromWithStore(
                                effectiveGroupAllowFrom, ownerAllowFrom);

                        if (groupAllowOverride != null) {
                            // Per-group override: sender must be in allowFrom
                            @SuppressWarnings("unchecked")
                            Map<String, Object> from = message.get("from") instanceof Map
                                    ? (Map<String, Object>) message.get("from")
                                    : null;
                            String senderId = from != null ? String.valueOf(from.get("id")) : "";
                            String senderUsername = from != null
                                    ? (String) from.get("username")
                                    : "";
                            if (!TelegramBotAccess.isSenderAllowed(
                                    effectiveGroupAllow, senderId, senderUsername)) {
                                log.debug("Blocked group sender {} (group allowFrom override)",
                                        senderId);
                                return;
                            }
                        }

                        // 3. Group policy (open / disabled / allowlist)
                        String groupPolicy = firstDefinedString(
                                topicConfig != null ? (String) topicConfig.get("groupPolicy") : null,
                                groupConfig != null ? (String) groupConfig.get("groupPolicy") : null,
                                tgConfig != null ? (String) tgConfig.get("groupPolicy") : null,
                                "open");

                        if ("disabled".equals(groupPolicy)) {
                            log.debug("Blocked group message (groupPolicy: disabled)");
                            return;
                        }
                        if ("allowlist".equals(groupPolicy)) {
                            @SuppressWarnings("unchecked")
                            Map<String, Object> from = message.get("from") instanceof Map
                                    ? (Map<String, Object>) message.get("from")
                                    : null;
                            String senderId = from != null ? String.valueOf(from.get("id")) : null;
                            if (senderId == null) {
                                log.debug("Blocked group message (no sender, groupPolicy: allowlist)");
                                return;
                            }
                            if (!effectiveGroupAllow.hasEntries()) {
                                log.debug("Blocked group message (groupPolicy: allowlist, no entries)");
                                return;
                            }
                            String senderUsername = from != null
                                    ? (String) from.get("username")
                                    : "";
                            if (!TelegramBotAccess.isSenderAllowed(
                                    effectiveGroupAllow, senderId, senderUsername)) {
                                log.debug("Blocked group sender {} (groupPolicy: allowlist)",
                                        senderId);
                                return;
                            }
                        }

                        // 4. Group ID allowlist check
                        String resolvedPolicy = resolveGroupPolicy(latestConfig, chatId);
                        if ("allowlist".equals(resolvedPolicy)) {
                            // The group itself must be in the configured groups
                            if (groupConfig == null) {
                                log.debug("Blocked group {} (not in group allowlist)", chatId);
                                return;
                            }
                        }
                    }

                    // Build context and dispatch
                    var msgCtx = TelegramBotMessageContext.buildMessageContext(
                            message, media,
                            TelegramBotHelpers.resolveAllowFrom(
                                    TelegramBotHelpers.resolveTelegramAccountConfig(
                                            latestConfig, accountId)),
                            Map.of("botUsername", ctx.getBotUsername() != null ? ctx.getBotUsername() : "",
                                    "requireMention", ctx.getOptions().isRequireMention()),
                            latestConfig, accountId);
                    if (msgCtx != null) {
                        TelegramBotMessageDispatch.dispatch(
                                msgCtx, latestConfig, accountId,
                                ctx.getOptions().getReplyToMode(),
                                TelegramBotHelpers.resolveTelegramStreamMode(
                                        latestConfig, accountId),
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
