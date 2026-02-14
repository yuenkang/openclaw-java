package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.concurrent.CopyOnWriteArrayList;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for the Telegram channel pipeline.
 * <p>
 * These tests exercise the full flow across multiple classes
 * (Bot creation → handler registration → update processing → context
 * building → dispatch) without hitting the real Telegram API.
 */
class TelegramChannelIntegrationTest {

    private OpenClawConfig config;
    private TelegramBot.TelegramBotContext botContext;

    @BeforeEach
    void setUp() {
        config = new OpenClawConfig();
        // Create minimal Telegram channel config
        var channels = new OpenClawConfig.ChannelsConfig();
        Map<String, Object> providers = new HashMap<>();
        Map<String, Object> telegramConfig = new HashMap<>();
        telegramConfig.put("token", "test-token-123");
        telegramConfig.put("allowFrom", List.of("*")); // allow all
        providers.put("telegram", telegramConfig);
        channels.setProviders(providers);
        config.setChannels(channels);

        // Create bot context
        TelegramBot.TelegramBotOptions opts = TelegramBot.TelegramBotOptions.builder()
                .token("test-token-123")
                .accountId("test-account")
                .config(config)
                .build();
        botContext = TelegramBot.createTelegramBot(opts);
    }

    // =========================================================================
    // Bot lifecycle
    // =========================================================================

    @Nested
    class BotLifecycleTests {

        @Test
        void createBot_setsUpContext() {
            assertNotNull(botContext);
            assertNotNull(botContext.getUpdateDedupe());
            assertNotNull(botContext.getMediaGroups());
            assertEquals(config, botContext.getConfig());
            assertFalse(botContext.isRunning());
        }

        @Test
        void startAndStopPolling() {
            assertFalse(botContext.isRunning());
            TelegramBot.startPolling(botContext);
            assertTrue(botContext.isRunning());
            TelegramBot.stop(botContext);
            assertFalse(botContext.isRunning());
        }
    }

    // =========================================================================
    // Message handling pipeline
    // =========================================================================

    @Nested
    class MessagePipelineTests {

        private TelegramBotHandlers.RegisterHandlerParams params;
        private List<Map<String, Object>> processedMessages;

        @BeforeEach
        void setUp() {
            processedMessages = new CopyOnWriteArrayList<>();
            params = TelegramBotHandlers.RegisterHandlerParams.builder()
                    .config(config)
                    .accountId("test-account")
                    .botContext(botContext)
                    .mediaMaxBytes(20 * 1024 * 1024)
                    .processMessage((msg, media) -> processedMessages.add(msg))
                    .build();
        }

        @Test
        void handleTextMessage_processesAllowedMessage() {
            Map<String, Object> update = buildTextUpdate(1001, 42, "testuser", "Hello bot!");
            TelegramBotHandlers.handleTextMessage(params, update);
            assertEquals(1, processedMessages.size());
            assertEquals("Hello bot!", processedMessages.get(0).get("text"));
        }

        @Test
        void handleTextMessage_deduplicatesRepeatUpdate() {
            Map<String, Object> update = buildTextUpdate(1001, 42, "testuser", "Hello!");
            TelegramBotHandlers.handleTextMessage(params, update);
            TelegramBotHandlers.handleTextMessage(params, update);
            // Same update_id → only processed once
            assertEquals(1, processedMessages.size());
        }

        @Test
        void handleTextMessage_differentUpdatesAreNotDeduplicated() {
            TelegramBotHandlers.handleTextMessage(params,
                    buildTextUpdate(1001, 42, "user1", "First"));
            TelegramBotHandlers.handleTextMessage(params,
                    buildTextUpdate(1002, 42, "user1", "Second"));
            assertEquals(2, processedMessages.size());
        }

        @Test
        void handleTextMessage_deniedSenderNotProcessed() {
            // Restrict access to specific user
            var channels = new OpenClawConfig.ChannelsConfig();
            Map<String, Object> providers = new HashMap<>();
            Map<String, Object> tgCfg = new HashMap<>();
            tgCfg.put("allowFrom", List.of("allowedUser"));
            providers.put("telegram", tgCfg);
            channels.setProviders(providers);

            var restrictedConfig = new OpenClawConfig();
            restrictedConfig.setChannels(channels);

            var restrictedParams = TelegramBotHandlers.RegisterHandlerParams.builder()
                    .config(restrictedConfig)
                    .accountId("test")
                    .botContext(botContext)
                    .processMessage((msg, media) -> processedMessages.add(msg))
                    .build();

            // "denieduser" is not in allow list
            TelegramBotHandlers.handleTextMessage(restrictedParams,
                    buildTextUpdate(2001, 99, "denieduser", "Blocked"));
            assertTrue(processedMessages.isEmpty());
        }

        @Test
        void handleTextMessage_nullMessageIgnored() {
            Map<String, Object> update = Map.of("update_id", 3001);
            // No "message" key in the update
            TelegramBotHandlers.handleTextMessage(params, update);
            assertTrue(processedMessages.isEmpty());
        }
    }

    // =========================================================================
    // Message context building (end-to-end)
    // =========================================================================

    @Nested
    class MessageContextTests {

        @Test
        void buildContext_privateChat() {
            Map<String, Object> msg = buildMessage(100, 42, "alice",
                    "Alice", "Smith", "Hello!", "private");

            var ctx = TelegramBotMessageContext.buildMessageContext(
                    msg, null, null, null, config, "test-account");

            assertNotNull(ctx);
            assertEquals("Hello!", ctx.getText());
            assertEquals("100", ctx.getChatId());
            assertEquals("42", ctx.getSenderId());
            assertEquals("alice", ctx.getSenderUsername());
            assertEquals("Alice", ctx.getSenderFirstName());
            assertEquals("Smith", ctx.getSenderLastName());
            assertEquals("private", ctx.getChatType());
            assertFalse(ctx.isGroup());
            assertFalse(ctx.isForum());
            assertEquals("tg:100", ctx.getSessionKey());
        }

        @Test
        void buildContext_groupChat() {
            Map<String, Object> msg = buildMessage(-100123, 42, "alice",
                    "Alice", null, "Hi group", "supergroup");

            var ctx = TelegramBotMessageContext.buildMessageContext(
                    msg, null, null, null, config, "test-account");

            assertTrue(ctx.isGroup());
            assertEquals("tg:-100123", ctx.getSessionKey());
        }

        @Test
        void buildContext_forumThreadIncludesThreadInSessionKey() {
            Map<String, Object> msg = buildMessage(-100123, 42, "alice",
                    "Alice", null, "In topic", "supergroup");
            msg.put("message_thread_id", 5);
            ((Map<String, Object>) msg.get("chat")).put("is_forum", true);

            var ctx = TelegramBotMessageContext.buildMessageContext(
                    msg, null, null, null, config, "test-account");

            assertTrue(ctx.isForum());
            assertEquals(5, ctx.getMessageThreadId());
            assertEquals("tg:-100123:5", ctx.getSessionKey());
        }

        @Test
        void buildContext_withReplyToMessage() {
            Map<String, Object> msg = buildMessage(100, 42, "alice",
                    "Alice", null, "Replying", "private");
            msg.put("reply_to_message", Map.of("message_id", 999));

            var ctx = TelegramBotMessageContext.buildMessageContext(
                    msg, null, null, null, config, "test-account");

            assertEquals("999", ctx.getReplyToMessageId());
        }

        @Test
        void buildContext_captionUsedAsTextWhenNoText() {
            Map<String, Object> msg = buildMessage(100, 42, "alice",
                    "Alice", null, null, "private");
            msg.put("caption", "Photo caption");

            var ctx = TelegramBotMessageContext.buildMessageContext(
                    msg, null, null, null, config, "test-account");

            assertEquals("Photo caption", ctx.getText());
        }

        @Test
        void buildContext_nullMessageReturnsNull() {
            var ctx = TelegramBotMessageContext.buildMessageContext(
                    null, null, null, null, config, "test-account");
            assertNull(ctx);
        }

        @Test
        void buildContext_noChatReturnsNull() {
            Map<String, Object> msg = Map.of("text", "no chat");
            var ctx = TelegramBotMessageContext.buildMessageContext(
                    msg, null, null, null, config, "test-account");
            assertNull(ctx);
        }
    }

    // =========================================================================
    // Media extraction and context integration
    // =========================================================================

    @Nested
    class MediaExtractionTests {

        @Test
        void extractMediaRef_photo() {
            Map<String, Object> msg = buildMessage(100, 42, "alice",
                    "Alice", null, null, "private");
            msg.put("photo", List.of(
                    Map.of("file_id", "small", "width", 100),
                    Map.of("file_id", "large", "width", 800)));

            var ref = TelegramBotMessageContext.extractMediaRef(msg);
            assertNotNull(ref);
            assertEquals("large", ref.getFileId()); // takes largest
            assertEquals("image/jpeg", ref.getContentType());
        }

        @Test
        void extractMediaRef_document() {
            Map<String, Object> msg = buildMessage(100, 42, "alice",
                    "Alice", null, null, "private");
            msg.put("document", Map.of(
                    "file_id", "doc1",
                    "mime_type", "application/pdf"));

            var ref = TelegramBotMessageContext.extractMediaRef(msg);
            assertNotNull(ref);
            assertEquals("doc1", ref.getFileId());
            assertEquals("application/pdf", ref.getContentType());
        }

        @Test
        void extractMediaRef_voice() {
            Map<String, Object> msg = buildMessage(100, 42, "alice",
                    "Alice", null, null, "private");
            msg.put("voice", Map.of("file_id", "voice1"));

            var ref = TelegramBotMessageContext.extractMediaRef(msg);
            assertNotNull(ref);
            assertEquals("voice1", ref.getFileId());
            assertEquals("audio/ogg", ref.getContentType());
        }

        @Test
        void extractMediaRef_sticker() {
            Map<String, Object> msg = buildMessage(100, 42, "alice",
                    "Alice", null, null, "private");
            msg.put("sticker", Map.of(
                    "file_id", "sticker1",
                    "emoji", "😀",
                    "set_name", "HappyFaces",
                    "is_animated", false,
                    "is_video", false));

            var ref = TelegramBotMessageContext.extractMediaRef(msg);
            assertNotNull(ref);
            assertEquals("sticker1", ref.getFileId());
            assertEquals("image/webp", ref.getContentType());
            assertNotNull(ref.getStickerMetadata());
            assertEquals("😀", ref.getStickerMetadata().getEmoji());
            assertEquals("HappyFaces", ref.getStickerMetadata().getSetName());
        }

        @Test
        void extractMediaRef_video() {
            Map<String, Object> msg = buildMessage(100, 42, "alice",
                    "Alice", null, null, "private");
            msg.put("video", Map.of(
                    "file_id", "vid1",
                    "mime_type", "video/mp4"));

            var ref = TelegramBotMessageContext.extractMediaRef(msg);
            assertNotNull(ref);
            assertEquals("vid1", ref.getFileId());
            assertEquals("video/mp4", ref.getContentType());
        }

        @Test
        void extractMediaRef_noMediaReturnsNull() {
            Map<String, Object> msg = buildMessage(100, 42, "alice",
                    "Alice", null, "Just text", "private");

            var ref = TelegramBotMessageContext.extractMediaRef(msg);
            assertNull(ref);
        }

        @Test
        void buildContext_mediaIsIncludedInContext() {
            Map<String, Object> msg = buildMessage(100, 42, "alice",
                    "Alice", null, "Check photo", "private");
            msg.put("photo", List.of(
                    Map.of("file_id", "ph1", "width", 800)));

            var ctx = TelegramBotMessageContext.buildMessageContext(
                    msg, null, null, null, config, "test-account");

            assertFalse(ctx.getMedia().isEmpty());
            assertEquals("ph1", ctx.getMedia().get(0).getFileId());
        }

        @Test
        void buildContext_externalMediaMergedWithInlineMedia() {
            Map<String, Object> msg = buildMessage(100, 42, "alice",
                    "Alice", null, "Multi media", "private");
            msg.put("photo", List.of(
                    Map.of("file_id", "inline_ph", "width", 800)));

            var externalRef = TelegramBotMessageContext.TelegramMediaRef.builder()
                    .fileId("ext_doc")
                    .contentType("application/pdf")
                    .build();

            var ctx = TelegramBotMessageContext.buildMessageContext(
                    msg, List.of(externalRef), null, null, config, "test-account");

            assertEquals(2, ctx.getMedia().size());
        }
    }

    // =========================================================================
    // Config resolution chain
    // =========================================================================

    @Nested
    class ConfigResolutionTests {

        @Test
        void resolveTelegramAccountConfig_resolvesFromProviders() {
            var resolved = TelegramBotHelpers.resolveTelegramAccountConfig(config, null);
            assertNotNull(resolved);
            assertEquals("test-token-123", resolved.get("token"));
        }

        @Test
        void resolveTelegramAccountConfig_nullConfigReturnsNull() {
            assertNull(TelegramBotHelpers.resolveTelegramAccountConfig(null, null));
        }

        @Test
        void resolveAllowFrom_fromConfig() {
            var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, null);
            var allowFrom = TelegramBotHelpers.resolveAllowFrom(tgConfig);
            assertEquals(List.of("*"), allowFrom);
        }

        @Test
        void buildSessionKey_privateChat() {
            assertEquals("tg:12345", TelegramBotHelpers.buildSessionKey("12345", null));
        }

        @Test
        void buildSessionKey_forumThread() {
            assertEquals("tg:12345:7",
                    TelegramBotHelpers.buildSessionKey("12345", 7));
        }

        @Test
        void resolveGroupConfig_findsMatchingChat() {
            Map<String, Object> tgConfig = new HashMap<>();
            tgConfig.put("groups", Map.of(
                    "-100999", Map.of("policy", "restricted", "model", "gpt-4o")));

            var groupCfg = TelegramBotHelpers.resolveGroupConfig(tgConfig, "-100999");
            assertNotNull(groupCfg);
            assertEquals("restricted", groupCfg.get("policy"));
        }

        @Test
        void resolveGroupConfig_noMatchReturnsNull() {
            Map<String, Object> tgConfig = Map.of(
                    "groups", Map.of("-100999", Map.of("policy", "open")));
            assertNull(TelegramBotHelpers.resolveGroupConfig(tgConfig, "-100888"));
        }

        @Test
        void resolveTopicConfig_findsThreadConfig() {
            Map<String, Object> tgConfig = new HashMap<>();
            tgConfig.put("groups", Map.of(
                    "-100999", Map.of(
                            "topics", Map.of("5", Map.of("model", "opus")))));

            var topicCfg = TelegramBotHelpers.resolveTopicConfig(tgConfig, "-100999", 5);
            assertNotNull(topicCfg);
            assertEquals("opus", topicCfg.get("model"));
        }

        @Test
        void resolveStreamMode_defaultIsDraft() {
            String mode = TelegramBotHelpers.resolveTelegramStreamMode(config, null);
            assertEquals("draft", mode);
        }

        @Test
        void resolveBotTopicsEnabled_defaultIsFalse() {
            assertFalse(TelegramBot.resolveBotTopicsEnabled(config));
        }

        @Test
        void resolveGroupPolicy_defaultIsOpen() {
            assertEquals("open", TelegramBot.resolveGroupPolicy(config, "12345"));
        }

        @Test
        void getSequentialKey_extractsChatId() {
            Map<String, Object> update = buildTextUpdate(1, 42, "user", "text");
            String key = TelegramBot.getSequentialKey(update);
            assertEquals("chat:100", key);
        }
    }

    // =========================================================================
    // Webhook path creation (integration with config)
    // =========================================================================

    @Nested
    class WebhookTests {

        @Test
        void webhookPathCreation() {
            assertEquals("/api/v1/telegram-webhook",
                    TelegramWebhook.createWebhookPath("/api/v1"));
        }
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    /**
     * Build a complete Telegram update with a text message.
     */
    private static Map<String, Object> buildTextUpdate(
            int updateId, int senderId, String username, String text) {
        Map<String, Object> update = new HashMap<>();
        update.put("update_id", updateId);
        update.put("message", buildMessage(100, senderId, username,
                "First", null, text, "private"));
        return update;
    }

    /**
     * Build a raw Telegram message map.
     */
    private static Map<String, Object> buildMessage(
            long chatId, long senderId, String username,
            String firstName, String lastName,
            String text, String chatType) {

        Map<String, Object> chat = new HashMap<>();
        chat.put("id", chatId);
        chat.put("type", chatType);

        Map<String, Object> from = new HashMap<>();
        from.put("id", senderId);
        from.put("username", username);
        from.put("first_name", firstName);
        if (lastName != null)
            from.put("last_name", lastName);

        Map<String, Object> msg = new HashMap<>();
        msg.put("message_id", 1);
        msg.put("chat", chat);
        msg.put("from", from);
        if (text != null)
            msg.put("text", text);

        return msg;
    }
}
