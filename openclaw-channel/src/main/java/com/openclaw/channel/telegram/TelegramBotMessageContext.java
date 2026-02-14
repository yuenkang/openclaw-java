package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Telegram message context building.
 * Extracts sender info, chat context, media references,
 * mention detection, and builds a context object for dispatch.
 * Corresponds to TypeScript's telegram/bot-message-context.ts.
 */
@Slf4j
public class TelegramBotMessageContext {

    // =========================================================================
    // Media reference
    // =========================================================================

    @Data
    @Builder
    public static class TelegramMediaRef {
        private String fileId;
        private String path;
        private String contentType;
        private Long fileSize;
        private StickerMetadata stickerMetadata;
    }

    @Data
    @Builder
    public static class StickerMetadata {
        private String emoji;
        private String setName;
        private boolean animated;
        private boolean video;
    }

    // =========================================================================
    // Message context
    // =========================================================================

    @Data
    @Builder
    public static class TelegramMessageContext {
        private String text;
        private String chatId;
        private String messageId;
        private String senderId;
        private String senderUsername;
        private String senderFirstName;
        private String senderLastName;
        private String chatType;
        private boolean isGroup;
        private boolean isForum;
        private Integer messageThreadId;
        private String replyToMessageId;
        private String forwardedFromName;
        @Builder.Default
        private List<TelegramMediaRef> media = new ArrayList<>();
        private boolean wasMentioned;
        private String agentId;
        private String sessionKey;
        private String accountId;
        @Builder.Default
        private Map<String, Object> locationContext = new HashMap<>();
    }

    // =========================================================================
    // Context building
    // =========================================================================

    /**
     * Build a message context from a raw Telegram message.
     */
    @SuppressWarnings("unchecked")
    public static TelegramMessageContext buildMessageContext(
            Map<String, Object> message,
            List<TelegramMediaRef> allMedia,
            List<String> storeAllowFrom,
            Map<String, Object> options,
            OpenClawConfig config,
            String accountId) {

        if (message == null)
            return null;

        // Extract basic fields
        Map<String, Object> chat = (Map<String, Object>) message.get("chat");
        Map<String, Object> from = (Map<String, Object>) message.get("from");
        if (chat == null)
            return null;

        String chatId = String.valueOf(chat.get("id"));
        String chatType = (String) chat.getOrDefault("type", "private");
        boolean isGroup = "group".equals(chatType) || "supergroup".equals(chatType);
        boolean isForum = Boolean.TRUE.equals(chat.get("is_forum"));

        String messageId = String.valueOf(message.getOrDefault("message_id", ""));
        String text = (String) message.get("text");
        String caption = (String) message.get("caption");
        if (text == null && caption != null)
            text = caption;

        // Sender info
        String senderId = from != null ? String.valueOf(from.get("id")) : null;
        String senderUsername = from != null ? (String) from.get("username") : null;
        String senderFirstName = from != null ? (String) from.get("first_name") : null;
        String senderLastName = from != null ? (String) from.get("last_name") : null;

        // Thread info
        Integer messageThreadId = null;
        Object threadId = message.get("message_thread_id");
        if (threadId instanceof Number n)
            messageThreadId = n.intValue();

        // Reply context
        String replyToMessageId = null;
        Object replyTo = message.get("reply_to_message");
        if (replyTo instanceof Map<?, ?> replyMsg) {
            Object replyMsgId = replyMsg.get("message_id");
            if (replyMsgId != null)
                replyToMessageId = String.valueOf(replyMsgId);
        }

        // Forwarded context
        String forwardedFromName = null;
        Object forwardFrom = message.get("forward_from");
        if (forwardFrom instanceof Map<?, ?> ff) {
            forwardedFromName = (String) ff.get("first_name");
        }

        // Media from message
        List<TelegramMediaRef> media = allMedia != null
                ? new ArrayList<>(allMedia)
                : new ArrayList<>();
        TelegramMediaRef msgMedia = extractMediaRef(message);
        if (msgMedia != null && !media.contains(msgMedia)) {
            media.add(msgMedia);
        }

        // Mention detection
        boolean wasMentioned = false;
        if (options != null && Boolean.TRUE.equals(options.get("forceWasMentioned"))) {
            wasMentioned = true;
        } else if (text != null) {
            // Check for @bot mention in text or entities
            wasMentioned = hasBotMention(message, config, accountId);
        }

        // Session key
        String sessionKey = TelegramBotHelpers.buildSessionKey(chatId, messageThreadId);

        return TelegramMessageContext.builder()
                .text(text)
                .chatId(chatId)
                .messageId(messageId)
                .senderId(senderId)
                .senderUsername(senderUsername)
                .senderFirstName(senderFirstName)
                .senderLastName(senderLastName)
                .chatType(chatType)
                .isGroup(isGroup)
                .isForum(isForum)
                .messageThreadId(messageThreadId)
                .replyToMessageId(replyToMessageId)
                .forwardedFromName(forwardedFromName)
                .media(media)
                .wasMentioned(wasMentioned)
                .accountId(accountId)
                .sessionKey(sessionKey)
                .build();
    }

    /**
     * Extract a media reference from a message if it contains
     * photo/document/audio/video/sticker.
     */
    @SuppressWarnings("unchecked")
    public static TelegramMediaRef extractMediaRef(Map<String, Object> message) {
        if (message == null)
            return null;

        // Photo (array of PhotoSize, take the largest)
        Object photos = message.get("photo");
        if (photos instanceof List<?> photoList && !photoList.isEmpty()) {
            Map<String, Object> largest = (Map<String, Object>) photoList.get(photoList.size() - 1);
            return TelegramMediaRef.builder()
                    .fileId((String) largest.get("file_id"))
                    .contentType("image/jpeg")
                    .build();
        }

        // Document
        Object doc = message.get("document");
        if (doc instanceof Map<?, ?> docMap) {
            return TelegramMediaRef.builder()
                    .fileId((String) docMap.get("file_id"))
                    .contentType((String) docMap.get("mime_type"))
                    .build();
        }

        // Audio
        Object audio = message.get("audio");
        if (audio instanceof Map<?, ?> audioMap) {
            String mimeType = audioMap.get("mime_type") instanceof String s ? s : "audio/mpeg";
            return TelegramMediaRef.builder()
                    .fileId((String) audioMap.get("file_id"))
                    .contentType(mimeType)
                    .build();
        }

        // Voice
        Object voice = message.get("voice");
        if (voice instanceof Map<?, ?> voiceMap) {
            String mimeType = voiceMap.get("mime_type") instanceof String s ? s : "audio/ogg";
            return TelegramMediaRef.builder()
                    .fileId((String) voiceMap.get("file_id"))
                    .contentType(mimeType)
                    .build();
        }

        // Video
        Object video = message.get("video");
        if (video instanceof Map<?, ?> videoMap) {
            String mimeType = videoMap.get("mime_type") instanceof String s ? s : "video/mp4";
            return TelegramMediaRef.builder()
                    .fileId((String) videoMap.get("file_id"))
                    .contentType(mimeType)
                    .build();
        }

        // Sticker
        Object sticker = message.get("sticker");
        if (sticker instanceof Map<?, ?> stickerMap) {
            boolean animated = Boolean.TRUE.equals(stickerMap.get("is_animated"));
            boolean videoSticker = Boolean.TRUE.equals(stickerMap.get("is_video"));
            return TelegramMediaRef.builder()
                    .fileId((String) stickerMap.get("file_id"))
                    .contentType("image/webp")
                    .stickerMetadata(StickerMetadata.builder()
                            .emoji((String) stickerMap.get("emoji"))
                            .setName((String) stickerMap.get("set_name"))
                            .animated(animated)
                            .video(videoSticker)
                            .build())
                    .build();
        }

        return null;
    }

    /**
     * Check if the bot was mentioned in the message.
     */
    @SuppressWarnings("unchecked")
    private static boolean hasBotMention(
            Map<String, Object> message, OpenClawConfig config, String accountId) {

        String text = (String) message.get("text");
        if (text == null)
            return false;

        // Get bot username from config
        var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, accountId);
        if (tgConfig == null)
            return false;

        String botUsername = (String) tgConfig.get("botUsername");
        if (botUsername == null)
            return false;

        // Check text entities for mention
        Object entities = message.get("entities");
        if (entities instanceof List<?> entityList) {
            for (Object entity : entityList) {
                if (entity instanceof Map<?, ?> e) {
                    String type = (String) e.get("type");
                    if ("mention".equals(type)) {
                        int offset = ((Number) e.get("offset")).intValue();
                        int length = ((Number) e.get("length")).intValue();
                        String mention = text.substring(offset, Math.min(offset + length, text.length()));
                        if (mention.equalsIgnoreCase("@" + botUsername))
                            return true;
                    }
                }
            }
        }

        // Fallback: text contains @botUsername
        return text.toLowerCase().contains("@" + botUsername.toLowerCase());
    }
}
