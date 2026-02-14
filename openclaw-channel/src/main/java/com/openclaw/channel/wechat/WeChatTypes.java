package com.openclaw.channel.wechat;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * WeChat (微信) data types.
 */
public final class WeChatTypes {

    private WeChatTypes() {
    }

    // =========================================================================
    // Configuration
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class WeChatConfig {
        private String appId;
        private String appSecret;
        /** Token used for webhook signature verification. */
        private String token;
        /** AES key for message encryption (optional, 43 chars). */
        private String encodingAesKey;
    }

    // =========================================================================
    // Incoming message (parsed from XML)
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class WeChatIncomingMessage {
        /** Recipient (the Official Account's OpenID). */
        private String toUserName;
        /** Sender OpenID. */
        private String fromUserName;
        /** Message creation time (seconds since epoch). */
        private long createTime;
        /**
         * Message type: text, image, voice, video, shortvideo, location, link, event.
         */
        private String msgType;
        /** Message ID (unique per message, absent for events). */
        private String msgId;

        // --- Text ---
        private String content;

        // --- Image ---
        private String picUrl;
        private String mediaId;

        // --- Voice ---
        private String format;
        /** Speech recognition result (if enabled). */
        private String recognition;

        // --- Video / ShortVideo ---
        private String thumbMediaId;

        // --- Location ---
        private Double locationX; // latitude
        private Double locationY; // longitude
        private Integer scale;
        private String label;

        // --- Link ---
        private String title;
        private String description;
        private String url;

        // --- Event ---
        private String event; // subscribe, unsubscribe, CLICK, VIEW, LOCATION, etc.
        private String eventKey;
    }

    // =========================================================================
    // Access token
    // =========================================================================

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AccessTokenResponse {
        private String accessToken;
        private int expiresIn;
        private int errcode;
        private String errmsg;
    }
}
