package com.openclaw.channel.wechat;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * WeChat channel plugin: registers the WeChat channel with the gateway.
 * Acts as the entry point for WeChat Official Account integration.
 * <p>
 * Analogous to {@code TelegramChannelPlugin}.
 */
@Slf4j
public class WeChatChannelPlugin {

    public static final String CHANNEL_ID = "wechat";
    public static final String DISPLAY_NAME = "WeChat";

    /**
     * Context holding all WeChat runtime components.
     */
    public static class WeChatContext {
        private final WeChatTypes.WeChatConfig config;
        private final WeChatAccessToken accessTokenManager;
        private final WeChatOutboundAdapter outboundAdapter;
        private final WeChatMessageHandler messageHandler;
        private final WeChatWebhookController webhookController;

        public WeChatContext(WeChatTypes.WeChatConfig config,
                WeChatAccessToken accessTokenManager,
                WeChatOutboundAdapter outboundAdapter,
                WeChatMessageHandler messageHandler,
                WeChatWebhookController webhookController) {
            this.config = config;
            this.accessTokenManager = accessTokenManager;
            this.outboundAdapter = outboundAdapter;
            this.messageHandler = messageHandler;
            this.webhookController = webhookController;
        }

        public WeChatTypes.WeChatConfig getConfig() {
            return config;
        }

        public WeChatAccessToken getAccessTokenManager() {
            return accessTokenManager;
        }

        public WeChatOutboundAdapter getOutboundAdapter() {
            return outboundAdapter;
        }

        public WeChatMessageHandler getMessageHandler() {
            return messageHandler;
        }

        public WeChatWebhookController getWebhookController() {
            return webhookController;
        }
    }

    /**
     * Initialize the WeChat channel plugin from config.
     *
     * @return WeChatContext, or null if WeChat is not configured
     */
    @SuppressWarnings("unchecked")
    public static WeChatContext initialize(OpenClawConfig config) {
        log.info("Initializing WeChat channel plugin");

        var channels = config.getChannels();
        if (channels == null || channels.getProviders() == null) {
            log.warn("No channel providers configured, skipping WeChat");
            return null;
        }

        Object wechatProvider = channels.getProviders().get("wechat");
        if (wechatProvider == null) {
            log.info("WeChat not configured, skipping");
            return null;
        }

        if (!(wechatProvider instanceof Map)) {
            log.warn("WeChat config is not a Map, skipping");
            return null;
        }

        Map<String, Object> wechatMap = (Map<String, Object>) wechatProvider;

        String appId = resolveString(wechatMap, "appId", "WECHAT_APP_ID");
        String appSecret = resolveString(wechatMap, "appSecret", "WECHAT_APP_SECRET");
        String token = resolveString(wechatMap, "token", "WECHAT_TOKEN");
        String encodingAesKey = resolveString(wechatMap, "encodingAesKey", "WECHAT_ENCODING_AES_KEY");

        if (appId == null || appSecret == null) {
            log.warn("WeChat appId or appSecret not found, skipping initialization");
            return null;
        }

        if (token == null) {
            log.warn("WeChat verification token not found, webhook verification will fail");
            token = "";
        }

        WeChatTypes.WeChatConfig wechatConfig = WeChatTypes.WeChatConfig.builder()
                .appId(appId)
                .appSecret(appSecret)
                .token(token)
                .encodingAesKey(encodingAesKey)
                .build();

        // Build components
        WeChatAccessToken accessTokenManager = new WeChatAccessToken(appId, appSecret);
        WeChatOutboundAdapter outboundAdapter = new WeChatOutboundAdapter(appId, appSecret);
        WeChatMessageHandler messageHandler = new WeChatMessageHandler(outboundAdapter);
        WeChatWebhookController webhookController = new WeChatWebhookController(token, messageHandler);

        log.info("WeChat channel plugin initialized for appId: {}", appId);

        return new WeChatContext(wechatConfig, accessTokenManager, outboundAdapter, messageHandler, webhookController);
    }

    /**
     * Get the WeChat channel identifier.
     */
    public static String getChannelId() {
        return CHANNEL_ID;
    }

    // --- Private helpers ---

    private static String resolveString(Map<String, Object> map, String key, String envKey) {
        Object value = map.get(key);
        if (value instanceof String s && !s.isBlank()) {
            return s;
        }
        // Fallback to environment variable
        String envValue = System.getenv(envKey);
        if (envValue != null && !envValue.isBlank()) {
            return envValue;
        }
        return null;
    }
}
