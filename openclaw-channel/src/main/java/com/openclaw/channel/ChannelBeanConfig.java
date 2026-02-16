package com.openclaw.channel;

import com.openclaw.channel.delivery.MessageDeliveryService;
import com.openclaw.channel.dock.ChannelDock;
import com.openclaw.channel.registry.ChannelRegistry;
import com.openclaw.channel.routing.TargetResolver;
import com.openclaw.channel.telegram.TelegramBot;
import com.openclaw.channel.telegram.TelegramChannelPlugin;
import com.openclaw.channel.telegram.TelegramOutboundAdapter;
import com.openclaw.channel.wechat.WeChatChannelPlugin;
import com.openclaw.channel.wechat.WeChatOutboundAdapter;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Spring configuration for Channel beans.
 */
@Slf4j
@Configuration
public class ChannelBeanConfig {

    private final ConfigService configService;

    public ChannelBeanConfig(ConfigService configService) {
        this.configService = configService;
    }

    @Bean
    public ChannelRegistry channelRegistry() {
        return new ChannelRegistry();
    }

    @Bean
    public ChannelDock channelDock() {
        return new ChannelDock();
    }

    @Bean
    public TargetResolver targetResolver() {
        return new TargetResolver();
    }

    @Bean
    public MessageDeliveryService messageDeliveryService(ChannelDock channelDock) {
        MessageDeliveryService service = new MessageDeliveryService(channelDock);

        // Auto-register Telegram if bot token is configured
        String tgToken = null;

        // 1) Try config providers map
        try {
            OpenClawConfig config = configService.loadConfig();
            if (config.getChannels() != null && config.getChannels().getProviders() != null) {
                Object tgProvider = config.getChannels().getProviders().get("telegram");
                if (tgProvider instanceof java.util.Map) {
                    @SuppressWarnings("unchecked")
                    java.util.Map<String, Object> tgMap = (java.util.Map<String, Object>) tgProvider;
                    Object token = tgMap.get("botToken");
                    if (token == null)
                        token = tgMap.get("token");
                    if (token instanceof String s && !s.isBlank()) {
                        tgToken = s;
                    }
                }
            }
        } catch (Exception e) {
            log.debug("No Telegram config found: {}", e.getMessage());
        }

        // 2) Fallback to env variable
        if (tgToken == null) {
            String envToken = System.getenv("TELEGRAM_BOT_TOKEN");
            if (envToken != null && !envToken.isBlank()) {
                tgToken = envToken;
            }
        }

        if (tgToken != null) {
            service.registerAdapter(new TelegramOutboundAdapter(tgToken));
        }

        // Auto-register WeChat if configured
        try {
            OpenClawConfig cfg = configService.loadConfig();
            if (cfg.getChannels() != null && cfg.getChannels().getProviders() != null) {
                Object wechatProvider = cfg.getChannels().getProviders().get("wechat");
                if (wechatProvider instanceof java.util.Map) {
                    @SuppressWarnings("unchecked")
                    java.util.Map<String, Object> wechatMap = (java.util.Map<String, Object>) wechatProvider;
                    String appId = wechatMap.get("appId") instanceof String s ? s : System.getenv("WECHAT_APP_ID");
                    String appSecret = wechatMap.get("appSecret") instanceof String s ? s
                            : System.getenv("WECHAT_APP_SECRET");
                    if (appId != null && !appId.isBlank() && appSecret != null && !appSecret.isBlank()) {
                        service.registerAdapter(new WeChatOutboundAdapter(appId, appSecret));
                    }
                }
            }
        } catch (Exception e) {
            log.debug("No WeChat config found: {}", e.getMessage());
        }

        return service;
    }

    /**
     * Initialize channel plugins (Telegram bot polling, WeChat plugin) at startup
     * and cleanly shut them down when the application stops.
     */
    @Bean
    public ChannelPluginLifecycle channelPluginLifecycle() {
        OpenClawConfig config;
        try {
            config = configService.loadConfig();
        } catch (Exception e) {
            log.warn("Failed to load config for channel plugin initialization: {}", e.getMessage());
            return new ChannelPluginLifecycle(null);
        }

        // Initialize Telegram
        TelegramBot.TelegramBotContext telegramContext = null;
        try {
            telegramContext = TelegramChannelPlugin.initialize(config);
            if (telegramContext != null) {
                // Wire config supplier for per-message config reload (picks up runtime
                // overrides)
                telegramContext.setConfigSupplier(configService::loadConfig);
                TelegramChannelPlugin.start(telegramContext);
            }
        } catch (Exception e) {
            log.warn("Failed to initialize Telegram channel plugin: {}", e.getMessage());
        }

        // Initialize WeChat
        try {
            WeChatChannelPlugin.initialize(config);
        } catch (Exception e) {
            log.warn("Failed to initialize WeChat channel plugin: {}", e.getMessage());
        }

        return new ChannelPluginLifecycle(telegramContext);
    }

    /**
     * Manages the lifecycle of channel plugins, ensuring clean shutdown.
     */
    public static class ChannelPluginLifecycle implements DisposableBean {
        private final TelegramBot.TelegramBotContext telegramContext;

        public ChannelPluginLifecycle(TelegramBot.TelegramBotContext telegramContext) {
            this.telegramContext = telegramContext;
        }

        @Override
        public void destroy() {
            if (telegramContext != null) {
                TelegramChannelPlugin.stop(telegramContext);
            }
        }
    }
}
