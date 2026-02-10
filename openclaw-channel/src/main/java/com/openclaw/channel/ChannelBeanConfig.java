package com.openclaw.channel;

import com.openclaw.channel.delivery.MessageDeliveryService;
import com.openclaw.channel.dock.ChannelDock;
import com.openclaw.channel.registry.ChannelRegistry;
import com.openclaw.channel.routing.TargetResolver;
import com.openclaw.channel.telegram.TelegramOutboundAdapter;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;
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
        try {
            OpenClawConfig config = configService.loadConfig();
            if (config.getChannels() != null && config.getChannels().getConfig() != null) {
                OpenClawConfig.ChannelConfig tgConfig = config.getChannels().getConfig().get("telegram");
                if (tgConfig != null && tgConfig.getBotToken() != null) {
                    service.registerAdapter(new TelegramOutboundAdapter(tgConfig.getBotToken()));
                }
            }
        } catch (Exception e) {
            log.debug("No Telegram config found: {}", e.getMessage());
        }

        // Also check env variable
        String tgToken = System.getenv("TELEGRAM_BOT_TOKEN");
        if (tgToken != null && !tgToken.isBlank() && !service.hasAdapter("telegram")) {
            service.registerAdapter(new TelegramOutboundAdapter(tgToken));
        }

        return service;
    }
}
