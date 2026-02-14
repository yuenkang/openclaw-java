package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Telegram channel onboarding: setup wizard steps for bot token,
 * allow-from users, and DM policy.
 * Corresponds to TypeScript's channels/plugins/onboarding/telegram.ts.
 */
@Slf4j
public class TelegramOnboarding {

    private static final String CHANNEL = "telegram";

    // =========================================================================
    // Status
    // =========================================================================

    /**
     * Get the setup status for the Telegram channel.
     */
    public static Map<String, Object> getStatus(OpenClawConfig config) {
        Map<String, Object> status = new LinkedHashMap<>();
        status.put("channel", CHANNEL);

        var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, null);
        if (tgConfig == null) {
            status.put("configured", false);
            status.put("message", "No Telegram account configured");
            return status;
        }

        Object token = tgConfig.get("token");
        boolean hasToken = token instanceof String s && !s.isBlank();
        status.put("configured", hasToken);
        status.put("hasToken", hasToken);

        if (!hasToken) {
            status.put("message", "Bot token not set");
        } else {
            status.put("message", "Telegram bot configured");
        }
        return status;
    }

    // =========================================================================
    // DM Policy
    // =========================================================================

    /**
     * Set the Telegram DM policy in config.
     */
    @SuppressWarnings("unchecked")
    public static OpenClawConfig setDmPolicy(OpenClawConfig config, String dmPolicy) {
        var channelsConfig = config.getChannels();
        if (channelsConfig == null) {
            channelsConfig = new OpenClawConfig.ChannelsConfig();
            config.setChannels(channelsConfig);
        }
        Map<String, Object> providers = channelsConfig.getProviders();
        Map<String, Object> telegram;
        if (providers != null && providers.get("telegram") instanceof Map<?, ?> tg) {
            telegram = (Map<String, Object>) tg;
        } else {
            telegram = new LinkedHashMap<>();
            channelsConfig.addChannel("telegram", telegram);
        }
        telegram.put("dmPolicy", dmPolicy);
        return config;
    }

    // =========================================================================
    // Allow-from resolution
    // =========================================================================

    /**
     * Resolve allow-from entries for a Telegram account.
     */
    public static List<String> resolveAllowFrom(OpenClawConfig config, String accountId) {
        var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, accountId);
        if (tgConfig == null)
            return List.of();

        Object allowFrom = tgConfig.get("allowFrom");
        if (allowFrom instanceof List<?> list) {
            return list.stream()
                    .map(Object::toString)
                    .toList();
        }
        return List.of();
    }

    /**
     * Format allow-from entries for display.
     */
    public static List<String> formatAllowFrom(List<String> allowFrom) {
        if (allowFrom == null || allowFrom.isEmpty()) {
            return List.of("(none)");
        }
        return allowFrom.stream()
                .map(entry -> {
                    String trimmed = entry.trim();
                    if (trimmed.matches("-?\\d+"))
                        return "User ID: " + trimmed;
                    if (trimmed.startsWith("@"))
                        return "Username: " + trimmed;
                    return trimmed;
                })
                .toList();
    }

    // =========================================================================
    // Disable
    // =========================================================================

    /**
     * Disable the Telegram channel in config.
     */
    @SuppressWarnings("unchecked")
    public static OpenClawConfig disable(OpenClawConfig config) {
        if (config.getChannels() == null)
            return config;
        Map<String, Object> providers = config.getChannels().getProviders();
        if (providers != null && providers.get("telegram") instanceof Map<?, ?> tg) {
            Map<String, Object> telegram = (Map<String, Object>) tg;
            telegram.put("enabled", false);
            telegram.remove("token");
        }
        return config;
    }
}
