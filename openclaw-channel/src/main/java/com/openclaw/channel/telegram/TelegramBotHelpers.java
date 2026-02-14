package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Telegram bot helper functions: session keys, peer IDs, config resolution.
 * Corresponds to TypeScript's telegram/bot/helpers.ts.
 */
@Slf4j
public class TelegramBotHelpers {

    /**
     * Build a session key from chat ID and optional thread ID.
     */
    public static String buildSessionKey(String chatId, Integer messageThreadId) {
        if (messageThreadId != null && messageThreadId > 0) {
            return "tg:" + chatId + ":" + messageThreadId;
        }
        return "tg:" + chatId;
    }

    /**
     * Build a group peer ID.
     */
    public static String buildGroupPeerId(String chatId) {
        return "tg:group:" + chatId;
    }

    /**
     * Build a parent peer (for forum threads).
     */
    public static String buildParentPeer(String chatId) {
        return "tg:chat:" + chatId;
    }

    /**
     * Resolve the Telegram account configuration from OpenClawConfig.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> resolveTelegramAccountConfig(
            OpenClawConfig config, String accountId) {

        if (config == null)
            return null;
        var channels = config.getChannels();
        if (channels == null)
            return null;
        var providers = channels.getProviders();
        if (providers == null)
            return null;

        // Look in the providers for telegram entries
        Object telegram = providers.get("telegram");
        if (telegram instanceof Map<?, ?> tgMap) {
            if (accountId != null) {
                Object accounts = tgMap.get("accounts");
                if (accounts instanceof Map<?, ?> accountsMap) {
                    Object account = accountsMap.get(accountId);
                    if (account instanceof Map<?, ?>) {
                        return (Map<String, Object>) account;
                    }
                }
            }
            return (Map<String, Object>) tgMap;
        }

        return null;
    }

    /**
     * Resolve allow-from list from a Telegram account config.
     */
    @SuppressWarnings("unchecked")
    public static List<String> resolveAllowFrom(Map<String, Object> tgConfig) {
        if (tgConfig == null)
            return List.of();
        Object allowFrom = tgConfig.get("allowFrom");
        if (allowFrom instanceof List<?> list) {
            return (List<String>) list;
        }
        return List.of();
    }

    /**
     * Resolve group configuration for a given chat.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> resolveGroupConfig(
            Map<String, Object> tgConfig, String chatId) {

        if (tgConfig == null || chatId == null)
            return null;
        Object groups = tgConfig.get("groups");
        if (groups instanceof Map<?, ?> groupsMap) {
            Object groupCfg = groupsMap.get(chatId);
            if (groupCfg instanceof Map<?, ?>) {
                return (Map<String, Object>) groupCfg;
            }
        }
        return null;
    }

    /**
     * Resolve topic configuration for a forum thread.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> resolveTopicConfig(
            Map<String, Object> tgConfig, String chatId, int threadId) {

        Map<String, Object> groupCfg = resolveGroupConfig(tgConfig, chatId);
        if (groupCfg == null)
            return null;

        Object topics = groupCfg.get("topics");
        if (topics instanceof Map<?, ?> topicsMap) {
            Object topicCfg = topicsMap.get(String.valueOf(threadId));
            if (topicCfg instanceof Map<?, ?>) {
                return (Map<String, Object>) topicCfg;
            }
        }
        return null;
    }

    /**
     * Resolve the Telegram stream mode.
     */
    public static String resolveTelegramStreamMode(OpenClawConfig config, String accountId) {
        var tgConfig = resolveTelegramAccountConfig(config, accountId);
        if (tgConfig == null)
            return "draft";
        Object streamMode = tgConfig.get("streamMode");
        if (streamMode instanceof String s)
            return s;
        return "draft";
    }

    /**
     * Resolve the forum thread ID from a message context.
     */
    public static Integer resolveForumThreadId(Map<String, Object> message) {
        if (message == null)
            return null;
        Object threadId = message.get("message_thread_id");
        if (threadId instanceof Number n)
            return n.intValue();
        return null;
    }

    /**
     * Check if a message has a bot mention in its entities.
     */
    public static boolean hasBotMention(Map<String, Object> message, String botUsername) {
        if (message == null || botUsername == null)
            return false;
        String text = (String) message.get("text");
        if (text == null)
            return false;
        return text.toLowerCase().contains("@" + botUsername.toLowerCase());
    }
}
