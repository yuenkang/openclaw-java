package com.openclaw.channel.telegram;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * In-memory cache of sent message IDs per chat for own-message detection.
 * Corresponds to TypeScript's telegram/sent-message-cache.ts.
 */
public final class TelegramSentMessageCache {

    private TelegramSentMessageCache() {
    }

    private static final long TTL_MS = 24L * 60 * 60 * 1000; // 24 hours

    private static final Map<String, ConcurrentHashMap<Integer, Long>> cache = new ConcurrentHashMap<>();

    /**
     * Record a message ID as sent by the bot.
     */
    public static void record(Object chatId, int messageId) {
        String key = String.valueOf(chatId);
        ConcurrentHashMap<Integer, Long> entry = cache.computeIfAbsent(key,
                k -> new ConcurrentHashMap<>());
        entry.put(messageId, System.currentTimeMillis());
        if (entry.size() > 100) {
            cleanupExpired(entry);
        }
    }

    /**
     * Check if a message was sent by the bot.
     */
    public static boolean wasSentByBot(Object chatId, int messageId) {
        String key = String.valueOf(chatId);
        ConcurrentHashMap<Integer, Long> entry = cache.get(key);
        if (entry == null)
            return false;
        cleanupExpired(entry);
        return entry.containsKey(messageId);
    }

    /**
     * Clear all cached entries.
     */
    public static void clear() {
        cache.clear();
    }

    private static void cleanupExpired(ConcurrentHashMap<Integer, Long> entry) {
        long now = System.currentTimeMillis();
        entry.entrySet().removeIf(e -> now - e.getValue() > TTL_MS);
    }
}
