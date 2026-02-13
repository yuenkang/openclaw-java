package com.openclaw.channel.discord;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * In-memory cache of Discord user presence data (online/offline/idle/dnd).
 * Corresponds to TypeScript's discord/monitor/presence-cache.ts.
 */
public final class DiscordPresenceCache {

    private DiscordPresenceCache() {
    }

    private static final Map<String, Map<String, Object>> cache = new ConcurrentHashMap<>();

    private static String resolveAccountKey(String accountId) {
        return accountId != null && !accountId.isBlank() ? accountId : "default";
    }

    public static void setPresence(String accountId, String userId, Object data) {
        cache.computeIfAbsent(resolveAccountKey(accountId), k -> new ConcurrentHashMap<>())
                .put(userId, data);
    }

    public static Object getPresence(String accountId, String userId) {
        Map<String, Object> accountCache = cache.get(resolveAccountKey(accountId));
        return accountCache != null ? accountCache.get(userId) : null;
    }

    public static void clearPresences(String accountId) {
        if (accountId != null) {
            cache.remove(resolveAccountKey(accountId));
        } else {
            cache.clear();
        }
    }

    public static int size() {
        int total = 0;
        for (Map<String, Object> accountCache : cache.values()) {
            total += accountCache.size();
        }
        return total;
    }
}
