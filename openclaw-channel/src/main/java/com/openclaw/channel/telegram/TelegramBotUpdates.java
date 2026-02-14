package com.openclaw.channel.telegram;

import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Telegram update deduplication and media group batching.
 * Corresponds to TypeScript's telegram/bot-updates.ts.
 */
@Slf4j
public class TelegramBotUpdates {

    public static final long MEDIA_GROUP_TIMEOUT_MS = 500;
    private static final long RECENT_UPDATE_TTL_MS = 5 * 60_000;
    private static final int RECENT_UPDATE_MAX = 2000;

    // =========================================================================
    // Media group batching
    // =========================================================================

    /**
     * Media group entry: batches messages that share a media_group_id.
     */
    public static class MediaGroupEntry {
        private final List<Map<String, Object>> messages = new ArrayList<>();
        private long lastUpdatedMs;
        private final String mediaGroupId;

        public MediaGroupEntry(String mediaGroupId) {
            this.mediaGroupId = mediaGroupId;
            this.lastUpdatedMs = System.currentTimeMillis();
        }

        public void addMessage(Map<String, Object> msg) {
            messages.add(msg);
            lastUpdatedMs = System.currentTimeMillis();
        }

        public List<Map<String, Object>> getMessages() {
            return Collections.unmodifiableList(messages);
        }

        public String getMediaGroupId() {
            return mediaGroupId;
        }

        public long getLastUpdatedMs() {
            return lastUpdatedMs;
        }
    }

    // =========================================================================
    // Update deduplication
    // =========================================================================

    /**
     * Simple LRU-based update deduplication cache.
     */
    public static class UpdateDedupe {
        private final Map<String, Long> seen = new ConcurrentHashMap<>();
        private final long ttlMs;
        private final int maxSize;

        public UpdateDedupe(long ttlMs, int maxSize) {
            this.ttlMs = ttlMs;
            this.maxSize = maxSize;
        }

        /**
         * Check if an update key has been seen recently. Returns true if duplicate.
         */
        public boolean isDuplicate(String key) {
            if (key == null)
                return false;
            long now = System.currentTimeMillis();
            Long prev = seen.get(key);
            if (prev != null && (now - prev) < ttlMs) {
                return true;
            }
            // Evict old entries if over max
            if (seen.size() >= maxSize) {
                evictOldest();
            }
            seen.put(key, now);
            return false;
        }

        private void evictOldest() {
            long now = System.currentTimeMillis();
            seen.entrySet().removeIf(e -> (now - e.getValue()) >= ttlMs);
            // If still over, remove oldest
            if (seen.size() >= maxSize) {
                seen.entrySet().stream()
                        .min(Comparator.comparingLong(Map.Entry::getValue))
                        .ifPresent(e -> seen.remove(e.getKey()));
            }
        }

        public void clear() {
            seen.clear();
        }
    }

    public static UpdateDedupe createUpdateDedupe() {
        return new UpdateDedupe(RECENT_UPDATE_TTL_MS, RECENT_UPDATE_MAX);
    }

    // =========================================================================
    // Update key resolution
    // =========================================================================

    /**
     * Resolve the unique update ID from a Telegram update.
     */
    public static Integer resolveUpdateId(Map<String, Object> update) {
        Object updateId = update.get("update_id");
        if (updateId instanceof Number n)
            return n.intValue();
        return null;
    }

    /**
     * Build a unique deduplication key from a Telegram update or context.
     */
    public static String buildUpdateKey(Map<String, Object> update) {
        Integer updateId = resolveUpdateId(update);
        if (updateId != null) {
            return "update:" + updateId;
        }

        // Try callback query ID
        if (update.get("callback_query") instanceof Map<?, ?> cbq) {
            Object id = cbq.get("id");
            if (id != null)
                return "callback:" + id;
        }

        // Try message chat + message_id
        Map<?, ?> msg = resolveMessage(update);
        if (msg != null) {
            Object chat = msg.get("chat");
            if (chat instanceof Map<?, ?> chatMap) {
                Object chatId = chatMap.get("id");
                Object messageId = msg.get("message_id");
                if (chatId != null && messageId instanceof Number) {
                    return "message:" + chatId + ":" + messageId;
                }
            }
        }

        return null;
    }

    private static Map<?, ?> resolveMessage(Map<String, Object> update) {
        Object msg = update.get("message");
        if (msg instanceof Map<?, ?> m)
            return m;

        msg = update.get("edited_message");
        if (msg instanceof Map<?, ?> m)
            return m;

        if (update.get("callback_query") instanceof Map<?, ?> cbq) {
            msg = cbq.get("message");
            if (msg instanceof Map<?, ?> m)
                return m;
        }

        return null;
    }
}
