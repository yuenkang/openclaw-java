package com.openclaw.channel.telegram;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * In-memory sticker cache for Telegram with search support.
 * Corresponds to TypeScript's telegram/sticker-cache.ts.
 */
public final class TelegramStickerCache {

    private TelegramStickerCache() {
    }

    public record CachedSticker(String fileId, String fileUniqueId, String emoji,
            String setName, String description, String cachedAt,
            String receivedFrom) {
    }

    public record CacheStats(int count, String oldestAt, String newestAt) {
    }

    // In-memory store keyed by fileUniqueId
    private static final Map<String, CachedSticker> stickers = new ConcurrentHashMap<>();

    public static CachedSticker get(String fileUniqueId) {
        return stickers.get(fileUniqueId);
    }

    public static void put(CachedSticker sticker) {
        stickers.put(sticker.fileUniqueId(), sticker);
    }

    public static List<CachedSticker> getAll() {
        return new ArrayList<>(stickers.values());
    }

    /**
     * Search cached stickers by text query (fuzzy match on description, emoji,
     * setName).
     */
    public static List<CachedSticker> search(String query, int limit) {
        String queryLower = query.toLowerCase();
        record Scored(CachedSticker sticker, int score) {
        }
        List<Scored> results = new ArrayList<>();

        for (CachedSticker sticker : stickers.values()) {
            int score = 0;
            String descLower = sticker.description() != null
                    ? sticker.description().toLowerCase()
                    : "";

            if (descLower.contains(queryLower))
                score += 10;

            String[] queryWords = queryLower.split("\\s+");
            String[] descWords = descLower.split("\\s+");
            for (String qw : queryWords) {
                if (qw.isEmpty())
                    continue;
                for (String dw : descWords) {
                    if (dw.contains(qw)) {
                        score += 5;
                        break;
                    }
                }
            }

            if (sticker.emoji() != null && query.contains(sticker.emoji()))
                score += 8;
            if (sticker.setName() != null
                    && sticker.setName().toLowerCase().contains(queryLower))
                score += 3;

            if (score > 0)
                results.add(new Scored(sticker, score));
        }

        results.sort(Comparator.comparingInt(Scored::score).reversed());
        return results.stream().limit(limit).map(Scored::sticker).toList();
    }

    public static CacheStats stats() {
        if (stickers.isEmpty())
            return new CacheStats(0, null, null);
        String oldest = null;
        String newest = null;
        for (CachedSticker s : stickers.values()) {
            if (s.cachedAt() == null)
                continue;
            if (oldest == null || s.cachedAt().compareTo(oldest) < 0)
                oldest = s.cachedAt();
            if (newest == null || s.cachedAt().compareTo(newest) > 0)
                newest = s.cachedAt();
        }
        return new CacheStats(stickers.size(), oldest, newest);
    }

    public static void clear() {
        stickers.clear();
    }
}
