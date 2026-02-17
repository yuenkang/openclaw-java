package com.openclaw.common.infra;

import java.util.LinkedHashMap;

/**
 * Time-based deduplication cache with TTL and max-size eviction.
 * <p>
 * Corresponds to TypeScript's {@code infra/dedupe.ts}.
 * Thread-safe via synchronization.
 */
public class DedupeCache {

    private final long ttlMs;
    private final int maxSize;
    private final LinkedHashMap<String, Long> cache = new LinkedHashMap<>(64, 0.75f, true);

    public DedupeCache(long ttlMs, int maxSize) {
        this.ttlMs = Math.max(0, ttlMs);
        this.maxSize = Math.max(0, maxSize);
    }

    /**
     * Check if a key has been seen within the TTL window.
     * Returns {@code true} if the key is a duplicate (already seen and not
     * expired).
     * The key is always touched (inserted or refreshed) after the check.
     */
    public synchronized boolean isDuplicate(String key) {
        return isDuplicate(key, System.currentTimeMillis());
    }

    /**
     * Check with an explicit timestamp (useful for testing).
     */
    public synchronized boolean isDuplicate(String key, long nowMs) {
        if (key == null || key.isEmpty()) {
            return false;
        }

        Long existing = cache.get(key);
        boolean duplicate = false;

        if (existing != null && (ttlMs <= 0 || nowMs - existing < ttlMs)) {
            duplicate = true;
        }

        // Touch: remove + put to maintain insertion order
        cache.remove(key);
        cache.put(key, nowMs);

        if (!duplicate) {
            prune(nowMs);
        }

        return duplicate;
    }

    /**
     * Remove expired entries and enforce max size.
     */
    private void prune(long nowMs) {
        // Remove expired entries
        if (ttlMs > 0) {
            long cutoff = nowMs - ttlMs;
            cache.entrySet().removeIf(e -> e.getValue() < cutoff);
        }

        // Enforce max size (remove oldest entries first)
        if (maxSize <= 0) {
            cache.clear();
            return;
        }
        while (cache.size() > maxSize) {
            var it = cache.entrySet().iterator();
            if (it.hasNext()) {
                it.next();
                it.remove();
            } else {
                break;
            }
        }
    }

    public synchronized void clear() {
        cache.clear();
    }

    public synchronized int size() {
        return cache.size();
    }
}
