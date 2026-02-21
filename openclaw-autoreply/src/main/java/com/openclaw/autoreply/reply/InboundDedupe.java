package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Inbound message deduplication using a TTL-based cache.
 * Mirrors {@code auto-reply/reply/inbound-dedupe.ts}.
 */
public final class InboundDedupe {

    private static final Logger log = LoggerFactory.getLogger(InboundDedupe.class);

    private InboundDedupe() {
    }

    private static final long DEFAULT_TTL_MS = 20 * 60_000L;
    private static final int DEFAULT_MAX_SIZE = 5000;

    /** Simple TTL-based dedupe cache. */
    private static final Map<String, Long> CACHE = new ConcurrentHashMap<>();

    /* ── helpers ────────────────────────────────────────────── */

    static String normalizeProvider(String value) {
        if (value == null)
            return "";
        return value.trim().toLowerCase();
    }

    /**
     * Build a deduplication key for an inbound message context.
     *
     * @return key or null if not deducible
     */
    public static String buildInboundDedupeKey(
            String provider, String messageId, String peerId,
            String sessionKey, String accountId, String threadId) {

        String prov = normalizeProvider(provider);
        String msgId = messageId != null ? messageId.trim() : "";
        if (prov.isEmpty() || msgId.isEmpty())
            return null;

        if (peerId == null || peerId.isBlank())
            return null;

        String sk = sessionKey != null ? sessionKey.trim() : "";
        String ai = accountId != null ? accountId.trim() : "";
        String ti = threadId != null ? threadId.trim() : "";

        StringBuilder sb = new StringBuilder();
        for (String part : new String[] { prov, ai, sk, peerId.trim(), ti, msgId }) {
            if (!part.isEmpty()) {
                if (sb.length() > 0)
                    sb.append('|');
                sb.append(part);
            }
        }
        return sb.toString();
    }

    /**
     * Check whether this inbound message is a duplicate.
     *
     * @return true if this key has been seen within the TTL window
     */
    public static boolean shouldSkipDuplicateInbound(String dedupeKey) {
        if (dedupeKey == null || dedupeKey.isEmpty())
            return false;

        long now = System.currentTimeMillis();

        // Evict expired entries lazily (keep cache bounded)
        if (CACHE.size() > DEFAULT_MAX_SIZE) {
            CACHE.entrySet().removeIf(e -> now - e.getValue() > DEFAULT_TTL_MS);
        }

        Long prev = CACHE.putIfAbsent(dedupeKey, now);
        if (prev != null && now - prev <= DEFAULT_TTL_MS) {
            log.debug("inbound dedupe: skipped {}", dedupeKey);
            return true;
        }
        if (prev != null) {
            // Expired — update
            CACHE.put(dedupeKey, now);
        }
        return false;
    }

    /** Reset the inbound dedupe cache (for testing). */
    public static void resetInboundDedupe() {
        CACHE.clear();
    }
}
