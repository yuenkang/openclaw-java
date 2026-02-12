package com.openclaw.agent.runner;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Cache TTL tracking for Anthropic prompt caching optimization.
 * Mirrors {@code agents/pi-embedded-runner/cache-ttl.ts}.
 */
public final class CacheTtl {

    private CacheTtl() {
    }

    public static final String CACHE_TTL_CUSTOM_TYPE = "openclaw.cache-ttl";

    /** Data stored in a cache-TTL session entry. */
    public record CacheTtlEntryData(long timestamp, String provider, String modelId) {
    }

    /**
     * Check if a provider/model combination is eligible for cache-TTL tracking.
     * Only Anthropic models (directly or via OpenRouter) qualify.
     */
    public static boolean isCacheTtlEligibleProvider(String provider, String modelId) {
        if (provider == null)
            return false;
        String norm = provider.toLowerCase();
        if ("anthropic".equals(norm))
            return true;
        return "openrouter".equals(norm)
                && modelId != null
                && modelId.toLowerCase().startsWith("anthropic/");
    }

    /**
     * Read the last cache-TTL timestamp from session custom entries.
     *
     * @param entries list of custom session entry maps
     * @return the timestamp, or null if not found
     */
    @SuppressWarnings("unchecked")
    public static Long readLastCacheTtlTimestamp(List<Map<String, Object>> entries) {
        if (entries == null || entries.isEmpty())
            return null;
        for (int i = entries.size() - 1; i >= 0; i--) {
            Map<String, Object> entry = entries.get(i);
            if (entry == null)
                continue;
            if (!"custom".equals(entry.get("type")))
                continue;
            if (!CACHE_TTL_CUSTOM_TYPE.equals(entry.get("customType")))
                continue;
            Object data = entry.get("data");
            if (data instanceof Map<?, ?> dataMap) {
                Object ts = dataMap.get("timestamp");
                if (ts instanceof Number n) {
                    long value = n.longValue();
                    if (value > 0)
                        return value;
                }
            }
        }
        return null;
    }
}
