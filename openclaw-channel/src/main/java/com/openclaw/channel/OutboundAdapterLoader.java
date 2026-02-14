package com.openclaw.channel;

import com.openclaw.channel.adapter.ChannelOutboundAdapter;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Lightweight outbound adapter loader — cached lookup by channel ID.
 * Keeps outbound sends cheap to import (no monitors, onboarding, etc.).
 * Corresponds to TypeScript's channels/plugins/outbound/load.ts.
 */
@Slf4j
public final class OutboundAdapterLoader {

    private OutboundAdapterLoader() {
    }

    private static final Map<String, ChannelOutboundAdapter> cache = new ConcurrentHashMap<>();

    /**
     * Get an outbound adapter by channel ID. Returns cached instance if available.
     */
    public static Optional<ChannelOutboundAdapter> get(String channelId) {
        return Optional.ofNullable(cache.get(channelId));
    }

    /**
     * Register an outbound adapter for a channel.
     */
    public static void register(String channelId, ChannelOutboundAdapter adapter) {
        cache.put(channelId, adapter);
        log.debug("Registered outbound adapter for channel: {}", channelId);
    }

    /**
     * Clear the adapter cache (e.g. on registry refresh).
     */
    public static void clearCache() {
        cache.clear();
    }
}
