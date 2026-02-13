package com.openclaw.channel;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Channel plugin loader — cached lookup of registered channel plugins.
 * Corresponds to TypeScript's channels/plugins/load.ts.
 */
public final class ChannelPluginLoader {

    private ChannelPluginLoader() {
    }

    private static final Map<String, ChannelPluginDef> cache = new ConcurrentHashMap<>();

    /**
     * Get a channel plugin by ID. Returns cached instance if available.
     */
    public static Optional<ChannelPluginDef> get(String channelId) {
        return Optional.ofNullable(cache.get(channelId));
    }

    /**
     * Register a channel plugin.
     */
    public static void register(ChannelPluginDef plugin) {
        cache.put(plugin.getId(), plugin);
    }

    /**
     * Clear the plugin cache (e.g. on registry refresh).
     */
    public static void clearCache() {
        cache.clear();
    }
}
