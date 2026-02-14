package com.openclaw.gateway.outbound;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Channel message adapter registry.
 * Maps channels to their embed and cross-context capabilities.
 * Corresponds to TypeScript's channel-adapters.ts.
 */
public final class ChannelMessageAdapters {

    private ChannelMessageAdapters() {
    }

    /**
     * Channel adapter capabilities.
     */
    public record ChannelMessageAdapter(
            boolean supportsEmbeds,
            boolean supportsCrossContextEmbeds) {
        /** Build a default adapter with no embed support. */
        public static ChannelMessageAdapter defaultAdapter() {
            return new ChannelMessageAdapter(false, false);
        }
    }

    private static final Map<String, ChannelMessageAdapter> ADAPTERS;

    static {
        Map<String, ChannelMessageAdapter> m = new HashMap<>();
        // Discord supports embeds
        m.put("discord", new ChannelMessageAdapter(true, true));
        // Other channels default to no embed support
        ADAPTERS = Collections.unmodifiableMap(m);
    }

    /**
     * Get the message adapter for a given channel.
     */
    public static ChannelMessageAdapter getAdapter(String channel) {
        return ADAPTERS.getOrDefault(
                channel != null ? channel.toLowerCase() : "",
                ChannelMessageAdapter.defaultAdapter());
    }

    /**
     * Build cross-context embed objects for Discord.
     */
    public static List<Map<String, Object>> buildCrossContextEmbeds(
            String channel, String originLabel) {
        ChannelMessageAdapter adapter = getAdapter(channel);
        if (!adapter.supportsCrossContextEmbeds()) {
            return Collections.emptyList();
        }
        return List.of(Map.of("description", "From " + originLabel));
    }
}
