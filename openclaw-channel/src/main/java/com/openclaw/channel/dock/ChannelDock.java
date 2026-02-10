package com.openclaw.channel.dock;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Runtime configuration for channel docks (capabilities & behavior).
 * Corresponds to TypeScript's channels/dock.ts.
 */
public class ChannelDock {

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DockConfig {
        private String channelId;

        // Outbound settings
        @Builder.Default
        private int maxTextLength = 4096;
        @Builder.Default
        private String textFormat = "plain"; // plain | html | markdown
        @Builder.Default
        private boolean splitLongMessages = true;

        // Streaming behavior
        @Builder.Default
        private boolean supportsStreaming = false;
        @Builder.Default
        private int streamingMinChars = 50;
        @Builder.Default
        private int streamingMaxChars = 500;

        // Media
        @Builder.Default
        private boolean supportsMedia = true;
        @Builder.Default
        private long maxMediaSize = 50 * 1024 * 1024; // 50MB

        // Threading
        @Builder.Default
        private boolean supportsThreads = false;
        @Builder.Default
        private boolean supportsReplyTo = true;
    }

    private final Map<String, DockConfig> docks = new ConcurrentHashMap<>();

    public ChannelDock() {
        registerDefaults();
    }

    private void registerDefaults() {
        register(DockConfig.builder()
                .channelId("telegram")
                .maxTextLength(4096)
                .textFormat("html")
                .supportsStreaming(true)
                .supportsThreads(true)
                .supportsMedia(true)
                .build());

        register(DockConfig.builder()
                .channelId("whatsapp")
                .maxTextLength(4096)
                .textFormat("plain")
                .supportsStreaming(false)
                .supportsThreads(false)
                .supportsMedia(true)
                .maxMediaSize(16 * 1024 * 1024) // 16MB
                .build());

        register(DockConfig.builder()
                .channelId("discord")
                .maxTextLength(2000)
                .textFormat("markdown")
                .supportsStreaming(true)
                .supportsThreads(true)
                .supportsMedia(true)
                .maxMediaSize(25 * 1024 * 1024) // 25MB
                .build());

        register(DockConfig.builder()
                .channelId("signal")
                .maxTextLength(6000)
                .textFormat("plain")
                .supportsStreaming(false)
                .supportsThreads(false)
                .build());

        register(DockConfig.builder()
                .channelId("slack")
                .maxTextLength(4000)
                .textFormat("markdown")
                .supportsStreaming(true)
                .supportsThreads(true)
                .build());
    }

    public void register(DockConfig config) {
        docks.put(config.getChannelId(), config);
    }

    public DockConfig get(String channelId) {
        return docks.getOrDefault(channelId, DockConfig.builder().channelId(channelId).build());
    }
}
