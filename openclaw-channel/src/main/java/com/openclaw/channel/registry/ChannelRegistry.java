package com.openclaw.channel.registry;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Registry for known chat channels with metadata.
 * Corresponds to TypeScript's channels/registry.ts.
 */
@Slf4j
public class ChannelRegistry {

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ChannelMeta {
        private String id;
        private String name;
        private List<String> aliases;
        private int order;
        private boolean supportsMedia;
        private boolean supportsThreads;
        private boolean supportsButtons;
        private boolean supportsMarkdown;
    }

    private final Map<String, ChannelMeta> channels = new ConcurrentHashMap<>();
    private final Map<String, String> aliasMap = new ConcurrentHashMap<>();

    public ChannelRegistry() {
        registerDefaults();
    }

    private void registerDefaults() {
        register(ChannelMeta.builder()
                .id("telegram").name("Telegram").aliases(List.of("tg", "tgram"))
                .order(1).supportsMedia(true).supportsThreads(true)
                .supportsButtons(true).supportsMarkdown(true).build());

        register(ChannelMeta.builder()
                .id("whatsapp").name("WhatsApp").aliases(List.of("wa"))
                .order(2).supportsMedia(true).supportsThreads(false)
                .supportsButtons(true).supportsMarkdown(false).build());

        register(ChannelMeta.builder()
                .id("discord").name("Discord").aliases(List.of("dc"))
                .order(3).supportsMedia(true).supportsThreads(true)
                .supportsButtons(true).supportsMarkdown(true).build());

        register(ChannelMeta.builder()
                .id("signal").name("Signal").aliases(List.of())
                .order(4).supportsMedia(true).supportsThreads(false)
                .supportsButtons(false).supportsMarkdown(false).build());

        register(ChannelMeta.builder()
                .id("slack").name("Slack").aliases(List.of())
                .order(5).supportsMedia(true).supportsThreads(true)
                .supportsButtons(true).supportsMarkdown(true).build());
    }

    public void register(ChannelMeta meta) {
        channels.put(meta.getId(), meta);
        if (meta.getAliases() != null) {
            meta.getAliases().forEach(alias -> aliasMap.put(alias, meta.getId()));
        }
        log.debug("Registered channel: {} ({})", meta.getId(), meta.getName());
    }

    /**
     * Normalize a channel identifier (resolve aliases).
     */
    public String normalize(String channelId) {
        if (channelId == null)
            return null;
        String lower = channelId.toLowerCase().trim();
        return aliasMap.getOrDefault(lower, lower);
    }

    public Optional<ChannelMeta> get(String channelId) {
        return Optional.ofNullable(channels.get(normalize(channelId)));
    }

    public boolean isKnown(String channelId) {
        return channels.containsKey(normalize(channelId));
    }

    public List<ChannelMeta> listAll() {
        return channels.values().stream()
                .sorted(Comparator.comparingInt(ChannelMeta::getOrder))
                .toList();
    }
}
