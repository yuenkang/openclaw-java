package com.openclaw.gateway.outbound;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Main outbound delivery orchestrator.
 * Handles payload normalization, channel handler creation, chunking, and
 * delivery.
 * Corresponds to TypeScript's deliver.ts.
 */
@Slf4j
public class OutboundDelivery {

    /**
     * Local adapter interface to avoid circular dependency on openclaw-channel.
     * Implementations bridge to ChannelOutboundAdapter at runtime.
     */
    @FunctionalInterface
    public interface ChannelSendAdapter {
        /**
         * Send a message (text and/or media) to a target.
         *
         * @param target   Recipient identifier
         * @param text     Text content (may be null)
         * @param mediaUrl Media URL (may be null)
         * @param options  Additional options (replyToId, threadId, etc.)
         * @return Delivery result
         */
        CompletableFuture<OutboundDeliveryResult> send(
                String target, String text, String mediaUrl,
                Map<String, Object> options);
    }

    /** Registered adapters by channel ID. */
    private final Map<String, ChannelSendAdapter> adapterRegistry = new ConcurrentHashMap<>();

    /** Channel text limits. */
    private static final Map<String, Integer> TEXT_LIMITS = Map.of(
            "telegram", 4096,
            "discord", 2000,
            "slack", 40000,
            "whatsapp", 65536);

    /**
     * Register a channel send adapter.
     */
    public void registerAdapter(String channelId, ChannelSendAdapter adapter) {
        adapterRegistry.put(channelId.toLowerCase(), adapter);
        log.debug("Registered outbound adapter for channel: {}", channelId);
    }

    /**
     * Get registered adapter for a channel.
     */
    public Optional<ChannelSendAdapter> getAdapter(String channelId) {
        return Optional.ofNullable(
                adapterRegistry.get(channelId != null ? channelId.toLowerCase() : ""));
    }

    /**
     * Deliver outbound payloads to a channel target.
     */
    public CompletableFuture<List<OutboundDeliveryResult>> deliverPayloads(
            OpenClawConfig cfg,
            String channel,
            String to,
            List<OutboundPayloads.NormalizedOutboundPayload> payloads,
            String accountId,
            String replyToId,
            String threadId) {

        if (payloads == null || payloads.isEmpty()) {
            log.debug("No payloads to deliver to {}:{}", channel, to);
            return CompletableFuture.completedFuture(Collections.emptyList());
        }

        Optional<ChannelSendAdapter> adapterOpt = getAdapter(channel);
        if (adapterOpt.isEmpty()) {
            log.warn("No outbound adapter registered for channel: {}", channel);
            return CompletableFuture.failedFuture(
                    new IllegalStateException("No adapter for channel: " + channel));
        }

        ChannelSendAdapter adapter = adapterOpt.get();
        int textLimit = TEXT_LIMITS.getOrDefault(
                channel != null ? channel.toLowerCase() : "", 4096);

        return CompletableFuture.supplyAsync(() -> {
            List<OutboundDeliveryResult> results = new ArrayList<>();
            Map<String, Object> options = new HashMap<>();
            if (replyToId != null)
                options.put("replyToId", replyToId);
            if (threadId != null)
                options.put("threadId", threadId);
            if (accountId != null)
                options.put("accountId", accountId);

            for (OutboundPayloads.NormalizedOutboundPayload payload : payloads) {
                try {
                    deliverSinglePayload(adapter, to, payload, textLimit, options, results);
                } catch (Exception e) {
                    log.error("Failed to deliver payload to {}:{} - {}",
                            channel, to, e.getMessage(), e);
                    throw new RuntimeException("Delivery failed: " + e.getMessage(), e);
                }
            }
            log.info("Delivered {} payloads to {}:{}", results.size(), channel, to);
            return results;
        });
    }

    /**
     * Deliver a single payload (text and/or media).
     */
    private void deliverSinglePayload(
            ChannelSendAdapter adapter,
            String to,
            OutboundPayloads.NormalizedOutboundPayload payload,
            int textLimit,
            Map<String, Object> options,
            List<OutboundDeliveryResult> results) {

        String text = payload.getText();
        List<String> mediaUrls = payload.getMediaUrls();
        boolean hasText = text != null && !text.isEmpty();
        boolean hasMedia = mediaUrls != null && !mediaUrls.isEmpty();

        if (hasMedia) {
            for (int i = 0; i < mediaUrls.size(); i++) {
                String mediaUrl = mediaUrls.get(i);
                String caption = (i == mediaUrls.size() - 1 && hasText) ? text : "";
                OutboundDeliveryResult result = adapter.send(to, caption, mediaUrl, options).join();
                results.add(result);
            }
        } else if (hasText) {
            List<String> chunks = chunkText(text, textLimit);
            for (String chunk : chunks) {
                OutboundDeliveryResult result = adapter.send(to, chunk, null, options).join();
                results.add(result);
            }
        }
    }

    /**
     * Chunk a long text into pieces that fit within the limit.
     * Tries to split at paragraph/line boundaries.
     */
    static List<String> chunkText(String text, int limit) {
        if (text == null || text.length() <= limit) {
            return text == null ? Collections.emptyList() : List.of(text);
        }

        List<String> chunks = new ArrayList<>();
        int start = 0;
        while (start < text.length()) {
            int end = Math.min(start + limit, text.length());
            if (end < text.length()) {
                int breakPoint = text.lastIndexOf("\n\n", end);
                if (breakPoint <= start) {
                    breakPoint = text.lastIndexOf("\n", end);
                }
                if (breakPoint <= start) {
                    breakPoint = text.lastIndexOf(" ", end);
                }
                if (breakPoint > start) {
                    end = breakPoint + 1;
                }
            }
            chunks.add(text.substring(start, end).stripTrailing());
            start = end;
            while (start < text.length() && text.charAt(start) == '\n') {
                start++;
            }
        }
        return chunks;
    }

    /**
     * Get the set of registered channel IDs.
     */
    public Set<String> getRegisteredChannels() {
        return Collections.unmodifiableSet(adapterRegistry.keySet());
    }
}
