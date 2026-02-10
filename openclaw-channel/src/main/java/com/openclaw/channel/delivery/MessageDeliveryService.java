package com.openclaw.channel.delivery;

import com.openclaw.channel.adapter.ChannelOutboundAdapter;
import com.openclaw.channel.dock.ChannelDock;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Unified message delivery service.
 * Handles text splitting, media sending, and adapter dispatch.
 * Corresponds to TypeScript's infra/outbound/deliver.ts.
 */
@Slf4j
public class MessageDeliveryService {

    private final Map<String, ChannelOutboundAdapter> adapters = new ConcurrentHashMap<>();
    private final ChannelDock channelDock;

    public MessageDeliveryService(ChannelDock channelDock) {
        this.channelDock = channelDock;
    }

    public void registerAdapter(ChannelOutboundAdapter adapter) {
        adapters.put(adapter.getChannelId(), adapter);
        log.info("Registered outbound adapter: {}", adapter.getChannelId());
    }

    /**
     * Deliver a text message, splitting if necessary.
     */
    public CompletableFuture<Void> deliverText(String channelId, ChannelOutboundAdapter.OutboundTextPayload payload) {
        ChannelOutboundAdapter adapter = adapters.get(channelId);
        if (adapter == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("No adapter for channel: " + channelId));
        }

        ChannelDock.DockConfig dock = channelDock.get(channelId);

        // Split long messages if needed
        if (dock.isSplitLongMessages() && payload.getText().length() > dock.getMaxTextLength()) {
            List<String> chunks = splitText(payload.getText(), dock.getMaxTextLength());
            log.debug("Splitting message into {} chunks for {}", chunks.size(), channelId);

            CompletableFuture<Void> chain = CompletableFuture.completedFuture(null);
            for (String chunk : chunks) {
                ChannelOutboundAdapter.OutboundTextPayload chunkPayload = ChannelOutboundAdapter.OutboundTextPayload
                        .builder()
                        .target(payload.getTarget())
                        .text(chunk)
                        .replyTo(payload.getReplyTo())
                        .threadId(payload.getThreadId())
                        .build();
                chain = chain.thenCompose(v -> adapter.sendText(chunkPayload));
            }
            return chain;
        }

        return adapter.sendText(payload);
    }

    /**
     * Deliver a media message.
     */
    public CompletableFuture<Void> deliverMedia(String channelId, ChannelOutboundAdapter.OutboundMediaPayload payload) {
        ChannelOutboundAdapter adapter = adapters.get(channelId);
        if (adapter == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("No adapter for channel: " + channelId));
        }
        return adapter.sendMedia(payload);
    }

    /**
     * Split text into chunks at natural boundaries (newlines, then spaces).
     */
    List<String> splitText(String text, int maxLen) {
        List<String> chunks = new ArrayList<>();
        int start = 0;

        while (start < text.length()) {
            if (start + maxLen >= text.length()) {
                chunks.add(text.substring(start));
                break;
            }

            // Find best break point
            int end = start + maxLen;
            int breakAt = text.lastIndexOf('\n', end);
            if (breakAt <= start) {
                breakAt = text.lastIndexOf(' ', end);
            }
            if (breakAt <= start) {
                breakAt = end; // force break
            }

            chunks.add(text.substring(start, breakAt));
            start = breakAt;
            // Skip the break character
            if (start < text.length() && (text.charAt(start) == '\n' || text.charAt(start) == ' ')) {
                start++;
            }
        }
        return chunks;
    }

    public boolean hasAdapter(String channelId) {
        return adapters.containsKey(channelId);
    }
}
