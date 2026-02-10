package com.openclaw.channel.adapter;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * Outbound channel adapter interface.
 * Corresponds to TypeScript's ChannelOutboundAdapter (types.adapters.ts).
 */
public interface ChannelOutboundAdapter {

    /** Channel identifier (e.g. "telegram", "whatsapp"). */
    String getChannelId();

    /** Delivery mode: "direct", "gateway", or "hybrid". */
    DeliveryMode getDeliveryMode();

    /** Send a text message. */
    CompletableFuture<Void> sendText(OutboundTextPayload payload);

    /** Send a media message (image, file, audio, etc). */
    CompletableFuture<Void> sendMedia(OutboundMediaPayload payload);

    // --- Supporting types ---

    enum DeliveryMode {
        DIRECT, GATEWAY, HYBRID
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class OutboundTextPayload {
        private String target;
        private String text;
        private String replyTo;
        private String threadId;
        private List<Button> buttons;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class OutboundMediaPayload {
        private String target;
        private String mediaUrl;
        private String mediaType;
        private String caption;
        private String filename;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class Button {
        private String label;
        private String data;
    }
}
