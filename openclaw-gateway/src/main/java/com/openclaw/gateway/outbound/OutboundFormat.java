package com.openclaw.gateway.outbound;

/**
 * Outbound delivery JSON representation and formatting utilities.
 * Corresponds to TypeScript's format.ts.
 */
public final class OutboundFormat {

    private OutboundFormat() {
    }

    /**
     * Format a human-readable summary of a delivery result.
     */
    public static String formatDeliverySummary(String channel, OutboundDeliveryResult result) {
        if (result == null) {
            return "✅ Sent via " + channel + ". Message ID: unknown";
        }

        String label = result.getChannel() != null ? result.getChannel() : channel;
        String base = "✅ Sent via " + label + ". Message ID: " + result.getMessageId();

        if (result.getChatId() != null) {
            return base + " (chat " + result.getChatId() + ")";
        }
        if (result.getChannelId() != null) {
            return base + " (channel " + result.getChannelId() + ")";
        }
        if (result.getRoomId() != null) {
            return base + " (room " + result.getRoomId() + ")";
        }
        if (result.getConversationId() != null) {
            return base + " (conversation " + result.getConversationId() + ")";
        }
        return base;
    }

    /**
     * Build a JSON-serializable delivery descriptor.
     */
    public static OutboundDeliveryJson buildDeliveryJson(String channel,
            String to,
            OutboundDeliveryResult result,
            String via,
            String mediaUrl) {
        String messageId = result != null && result.getMessageId() != null
                ? result.getMessageId()
                : "unknown";

        OutboundDeliveryJson.OutboundDeliveryJsonBuilder builder = OutboundDeliveryJson.builder()
                .channel(channel)
                .via(via != null ? via : "direct")
                .to(to)
                .messageId(messageId)
                .mediaUrl(mediaUrl);

        if (result != null) {
            if (result.getChatId() != null)
                builder.chatId(result.getChatId());
            if (result.getChannelId() != null)
                builder.channelId(result.getChannelId());
            if (result.getRoomId() != null)
                builder.roomId(result.getRoomId());
            if (result.getConversationId() != null)
                builder.conversationId(result.getConversationId());
            if (result.getTimestamp() != null)
                builder.timestamp(result.getTimestamp());
            if (result.getToJid() != null)
                builder.toJid(result.getToJid());
            if (result.getMeta() != null)
                builder.meta(result.getMeta());
        }

        return builder.build();
    }

    /**
     * Format a gateway action summary.
     */
    public static String formatGatewaySummary(String action, String channel, String messageId) {
        String a = action != null ? action : "Sent";
        String channelSuffix = channel != null ? " (" + channel + ")" : "";
        String mid = messageId != null ? messageId : "unknown";
        return "✅ " + a + " via gateway" + channelSuffix + ". Message ID: " + mid;
    }
}
