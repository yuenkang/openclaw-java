package com.openclaw.gateway.outbound;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Builder;
import lombok.Data;

import java.util.Map;

/**
 * Result returned by a successful outbound delivery.
 * Corresponds to TypeScript's OutboundDeliveryResult (deliver.ts).
 */
@Data
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class OutboundDeliveryResult {

    /** Channel through which the message was sent. */
    private String channel;

    /** Platform message ID. */
    private String messageId;

    /** Platform-specific chat/group ID. */
    private String chatId;

    /** Platform-specific channel ID (e.g. Discord channel). */
    private String channelId;

    /** Matrix room ID. */
    private String roomId;

    /** MS Teams conversation ID. */
    private String conversationId;

    /** Timestamp from the platform (epoch millis). */
    private Long timestamp;

    /** XMPP JID (WhatsApp). */
    private String toJid;

    /** Telegram poll ID. */
    private String pollId;

    /** Arbitrary metadata from channel plugin. */
    private Map<String, Object> meta;
}
