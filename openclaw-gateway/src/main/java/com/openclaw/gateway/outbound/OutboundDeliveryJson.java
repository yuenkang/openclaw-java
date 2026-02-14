package com.openclaw.gateway.outbound;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Builder;
import lombok.Data;

import java.util.Map;

/**
 * JSON-serializable outbound delivery descriptor.
 * Corresponds to TypeScript's OutboundDeliveryJson (format.ts).
 */
@Data
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class OutboundDeliveryJson {

    /** Channel identifier. */
    private String channel;

    /** Delivery mode: "direct" or "gateway". */
    @Builder.Default
    private String via = "direct";

    /** Recipient. */
    private String to;

    /** Platform message ID. */
    private String messageId;

    /** Media attachment URL. */
    private String mediaUrl;

    /** Platform chat/group ID. */
    private String chatId;

    /** Platform channel ID (Discord/Slack). */
    private String channelId;

    /** Matrix room ID. */
    private String roomId;

    /** MS Teams conversation ID. */
    private String conversationId;

    /** Timestamp (epoch millis). */
    private Long timestamp;

    /** XMPP JID. */
    private String toJid;

    /** Arbitrary channel-specific metadata. */
    private Map<String, Object> meta;
}
