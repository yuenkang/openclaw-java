package com.openclaw.gateway.outbound;

import lombok.Builder;
import lombok.Data;

/**
 * Represents an outbound message delivery target.
 * Corresponds to TypeScript's OutboundTarget (targets.ts).
 */
@Data
@Builder
public class OutboundTarget {

    /**
     * Channel identifier: "telegram", "whatsapp", "discord", "slack", "signal",
     * "none" etc.
     */
    private String channel;

    /** Recipient address/identifier for the channel. */
    private String to;

    /** Reason for the target selection (diagnostic). */
    private String reason;

    /** Account ID for multi-account channels. */
    private String accountId;

    /** Last-used channel from session history. */
    private String lastChannel;

    /** Last-used account ID from session history. */
    private String lastAccountId;
}
