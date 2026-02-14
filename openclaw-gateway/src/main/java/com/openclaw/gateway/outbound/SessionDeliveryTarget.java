package com.openclaw.gateway.outbound;

import lombok.Builder;
import lombok.Data;

/**
 * Delivery target resolved from session state.
 * Corresponds to TypeScript's SessionDeliveryTarget (targets.ts).
 */
@Data
@Builder
public class SessionDeliveryTarget {

    /** Resolved channel (e.g. "telegram", "discord"). */
    private String channel;

    /** Resolved recipient identifier. */
    private String to;

    /** Account ID for multi-account channels. */
    private String accountId;

    /** Thread/topic ID for threaded conversations. */
    private String threadId;

    /** Outbound target mode: "explicit", "implicit", "broadcast" */
    @Builder.Default
    private String mode = "implicit";

    /** Last-used channel from session. */
    private String lastChannel;

    /** Last-used recipient from session. */
    private String lastTo;

    /** Last-used account from session. */
    private String lastAccountId;

    /** Last-used thread ID. */
    private String lastThreadId;
}
