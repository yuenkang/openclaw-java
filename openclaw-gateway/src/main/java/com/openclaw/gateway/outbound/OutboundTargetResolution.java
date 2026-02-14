package com.openclaw.gateway.outbound;

import lombok.Data;

/**
 * Result of resolving an outbound target.
 * Corresponds to TypeScript's OutboundTargetResolution (targets.ts).
 */
@Data
public class OutboundTargetResolution {

    private final boolean ok;
    private final String to;
    private final Exception error;

    private OutboundTargetResolution(boolean ok, String to, Exception error) {
        this.ok = ok;
        this.to = to;
        this.error = error;
    }

    /** Successful resolution with resolved "to" address. */
    public static OutboundTargetResolution success(String to) {
        return new OutboundTargetResolution(true, to, null);
    }

    /** Failed resolution with error. */
    public static OutboundTargetResolution failure(Exception error) {
        return new OutboundTargetResolution(false, null, error);
    }

    /** Failed resolution with error message. */
    public static OutboundTargetResolution failure(String errorMessage) {
        return new OutboundTargetResolution(false, null, new RuntimeException(errorMessage));
    }
}
