package com.openclaw.channel;

import lombok.Data;

import java.util.function.Consumer;

/**
 * Channel session recording for inbound messages.
 * Corresponds to TypeScript's channels/session.ts.
 */
public final class ChannelSession {

    private ChannelSession() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    @Data
    public static class InboundLastRouteUpdate {
        private String sessionKey;
        private String channel;
        private String to;
        private String accountId;
        private String threadId;
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Record an inbound session entry and optionally update the last route.
     * Session recording is fire-and-forget; errors go to the error handler.
     *
     * @param sessionKey      the session key
     * @param lastRouteUpdate optional last route update info
     * @param recordAction    action to perform session recording
     * @param routeAction     action to perform route update (if lastRouteUpdate is
     *                        non-null)
     * @param onRecordError   error handler for recording failures
     */
    public static void recordInboundSession(String sessionKey,
            InboundLastRouteUpdate lastRouteUpdate,
            Runnable recordAction,
            Consumer<InboundLastRouteUpdate> routeAction,
            Consumer<Throwable> onRecordError) {
        // Fire-and-forget session recording
        try {
            recordAction.run();
        } catch (Exception err) {
            onRecordError.accept(err);
        }

        // Update last route if requested
        if (lastRouteUpdate != null && routeAction != null) {
            routeAction.accept(lastRouteUpdate);
        }
    }
}
