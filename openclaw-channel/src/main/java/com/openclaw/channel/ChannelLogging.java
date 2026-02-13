package com.openclaw.channel;

import java.util.function.Consumer;

/**
 * Channel event logging utilities.
 * Corresponds to TypeScript's channels/logging.ts.
 */
public final class ChannelLogging {

    private ChannelLogging() {
    }

    /**
     * Log an inbound message drop.
     */
    public static void logInboundDrop(Consumer<String> log, String channel,
            String reason, String target) {
        String targetPart = target != null ? " target=" + target : "";
        log.accept(channel + ": drop " + reason + targetPart);
    }

    /**
     * Log a typing indicator failure.
     *
     * @param action "start" or "stop"
     */
    public static void logTypingFailure(Consumer<String> log, String channel,
            String target, String action, Object error) {
        String targetPart = target != null ? " target=" + target : "";
        String actionPart = action != null ? " action=" + action : "";
        log.accept(channel + " typing" + actionPart + " failed" + targetPart + ": " + error);
    }

    /**
     * Log an ack/reaction cleanup failure.
     */
    public static void logAckFailure(Consumer<String> log, String channel,
            String target, Object error) {
        String targetPart = target != null ? " target=" + target : "";
        log.accept(channel + " ack cleanup failed" + targetPart + ": " + error);
    }
}
