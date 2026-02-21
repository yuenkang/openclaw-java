package com.openclaw.autoreply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Dispatch inbound messages to the reply pipeline — finalize context,
 * create dispatcher, and invoke dispatchReplyFromConfig.
 * Mirrors {@code auto-reply/dispatch.ts}.
 */
public final class Dispatch {

    private static final Logger log = LoggerFactory.getLogger(Dispatch.class);

    private Dispatch() {
    }

    /** Result of dispatching an inbound message. */
    public record DispatchInboundResult(
            Object reply,
            boolean handled,
            String error) {
    }

    /**
     * Dispatch an inbound message through the reply pipeline.
     */
    public static CompletableFuture<DispatchInboundResult> dispatchInboundMessage(
            Map<String, Object> ctx,
            Map<String, Object> cfg,
            Object dispatcher,
            Map<String, Object> replyOptions) {
        // Full dispatchReplyFromConfig integration deferred
        log.debug("Dispatching inbound message through reply pipeline");
        return CompletableFuture.completedFuture(
                new DispatchInboundResult(null, false, null));
    }

    /**
     * Dispatch with a buffered dispatcher that handles typing signals.
     */
    public static CompletableFuture<DispatchInboundResult> dispatchInboundMessageWithBufferedDispatcher(
            Map<String, Object> ctx,
            Map<String, Object> cfg,
            Map<String, Object> dispatcherOptions,
            Map<String, Object> replyOptions) {
        log.debug("Dispatching with buffered dispatcher");
        return dispatchInboundMessage(ctx, cfg, null, replyOptions);
    }

    /**
     * Dispatch with a standard dispatcher.
     */
    public static CompletableFuture<DispatchInboundResult> dispatchInboundMessageWithDispatcher(
            Map<String, Object> ctx,
            Map<String, Object> cfg,
            Map<String, Object> dispatcherOptions,
            Map<String, Object> replyOptions) {
        log.debug("Dispatching with standard dispatcher");
        return dispatchInboundMessage(ctx, cfg, null, replyOptions);
    }
}
