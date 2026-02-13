package com.openclaw.agent.autoreply.reply;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Provider dispatcher — thin wrapper that routes inbound messages through
 * the reply dispatcher, optionally with buffered block streaming.
 * Mirrors {@code auto-reply/reply/provider-dispatcher.ts}.
 */
public final class ProviderDispatcher {

    private ProviderDispatcher() {
    }

    /** Result of a dispatch call. */
    public record DispatchResult(Map<String, Object> reply, boolean dispatched) {
    }

    /**
     * Dispatch a reply using the buffered block dispatcher.
     * Full integration deferred — returns a placeholder future.
     */
    public static CompletableFuture<DispatchResult> dispatchReplyWithBufferedBlockDispatcher(
            Map<String, Object> ctx,
            Map<String, Object> cfg,
            Map<String, Object> dispatcherOptions,
            Map<String, Object> replyOptions) {
        // Full implementation deferred — delegates to reply-dispatcher
        return CompletableFuture.completedFuture(new DispatchResult(null, false));
    }

    /**
     * Dispatch a reply using the standard dispatcher.
     * Full integration deferred — returns a placeholder future.
     */
    public static CompletableFuture<DispatchResult> dispatchReplyWithDispatcher(
            Map<String, Object> ctx,
            Map<String, Object> cfg,
            Map<String, Object> dispatcherOptions,
            Map<String, Object> replyOptions) {
        // Full implementation deferred — delegates to reply-dispatcher
        return CompletableFuture.completedFuture(new DispatchResult(null, false));
    }
}
