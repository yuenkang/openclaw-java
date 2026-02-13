package com.openclaw.channel;

import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * Typing indicator callback types and factory.
 * Corresponds to TypeScript's channels/typing.ts.
 */
public final class ChannelTyping {

    private ChannelTyping() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    /** Typing action callbacks for a chat reply. */
    public interface TypingCallbacks {
        /** Called when the reply starts (send typing indicator). */
        CompletableFuture<Void> onReplyStart();

        /** Called when the reply chain becomes idle (stop typing indicator). */
        default void onIdle() {
        }
    }

    // =========================================================================
    // Factory
    // =========================================================================

    /**
     * Create typing callbacks with error handling.
     *
     * @param start        function to start typing indicator
     * @param stop         optional function to stop typing indicator
     * @param onStartError error handler for start failures
     * @param onStopError  optional error handler for stop failures (defaults to
     *                     onStartError)
     */
    public static TypingCallbacks createTypingCallbacks(
            Supplier<CompletableFuture<Void>> start,
            Supplier<CompletableFuture<Void>> stop,
            Consumer<Throwable> onStartError,
            Consumer<Throwable> onStopError) {

        Consumer<Throwable> effectiveStopError = onStopError != null ? onStopError : onStartError;

        return new TypingCallbacks() {
            @Override
            public CompletableFuture<Void> onReplyStart() {
                return start.get().exceptionally(err -> {
                    onStartError.accept(err);
                    return null;
                });
            }

            @Override
            public void onIdle() {
                if (stop != null) {
                    stop.get().exceptionally(err -> {
                        effectiveStopError.accept(err);
                        return null;
                    });
                }
            }
        };
    }
}
