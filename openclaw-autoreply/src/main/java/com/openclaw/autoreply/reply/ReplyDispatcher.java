package com.openclaw.autoreply.reply;

import com.openclaw.autoreply.AutoReplyTypes;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;

/**
 * Reply dispatcher — serializes deliveries with human-like delay,
 * normalization,
 * and idle signaling.
 * Mirrors {@code auto-reply/reply/reply-dispatcher.ts}.
 */
public final class ReplyDispatcher {

    private ReplyDispatcher() {
    }

    /** Delivery kind. */
    public static final String KIND_TOOL = "tool";
    public static final String KIND_BLOCK = "block";
    public static final String KIND_FINAL = "final";

    /** Skip reason. */
    public static final String SKIP_EMPTY = "empty";
    public static final String SKIP_SILENT = "silent";
    public static final String SKIP_HEARTBEAT = "heartbeat";

    private static final int DEFAULT_HUMAN_DELAY_MIN_MS = 800;
    private static final int DEFAULT_HUMAN_DELAY_MAX_MS = 2500;

    /** Dispatcher interface. */
    public interface Dispatcher {
        boolean sendToolResult(AutoReplyTypes.ReplyPayload payload);

        boolean sendBlockReply(AutoReplyTypes.ReplyPayload payload);

        boolean sendFinalReply(AutoReplyTypes.ReplyPayload payload);

        CompletableFuture<Void> waitForIdle();

        Map<String, Integer> getQueuedCounts();
    }

    /** Options for creating a dispatcher. */
    public record DispatcherOptions(
            BiConsumer<AutoReplyTypes.ReplyPayload, String> deliver,
            String responsePrefix,
            Runnable onHeartbeatStrip,
            Runnable onIdle,
            BiConsumer<Exception, String> onError,
            SkipHandler onSkip,
            HumanDelayConfig humanDelay) {
    }

    /** Functional interface for skip callbacks. */
    @FunctionalInterface
    public interface SkipHandler {
        void onSkip(AutoReplyTypes.ReplyPayload payload, String kind, String reason);
    }

    /** Human delay configuration. */
    public record HumanDelayConfig(String mode, Integer minMs, Integer maxMs) {
    }

    /**
     * Generate a random human-like delay in ms.
     */
    public static int getHumanDelay(HumanDelayConfig config) {
        String mode = config != null && config.mode() != null ? config.mode() : "off";
        if ("off".equals(mode))
            return 0;
        int min = "custom".equals(mode) && config.minMs() != null
                ? config.minMs()
                : DEFAULT_HUMAN_DELAY_MIN_MS;
        int max = "custom".equals(mode) && config.maxMs() != null
                ? config.maxMs()
                : DEFAULT_HUMAN_DELAY_MAX_MS;
        if (max <= min)
            return min;
        return (int) (Math.random() * (max - min + 1)) + min;
    }

    /**
     * Create a reply dispatcher that serializes deliveries.
     */
    public static Dispatcher createReplyDispatcher(DispatcherOptions options) {
        final Object lock = new Object();
        @SuppressWarnings("unchecked")
        final CompletableFuture<Void>[] sendChain = new CompletableFuture[] { CompletableFuture.completedFuture(null) };
        final int[] pending = { 0 };
        final boolean[] sentFirstBlock = { false };
        final Map<String, Integer> queuedCounts = new java.util.concurrent.ConcurrentHashMap<>(Map.of(
                KIND_TOOL, 0, KIND_BLOCK, 0, KIND_FINAL, 0));

        return new Dispatcher() {

            private boolean enqueue(String kind, AutoReplyTypes.ReplyPayload payload) {
                // Simplified normalization (full normalizeReplyPayload deferred)
                String text = payload.text();
                if (text != null)
                    text = text.trim();
                boolean hasMedia = (payload.mediaUrl() != null && !payload.mediaUrl().isEmpty())
                        || (payload.mediaUrls() != null && !payload.mediaUrls().isEmpty());
                boolean hasChannelData = payload.channelData() != null && !payload.channelData().isEmpty();

                if ((text == null || text.isEmpty()) && !hasMedia && !hasChannelData) {
                    if (options.onSkip() != null) {
                        options.onSkip().onSkip(payload, kind, SKIP_EMPTY);
                    }
                    return false;
                }

                queuedCounts.merge(kind, 1, Integer::sum);
                synchronized (lock) {
                    pending[0]++;
                }
                boolean shouldDelay = KIND_BLOCK.equals(kind) && sentFirstBlock[0];
                if (KIND_BLOCK.equals(kind))
                    sentFirstBlock[0] = true;

                sendChain[0] = sendChain[0].thenRunAsync(() -> {
                    try {
                        if (shouldDelay) {
                            int delay = getHumanDelay(options.humanDelay());
                            if (delay > 0)
                                Thread.sleep(delay);
                        }
                        if (options.deliver() != null) {
                            options.deliver().accept(payload, kind);
                        }
                    } catch (Exception e) {
                        if (options.onError() != null) {
                            options.onError().accept(e, kind);
                        }
                    } finally {
                        synchronized (lock) {
                            pending[0]--;
                            if (pending[0] == 0 && options.onIdle() != null) {
                                options.onIdle().run();
                            }
                        }
                    }
                });
                return true;
            }

            @Override
            public boolean sendToolResult(AutoReplyTypes.ReplyPayload payload) {
                return enqueue(KIND_TOOL, payload);
            }

            @Override
            public boolean sendBlockReply(AutoReplyTypes.ReplyPayload payload) {
                return enqueue(KIND_BLOCK, payload);
            }

            @Override
            public boolean sendFinalReply(AutoReplyTypes.ReplyPayload payload) {
                return enqueue(KIND_FINAL, payload);
            }

            @Override
            public CompletableFuture<Void> waitForIdle() {
                return sendChain[0];
            }

            @Override
            public Map<String, Integer> getQueuedCounts() {
                return Map.copyOf(queuedCounts);
            }
        };
    }
}
