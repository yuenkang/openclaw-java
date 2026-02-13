package com.openclaw.agent.autoreply.reply;

import com.openclaw.agent.autoreply.AutoReplyTypes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;

/**
 * Block reply pipeline — enqueues payloads through an optional coalescer
 * and buffer, then sends them with timeout protection.
 * Mirrors {@code auto-reply/reply/block-reply-pipeline.ts}.
 */
public final class BlockReplyPipeline {

    private static final Logger log = LoggerFactory.getLogger(BlockReplyPipeline.class);

    private BlockReplyPipeline() {
    }

    /** Pipeline interface returned by {@link #createBlockReplyPipeline}. */
    public interface Pipeline {
        void enqueue(AutoReplyTypes.ReplyPayload payload);

        CompletableFuture<Void> flush(boolean force);

        void stop();

        boolean hasBuffered();

        boolean didStream();

        boolean isAborted();

        boolean hasSentPayload(AutoReplyTypes.ReplyPayload payload);
    }

    /** Optional payload buffer (e.g. for audio-as-voice). */
    public interface BlockReplyBuffer {
        boolean shouldBuffer(AutoReplyTypes.ReplyPayload payload);

        default void onEnqueue(AutoReplyTypes.ReplyPayload payload) {
        }

        default AutoReplyTypes.ReplyPayload finalize(AutoReplyTypes.ReplyPayload payload) {
            return payload;
        }
    }

    /** Options for creating a pipeline. */
    public record PipelineOptions(
            BiConsumer<AutoReplyTypes.ReplyPayload, Long> onBlockReply,
            long timeoutMs,
            BlockStreamingTypes.BlockStreamingCoalescing coalescing,
            BlockReplyBuffer buffer) {
    }

    /**
     * Create a deterministic payload key for deduplication.
     */
    public static String createBlockReplyPayloadKey(AutoReplyTypes.ReplyPayload payload) {
        String text = payload.text() != null ? payload.text().trim() : "";
        List<String> mediaList;
        if (payload.mediaUrls() != null && !payload.mediaUrls().isEmpty()) {
            mediaList = payload.mediaUrls();
        } else if (payload.mediaUrl() != null && !payload.mediaUrl().isEmpty()) {
            mediaList = List.of(payload.mediaUrl());
        } else {
            mediaList = List.of();
        }
        return text + "|" + String.join(",", mediaList)
                + "|" + (payload.replyToId() != null ? payload.replyToId() : "");
    }

    /**
     * Create a block reply pipeline.
     */
    public static Pipeline createBlockReplyPipeline(PipelineOptions options) {
        final Set<String> sentKeys = ConcurrentHashMap.newKeySet();
        final Set<String> pendingKeys = ConcurrentHashMap.newKeySet();
        final Set<String> seenKeys = ConcurrentHashMap.newKeySet();
        final Set<String> bufferedKeys = ConcurrentHashMap.newKeySet();
        final Set<String> bufferedPayloadKeys = ConcurrentHashMap.newKeySet();
        final List<AutoReplyTypes.ReplyPayload> bufferedPayloads = Collections.synchronizedList(new ArrayList<>());
        final CompletableFuture<Void>[] sendChain = new CompletableFuture[] { CompletableFuture.completedFuture(null) };
        final boolean[] aborted = { false };
        final boolean[] didStream = { false };
        final boolean[] didLogTimeout = { false };

        final BiConsumer<AutoReplyTypes.ReplyPayload, Long> onBlockReply = options.onBlockReply();
        final long timeoutMs = options.timeoutMs();

        // Coalescer (optional)
        final BlockReplyCoalescer.Coalescer coalescer;
        if (options.coalescing() != null) {
            coalescer = BlockReplyCoalescer.create(
                    options.coalescing(),
                    () -> aborted[0],
                    (payload) -> {
                        bufferedKeys.clear();
                        sendPayloadInternal(payload, false, sentKeys, pendingKeys, seenKeys,
                                sendChain, aborted, didStream, didLogTimeout, onBlockReply, timeoutMs);
                    });
        } else {
            coalescer = null;
        }

        return new Pipeline() {

            private void sendPayload(AutoReplyTypes.ReplyPayload payload, boolean skipSeen) {
                sendPayloadInternal(payload, skipSeen, sentKeys, pendingKeys, seenKeys,
                        sendChain, aborted, didStream, didLogTimeout, onBlockReply, timeoutMs);
            }

            @Override
            public void enqueue(AutoReplyTypes.ReplyPayload payload) {
                if (aborted[0])
                    return;

                // Buffer check
                if (options.buffer() != null) {
                    options.buffer().onEnqueue(payload);
                    if (options.buffer().shouldBuffer(payload)) {
                        String payloadKey = createBlockReplyPayloadKey(payload);
                        if (seenKeys.contains(payloadKey) || sentKeys.contains(payloadKey)
                                || pendingKeys.contains(payloadKey) || bufferedPayloadKeys.contains(payloadKey)) {
                            return;
                        }
                        seenKeys.add(payloadKey);
                        bufferedPayloadKeys.add(payloadKey);
                        bufferedPayloads.add(payload);
                        return;
                    }
                }

                boolean hasMedia = (payload.mediaUrl() != null && !payload.mediaUrl().isEmpty())
                        || (payload.mediaUrls() != null && !payload.mediaUrls().isEmpty());
                if (hasMedia) {
                    if (coalescer != null)
                        coalescer.flush(true);
                    sendPayload(payload, false);
                    return;
                }
                if (coalescer != null) {
                    String payloadKey = createBlockReplyPayloadKey(payload);
                    if (seenKeys.contains(payloadKey) || pendingKeys.contains(payloadKey)
                            || bufferedKeys.contains(payloadKey)) {
                        return;
                    }
                    bufferedKeys.add(payloadKey);
                    coalescer.enqueue(payload);
                    return;
                }
                sendPayload(payload, false);
            }

            @Override
            public CompletableFuture<Void> flush(boolean force) {
                if (coalescer != null)
                    coalescer.flush(force);
                flushBuffered();
                return sendChain[0];
            }

            private void flushBuffered() {
                if (bufferedPayloads.isEmpty())
                    return;
                List<AutoReplyTypes.ReplyPayload> snapshot;
                synchronized (bufferedPayloads) {
                    snapshot = new ArrayList<>(bufferedPayloads);
                    bufferedPayloads.clear();
                    bufferedPayloadKeys.clear();
                }
                for (var payload : snapshot) {
                    var finalPayload = options.buffer() != null
                            ? options.buffer().finalize(payload)
                            : payload;
                    sendPayload(finalPayload, true);
                }
            }

            @Override
            public void stop() {
                if (coalescer != null)
                    coalescer.stop();
            }

            @Override
            public boolean hasBuffered() {
                return (coalescer != null && coalescer.hasBuffered()) || !bufferedPayloads.isEmpty();
            }

            @Override
            public boolean didStream() {
                return didStream[0];
            }

            @Override
            public boolean isAborted() {
                return aborted[0];
            }

            @Override
            public boolean hasSentPayload(AutoReplyTypes.ReplyPayload payload) {
                return sentKeys.contains(createBlockReplyPayloadKey(payload));
            }
        };
    }

    /* ── internal delivery ───────────────────────────────────── */

    @SuppressWarnings("unchecked")
    private static void sendPayloadInternal(
            AutoReplyTypes.ReplyPayload payload,
            boolean skipSeen,
            Set<String> sentKeys,
            Set<String> pendingKeys,
            Set<String> seenKeys,
            CompletableFuture<Void>[] sendChain,
            boolean[] aborted,
            boolean[] didStream,
            boolean[] didLogTimeout,
            BiConsumer<AutoReplyTypes.ReplyPayload, Long> onBlockReply,
            long timeoutMs) {

        if (aborted[0])
            return;
        String payloadKey = createBlockReplyPayloadKey(payload);
        if (!skipSeen) {
            if (seenKeys.contains(payloadKey))
                return;
            seenKeys.add(payloadKey);
        }
        if (sentKeys.contains(payloadKey) || pendingKeys.contains(payloadKey))
            return;
        pendingKeys.add(payloadKey);

        sendChain[0] = sendChain[0].thenRunAsync(() -> {
            if (aborted[0])
                return;
            try {
                CompletableFuture<Void> delivery = CompletableFuture
                        .runAsync(() -> onBlockReply.accept(payload, timeoutMs));
                if (timeoutMs > 0) {
                    delivery.get(timeoutMs, TimeUnit.MILLISECONDS);
                } else {
                    delivery.join();
                }
                sentKeys.add(payloadKey);
                didStream[0] = true;
            } catch (TimeoutException e) {
                aborted[0] = true;
                if (!didLogTimeout[0]) {
                    didLogTimeout[0] = true;
                    log.warn("block reply delivery timed out after {}ms", timeoutMs);
                }
            } catch (Exception e) {
                log.warn("block reply delivery failed: {}", String.valueOf(e));
            } finally {
                pendingKeys.remove(payloadKey);
            }
        });
    }
}
