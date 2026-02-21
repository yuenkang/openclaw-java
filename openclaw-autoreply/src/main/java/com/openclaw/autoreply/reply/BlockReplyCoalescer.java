package com.openclaw.autoreply.reply;

import com.openclaw.autoreply.AutoReplyTypes;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;

/**
 * Block reply coalescer — batches text payloads by min/max char limits
 * with idle flush timeout. Media payloads bypass the buffer.
 * Mirrors {@code auto-reply/reply/block-reply-coalescer.ts}.
 */
public final class BlockReplyCoalescer {

    /** Coalescer interface. */
    public interface Coalescer {
        void enqueue(AutoReplyTypes.ReplyPayload payload);

        void flush(boolean force);

        boolean hasBuffered();

        void stop();
    }

    private BlockReplyCoalescer() {
    }

    /**
     * Create a block reply coalescer.
     *
     * @param config      coalescing parameters (minChars, maxChars, idleMs, joiner,
     *                    flushOnEnqueue)
     * @param shouldAbort supplier that returns true when the operation has been
     *                    aborted
     * @param onFlush     callback that receives coalesced payloads for delivery
     */
    public static Coalescer create(
            BlockStreamingTypes.BlockStreamingCoalescing config,
            BooleanSupplier shouldAbort,
            Consumer<AutoReplyTypes.ReplyPayload> onFlush) {

        final int minChars = Math.max(1, config.minChars());
        final int maxChars = Math.max(minChars, config.maxChars());
        final int idleMs = Math.max(0, config.idleMs());
        final String joiner = config.joiner() != null ? config.joiner() : "";
        final boolean flushOnEnqueue = config.flushOnEnqueue();

        final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "block-coalescer");
            t.setDaemon(true);
            return t;
        });

        return new Coalescer() {
            private String bufferText = "";
            private String bufferReplyToId = null;
            private Boolean bufferAudioAsVoice = null;
            private ScheduledFuture<?> idleTimer = null;

            private void clearIdleTimer() {
                if (idleTimer != null) {
                    idleTimer.cancel(false);
                    idleTimer = null;
                }
            }

            private void resetBuffer() {
                bufferText = "";
                bufferReplyToId = null;
                bufferAudioAsVoice = null;
            }

            private void scheduleIdleFlush() {
                if (idleMs <= 0)
                    return;
                clearIdleTimer();
                idleTimer = scheduler.schedule(() -> flush(false), idleMs, TimeUnit.MILLISECONDS);
            }

            @Override
            public synchronized void flush(boolean force) {
                clearIdleTimer();
                if (shouldAbort.getAsBoolean()) {
                    resetBuffer();
                    return;
                }
                if (bufferText.isEmpty())
                    return;
                if (!force && !flushOnEnqueue && bufferText.length() < minChars) {
                    scheduleIdleFlush();
                    return;
                }
                var payload = new AutoReplyTypes.ReplyPayload(bufferText, null, null, bufferReplyToId,
                        false, false, bufferAudioAsVoice != null && bufferAudioAsVoice, false, null);
                resetBuffer();
                onFlush.accept(payload);
            }

            @Override
            public synchronized void enqueue(AutoReplyTypes.ReplyPayload payload) {
                if (shouldAbort.getAsBoolean())
                    return;
                boolean hasMedia = (payload.mediaUrl() != null && !payload.mediaUrl().isEmpty())
                        || (payload.mediaUrls() != null && !payload.mediaUrls().isEmpty());
                String text = payload.text() != null ? payload.text() : "";
                boolean hasText = !text.trim().isEmpty();

                if (hasMedia) {
                    flush(true);
                    onFlush.accept(payload);
                    return;
                }
                if (!hasText)
                    return;

                // flushOnEnqueue: each payload is a separate paragraph
                if (flushOnEnqueue) {
                    if (!bufferText.isEmpty())
                        flush(true);
                    bufferReplyToId = payload.replyToId();
                    bufferAudioAsVoice = payload.audioAsVoice();
                    bufferText = text;
                    flush(true);
                    return;
                }

                // Flush if reply context changed
                if (!bufferText.isEmpty()) {
                    boolean replyChanged = !java.util.Objects.equals(bufferReplyToId, payload.replyToId());
                    boolean audioChanged = !java.util.Objects.equals(bufferAudioAsVoice, payload.audioAsVoice());
                    if (replyChanged || audioChanged)
                        flush(true);
                }

                if (bufferText.isEmpty()) {
                    bufferReplyToId = payload.replyToId();
                    bufferAudioAsVoice = payload.audioAsVoice();
                }

                String nextText = bufferText.isEmpty() ? text : bufferText + joiner + text;
                if (nextText.length() > maxChars) {
                    if (!bufferText.isEmpty()) {
                        flush(true);
                        bufferReplyToId = payload.replyToId();
                        bufferAudioAsVoice = payload.audioAsVoice();
                        if (text.length() >= maxChars) {
                            onFlush.accept(payload);
                            return;
                        }
                        bufferText = text;
                        scheduleIdleFlush();
                        return;
                    }
                    onFlush.accept(payload);
                    return;
                }

                bufferText = nextText;
                if (bufferText.length() >= maxChars) {
                    flush(true);
                    return;
                }
                scheduleIdleFlush();
            }

            @Override
            public synchronized boolean hasBuffered() {
                return !bufferText.isEmpty();
            }

            @Override
            public void stop() {
                clearIdleTimer();
                scheduler.shutdownNow();
            }
        };
    }
}
