package com.openclaw.agent.autoreply.reply;

import com.openclaw.agent.autoreply.ReplyTokens;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

/**
 * Typing indicator controller with interval loop, TTL auto-stop, and dispatch
 * idle sealing.
 * Mirrors {@code auto-reply/reply/typing.ts}.
 */
public final class Typing {

    private Typing() {
    }

    /** Callback-style typing controller interface. */
    public interface TypingController {
        void onReplyStart();

        void startTypingLoop();

        void startTypingOnText(String text);

        void refreshTypingTtl();

        boolean isActive();

        void markRunComplete();

        void markDispatchIdle();

        void cleanup();
    }

    /**
     * Create a typing controller.
     *
     * @param onReplyStart          callback to trigger typing indicator
     * @param typingIntervalSeconds interval between typing pings (default 6)
     * @param typingTtlMs           max ms to keep typing alive (default 120_000)
     * @param silentToken           token indicating silent reply (default
     *                              SILENT_REPLY_TOKEN)
     * @param logFn                 optional debug logger
     */
    public static TypingController createTypingController(
            Runnable onReplyStart,
            Integer typingIntervalSeconds,
            Long typingTtlMs,
            String silentToken,
            Consumer<String> logFn) {

        final int intervalSec = typingIntervalSeconds != null ? typingIntervalSeconds : 6;
        final long ttlMs = typingTtlMs != null ? typingTtlMs : 2 * 60_000L;
        final String token = silentToken != null ? silentToken : ReplyTokens.SILENT_REPLY_TOKEN;
        final long intervalMs = intervalSec * 1000L;

        return new TypingController() {
            private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor(r -> {
                Thread t = new Thread(r, "typing-controller");
                t.setDaemon(true);
                return t;
            });

            private boolean started = false;
            private boolean active = false;
            private boolean runComplete = false;
            private boolean dispatchIdle = false;
            private boolean sealed = false;
            private ScheduledFuture<?> typingTimer = null;
            private ScheduledFuture<?> ttlTimer = null;

            private void resetCycle() {
                started = false;
                active = false;
                runComplete = false;
                dispatchIdle = false;
            }

            @Override
            public void cleanup() {
                if (sealed)
                    return;
                if (ttlTimer != null) {
                    ttlTimer.cancel(false);
                    ttlTimer = null;
                }
                if (typingTimer != null) {
                    typingTimer.cancel(false);
                    typingTimer = null;
                }
                resetCycle();
                sealed = true;
            }

            @Override
            public void refreshTypingTtl() {
                if (sealed || intervalMs <= 0 || ttlMs <= 0)
                    return;
                if (ttlTimer != null)
                    ttlTimer.cancel(false);
                ttlTimer = scheduler.schedule(() -> {
                    if (typingTimer == null)
                        return;
                    if (logFn != null) {
                        String label = ttlMs % 60_000 == 0 ? (ttlMs / 60_000) + "m" : Math.round(ttlMs / 1000.0) + "s";
                        logFn.accept("typing TTL reached (" + label + "); stopping typing indicator");
                    }
                    cleanup();
                }, ttlMs, TimeUnit.MILLISECONDS);
            }

            @Override
            public boolean isActive() {
                return active && !sealed;
            }

            private void triggerTyping() {
                if (sealed || onReplyStart == null)
                    return;
                onReplyStart.run();
            }

            private void ensureStart() {
                if (sealed || runComplete)
                    return;
                if (!active)
                    active = true;
                if (started)
                    return;
                started = true;
                triggerTyping();
            }

            private void maybeStopOnIdle() {
                if (!active)
                    return;
                if (runComplete && dispatchIdle)
                    cleanup();
            }

            @Override
            public void onReplyStart() {
                ensureStart();
            }

            @Override
            public void startTypingLoop() {
                if (sealed || runComplete)
                    return;
                refreshTypingTtl();
                if (onReplyStart == null || intervalMs <= 0)
                    return;
                if (typingTimer != null)
                    return;
                ensureStart();
                typingTimer = scheduler.scheduleAtFixedRate(this::triggerTyping, intervalMs, intervalMs,
                        TimeUnit.MILLISECONDS);
            }

            @Override
            public void startTypingOnText(String text) {
                if (sealed)
                    return;
                String trimmed = text != null ? text.trim() : "";
                if (trimmed.isEmpty())
                    return;
                if (ReplyTokens.isSilentReplyText(trimmed, token))
                    return;
                refreshTypingTtl();
                startTypingLoop();
            }

            @Override
            public void markRunComplete() {
                runComplete = true;
                maybeStopOnIdle();
            }

            @Override
            public void markDispatchIdle() {
                dispatchIdle = true;
                maybeStopOnIdle();
            }
        };
    }
}
