package com.openclaw.autoreply.reply;

/**
 * Queue facade — re-exports from queue sub-package.
 * Mirrors {@code auto-reply/reply/queue.ts}.
 * <p>
 * In Java, callers should use the specific queue classes directly:
 * <ul>
 * <li>{@link com.openclaw.autoreply.reply.queue.QueueDirective} —
 * directive extraction</li>
 * <li>{@link com.openclaw.autoreply.reply.queue.QueueCleanup} — session
 * queue cleanup</li>
 * <li>{@link com.openclaw.autoreply.reply.queue.QueueDrain} — followup
 * drain scheduling</li>
 * <li>{@link com.openclaw.autoreply.reply.queue.QueueEnqueue} — followup
 * enqueue</li>
 * <li>{@link com.openclaw.autoreply.reply.queue.QueueSettingsResolver} —
 * settings resolution</li>
 * <li>{@link com.openclaw.autoreply.reply.queue.QueueState} — queue state
 * management</li>
 * <li>{@link com.openclaw.autoreply.reply.queue.QueueTypes} — queue type
 * definitions</li>
 * </ul>
 */
public final class QueueFacade {

    private QueueFacade() {
    }

    /* Convenience delegates — use the specific classes directly. */
}
