package com.openclaw.agent.autoreply;

/**
 * Public re-export façade for the reply pipeline.
 * Mirrors {@code auto-reply/reply.ts} barrel export.
 *
 * <p>
 * Delegates to:
 * <ul>
 * <li>{@link com.openclaw.agent.autoreply.reply.Directives} — extract
 * think/verbose/elevated/reasoning directives</li>
 * <li>{@link com.openclaw.agent.autoreply.reply.GetReply} —
 * getReplyFromConfig</li>
 * <li>{@link com.openclaw.agent.autoreply.reply.Exec} —
 * extractExecDirective</li>
 * <li>{@link com.openclaw.agent.autoreply.reply.Queue} —
 * extractQueueDirective</li>
 * <li>{@link com.openclaw.agent.autoreply.reply.ReplyTags} —
 * extractReplyToTag</li>
 * </ul>
 */
public final class Reply {

    private Reply() {
    }
}
