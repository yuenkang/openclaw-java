package com.openclaw.agent.autoreply.reply;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Queue directive validation — checks /queue inline directives for errors
 * and renders current queue status.
 * Mirrors {@code auto-reply/reply/directive-handling.queue-validation.ts}.
 */
public final class DirectiveHandlingQueueValidation {

    private DirectiveHandlingQueueValidation() {
    }

    /**
     * Validate and handle inline /queue directives.
     *
     * @return reply payload (status or error) or null if no queue directive present
     */
    public static AutoReplyTypes.ReplyPayload maybeHandleQueueDirective(
            DirectiveHandlingParse.InlineDirectives directives,
            String channel,
            Map<String, Object> sessionEntry) {

        if (!directives.hasQueueDirective()) {
            return null;
        }

        boolean wantsStatus = directives.queueMode() == null
                && !directives.queueReset();

        if (wantsStatus) {
            // Show current queue settings (simplified — full settings resolution deferred)
            String text = DirectiveHandlingShared.withOptions(
                    "Current queue settings: mode=steer, debounce=default, cap=default, drop=default.",
                    "modes steer, followup, collect, steer+backlog, interrupt; "
                            + "debounce:<ms|s|m>, cap:<n>, drop:old|new|summarize");
            return new AutoReplyTypes.ReplyPayload(
                    text, null, null, null,
                    false, false, false, false, null);
        }

        // Validation
        List<String> errors = new ArrayList<>();
        if (directives.queueMode() == null && !directives.queueReset()) {
            // Raw queue mode provided but unrecognized
            errors.add("Unrecognized queue mode. Valid modes: steer, followup, collect, steer+backlog, interrupt.");
        }

        if (!errors.isEmpty()) {
            return new AutoReplyTypes.ReplyPayload(
                    String.join(" ", errors), null, null, null,
                    false, false, false, false, null);
        }

        return null;
    }
}
