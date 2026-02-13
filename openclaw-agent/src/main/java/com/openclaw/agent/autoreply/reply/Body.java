package com.openclaw.agent.autoreply.reply;

/**
 * Apply session hints to the agent request body (aborted flag, message ID).
 * Mirrors {@code auto-reply/reply/body.ts}.
 */
public final class Body {

    private Body() {
    }

    /**
     * Apply session hints to the agent body.
     *
     * @param baseBody       the original body text
     * @param abortedLastRun whether the previous run was aborted
     * @param messageId      optional message ID to embed
     * @return body with hints prepended/appended
     */
    public static String applySessionHints(
            String baseBody,
            boolean abortedLastRun,
            String messageId) {

        String result = baseBody;

        if (abortedLastRun) {
            result = "Note: The previous agent run was aborted by the user. "
                    + "Resume carefully or ask for clarification.\n\n" + result;
        }

        String trimmedId = messageId != null ? messageId.trim() : "";
        if (!trimmedId.isEmpty()) {
            result = result + "\n[message_id: " + trimmedId + "]";
        }

        return result;
    }
}
