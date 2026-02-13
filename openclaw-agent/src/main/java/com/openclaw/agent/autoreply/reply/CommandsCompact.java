package com.openclaw.agent.autoreply.reply;

import com.openclaw.agent.autoreply.AutoReplyTypes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Handle /compact command — compacts session history.
 * Mirrors {@code auto-reply/reply/commands-compact.ts}.
 */
public final class CommandsCompact {

    private static final Logger log = LoggerFactory.getLogger(CommandsCompact.class);

    private CommandsCompact() {
    }

    /**
     * Extract custom compaction instructions from the command body.
     *
     * @param rawBody the raw command body
     * @param isGroup whether this is a group chat
     * @return custom instructions or null
     */
    public static String extractCompactInstructions(String rawBody, boolean isGroup) {
        if (rawBody == null)
            return null;
        String stripped = Mentions.stripStructuralPrefixes(rawBody);
        // In group, strip mentions first (simplified — full implementation uses
        // stripMentions with ctx/cfg/agentId)
        String trimmed = stripped.trim();
        if (trimmed.isEmpty())
            return null;

        String lowered = trimmed.toLowerCase();
        if (!lowered.startsWith("/compact"))
            return null;

        String rest = trimmed.substring("/compact".length()).stripLeading();
        if (rest.startsWith(":"))
            rest = rest.substring(1).stripLeading();
        return rest.isEmpty() ? null : rest;
    }

    /**
     * Check if the normalized command is a /compact request.
     */
    public static boolean isCompactCommand(String commandBodyNormalized) {
        return commandBodyNormalized != null
                && (commandBodyNormalized.equals("/compact")
                        || commandBodyNormalized.startsWith("/compact "));
    }

    /**
     * Handle a /compact command (auth check + stub response).
     *
     * @return reply or null if not a /compact command
     */
    public static AutoReplyTypes.ReplyPayload handleCompactCommand(
            String commandBodyNormalized, boolean isAuthorized, String senderId,
            String sessionId) {

        if (!isCompactCommand(commandBodyNormalized))
            return null;

        if (!isAuthorized) {
            log.debug("Ignoring /compact from unauthorized sender: {}",
                    senderId != null ? senderId : "<unknown>");
            return null;
        }

        if (sessionId == null || sessionId.isBlank()) {
            return new AutoReplyTypes.ReplyPayload(
                    "⚙️ Compaction unavailable (missing session id).",
                    null, null, null, false, false, false, false, null);
        }

        // Full compaction logic deferred to embedded PI integration
        return new AutoReplyTypes.ReplyPayload(
                "⚙️ Compaction requested for session " + sessionId + ".",
                null, null, null, false, false, false, false, null);
    }

    /**
     * Format a compaction result label.
     */
    public static String formatCompactLabel(boolean ok, boolean compacted,
            Integer tokensBefore, Integer tokensAfter) {
        if (!ok)
            return "Compaction failed";
        if (!compacted)
            return "Compaction skipped";
        if (tokensBefore != null && tokensAfter != null) {
            return "Compacted (" + CommandsStatus.formatTokenCount(tokensBefore)
                    + " → " + CommandsStatus.formatTokenCount(tokensAfter) + ")";
        }
        if (tokensBefore != null) {
            return "Compacted (" + CommandsStatus.formatTokenCount(tokensBefore) + " before)";
        }
        return "Compacted";
    }
}
