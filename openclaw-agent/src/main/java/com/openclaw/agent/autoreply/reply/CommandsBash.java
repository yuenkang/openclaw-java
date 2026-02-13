package com.openclaw.agent.autoreply.reply;

import com.openclaw.agent.autoreply.AutoReplyTypes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Thin wrapper for /bash command handling — delegates to BashCommand.
 * Mirrors {@code auto-reply/reply/commands-bash.ts}.
 */
public final class CommandsBash {

    private static final Logger log = LoggerFactory.getLogger(CommandsBash.class);

    private CommandsBash() {
    }

    /**
     * Check if the normalized command body is a /bash or ! command.
     *
     * @return true if the message is a bash command request
     */
    public static boolean isBashCommand(String commandBodyNormalized) {
        if (commandBodyNormalized == null)
            return false;
        return commandBodyNormalized.equals("/bash")
                || commandBodyNormalized.startsWith("/bash ")
                || commandBodyNormalized.startsWith("!");
    }

    /**
     * Check if the bash command should be handled (respects text commands and
     * auth).
     *
     * @return reply payload or null if not a bash command
     */
    public static AutoReplyTypes.ReplyPayload checkBashCommand(
            String commandBodyNormalized, boolean allowTextCommands,
            boolean isAuthorizedSender, String senderId) {

        if (!allowTextCommands)
            return null;

        boolean slashRequested = commandBodyNormalized.equals("/bash")
                || commandBodyNormalized.startsWith("/bash ");
        boolean bangRequested = commandBodyNormalized.startsWith("!");

        if (!slashRequested && !(bangRequested && isAuthorizedSender))
            return null;

        if (!isAuthorizedSender) {
            log.debug("Ignoring /bash from unauthorized sender: {}",
                    senderId != null ? senderId : "<unknown>");
            return null; // shouldContinue: false
        }

        // Full bash execution deferred to BashCommand integration
        return new AutoReplyTypes.ReplyPayload(
                "⚙️ Bash command handling deferred to runtime integration.",
                null, null, null, false, false, false, false, null);
    }
}
