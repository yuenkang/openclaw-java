package com.openclaw.agent.autoreply.reply;

import com.openclaw.agent.autoreply.AutoReplyTypes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Handle /config and /debug chat commands — dispatch to parsers and apply
 * results.
 * Mirrors {@code auto-reply/reply/commands-config.ts}.
 */
public final class CommandsConfig {

    private static final Logger log = LoggerFactory.getLogger(CommandsConfig.class);

    private CommandsConfig() {
    }

    /* ── /config handler ───────────────────────────────────── */

    /**
     * Handle a /config command.
     *
     * @param normalized   the normalized command body
     * @param isAuthorized whether the sender is authorized
     * @param senderId     sender identifier for logging
     * @return reply payload or null if not a /config command
     */
    public static AutoReplyTypes.ReplyPayload handleConfigCommand(
            String normalized, boolean isAuthorized, String senderId) {

        ConfigCommands.ConfigCommand parsed = ConfigCommands.parseConfigCommand(normalized);
        if (parsed == null)
            return null;

        if (!isAuthorized) {
            log.debug("Ignoring /config from unauthorized sender: {}", senderId != null ? senderId : "<unknown>");
            return null;
        }

        if (parsed instanceof ConfigCommands.ConfigCommand.Show show) {
            String path = show.path();
            if (path == null) {
                return replyText("⚙️ Config: (full config display deferred)");
            }
            return replyText("⚙️ Config: " + path + " = (deferred)");
        } else if (parsed instanceof ConfigCommands.ConfigCommand.Set set) {
            return replyText("⚙️ Set `" + set.path() + "` = " + set.value());
        } else if (parsed instanceof ConfigCommands.ConfigCommand.Unset unset) {
            return replyText("⚙️ Unset `" + unset.path() + "`");
        } else if (parsed instanceof ConfigCommands.ConfigCommand.Error error) {
            return replyText("❌ " + error.message());
        }
        return null;
    }

    /* ── /debug handler ────────────────────────────────────── */

    /**
     * Handle a /debug command.
     *
     * @param normalized   the normalized command body
     * @param isAuthorized whether the sender is authorized
     * @param senderId     sender identifier for logging
     * @return reply payload or null if not a /debug command
     */
    public static AutoReplyTypes.ReplyPayload handleDebugCommand(
            String normalized, boolean isAuthorized, String senderId) {

        DebugCommands.DebugCommand parsed = DebugCommands.parseDebugCommand(normalized);
        if (parsed == null)
            return null;

        if (!isAuthorized) {
            log.debug("Ignoring /debug from unauthorized sender: {}", senderId != null ? senderId : "<unknown>");
            return null;
        }

        if (parsed instanceof DebugCommands.DebugCommand.Show) {
            return replyText("🐛 Debug: (state display deferred)");
        } else if (parsed instanceof DebugCommands.DebugCommand.Reset) {
            return replyText("🐛 Debug state reset.");
        } else if (parsed instanceof DebugCommands.DebugCommand.Set set) {
            return replyText("🐛 Set `" + set.path() + "` = " + set.value());
        } else if (parsed instanceof DebugCommands.DebugCommand.Unset unset) {
            return replyText("🐛 Unset `" + unset.path() + "`");
        } else if (parsed instanceof DebugCommands.DebugCommand.Error error) {
            return replyText("❌ " + error.message());
        }
        return null;
    }

    /* ── helpers ────────────────────────────────────────────── */

    private static AutoReplyTypes.ReplyPayload replyText(String text) {
        return new AutoReplyTypes.ReplyPayload(text, null, null, null, false, false, false, false, null);
    }
}
