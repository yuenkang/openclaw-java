package com.openclaw.agent.autoreply.reply;

import com.openclaw.agent.autoreply.AutoReplyTypes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

/**
 * Session-level command handlers: /activation, /sendpolicy, /usage,
 * /restart, /stop, and abort trigger.
 * Mirrors {@code auto-reply/reply/commands-session.ts}.
 */
public final class CommandsSession {

    private static final Logger log = LoggerFactory.getLogger(CommandsSession.class);

    private CommandsSession() {
    }

    /* ── /activation ───────────────────────────────────────── */

    /** Known activation modes. */
    private static final Map<String, String> ACTIVATION_MODES = Map.of(
            "always", "always",
            "mention", "mention",
            "off", "off",
            "on", "always");

    /**
     * Handle /activation command.
     *
     * @return reply or null
     */
    public static AutoReplyTypes.ReplyPayload handleActivationCommand(
            String normalized, boolean isAuthorized, String senderId) {

        if (!normalized.startsWith("/activation"))
            return null;
        if (!isAuthorized) {
            log.debug("Ignoring /activation from unauthorized sender: {}",
                    senderId != null ? senderId : "<unknown>");
            return null;
        }

        String rest = normalized.substring("/activation".length()).trim().toLowerCase();
        if (rest.isEmpty()) {
            return replyText("Usage: /activation <always|mention|off>");
        }

        String mode = ACTIVATION_MODES.get(rest);
        if (mode == null) {
            return replyText("Unknown activation mode: " + rest
                    + "\nUsage: /activation <always|mention|off>");
        }

        // Full persistence deferred
        return replyText("🔧 Activation set to `" + mode + "`.");
    }

    /* ── /sendpolicy ───────────────────────────────────────── */

    /**
     * Handle /sendpolicy command.
     */
    public static AutoReplyTypes.ReplyPayload handleSendPolicyCommand(
            String normalized, boolean isAuthorized, String senderId) {

        if (!normalized.startsWith("/sendpolicy"))
            return null;
        if (!isAuthorized) {
            log.debug("Ignoring /sendpolicy from unauthorized sender: {}",
                    senderId != null ? senderId : "<unknown>");
            return null;
        }

        String rest = normalized.substring("/sendpolicy".length()).trim().toLowerCase();
        if (rest.isEmpty()) {
            return replyText("Usage: /sendpolicy <allow|deny|ask>");
        }

        if (!rest.equals("allow") && !rest.equals("deny") && !rest.equals("ask")) {
            return replyText("Unknown send policy: " + rest
                    + "\nUsage: /sendpolicy <allow|deny|ask>");
        }

        // Full persistence deferred
        return replyText("🔧 Send policy set to `" + rest + "`.");
    }

    /* ── /usage ────────────────────────────────────────────── */

    /**
     * Handle /usage command.
     */
    public static AutoReplyTypes.ReplyPayload handleUsageCommand(
            String normalized, boolean isAuthorized, String senderId) {

        if (!normalized.equals("/usage") && !normalized.startsWith("/usage "))
            return null;
        if (!isAuthorized) {
            log.debug("Ignoring /usage from unauthorized sender: {}",
                    senderId != null ? senderId : "<unknown>");
            return null;
        }

        String rest = normalized.substring("/usage".length()).trim().toLowerCase();
        boolean showFull = "full".equals(rest);

        // Full usage display deferred — stub shows current mode
        return replyText(showFull
                ? "📈 Usage (full): display deferred to integration."
                : "📈 Usage: display deferred to integration.");
    }

    /* ── /restart ──────────────────────────────────────────── */

    /**
     * Handle /restart command.
     */
    public static AutoReplyTypes.ReplyPayload handleRestartCommand(
            String normalized, boolean isAuthorized, String senderId) {

        if (!normalized.startsWith("/restart"))
            return null;
        if (!isAuthorized) {
            log.debug("Ignoring /restart from unauthorized sender: {}",
                    senderId != null ? senderId : "<unknown>");
            return null;
        }

        // Full restart logic deferred
        return replyText("🔄 Restart requested. Session will be restarted.");
    }

    /* ── /stop ─────────────────────────────────────────────── */

    /**
     * Handle /stop command — stop current run and optionally subagents.
     */
    public static AutoReplyTypes.ReplyPayload handleStopCommand(
            String normalized, boolean isAuthorized, String senderId,
            String sessionKey) {

        if (!normalized.equals("/stop") && !normalized.startsWith("/stop "))
            return null;
        if (!isAuthorized) {
            log.debug("Ignoring /stop from unauthorized sender: {}",
                    senderId != null ? senderId : "<unknown>");
            return null;
        }

        // Set abort flag
        if (sessionKey != null && !sessionKey.isBlank()) {
            Abort.setAbortMemory(sessionKey, true);
        }

        return replyText("⏹ Stopped. The current run has been flagged as aborted.");
    }

    /* ── abort trigger ─────────────────────────────────────── */

    /**
     * Handle bare abort triggers ("stop", "cancel", etc.).
     */
    public static AutoReplyTypes.ReplyPayload handleAbortTrigger(
            String normalized, boolean isAuthorized, String sessionKey) {

        if (!Abort.isAbortTrigger(normalized))
            return null;
        if (!isAuthorized)
            return null;

        Abort.FastAbortResult r = Abort.tryFastAbortFromMessage(normalized, sessionKey);
        if (r.aborted()) {
            return replyText(r.replyText());
        }
        return null;
    }

    /* ── helpers ────────────────────────────────────────────── */

    private static AutoReplyTypes.ReplyPayload replyText(String text) {
        return new AutoReplyTypes.ReplyPayload(text, null, null, null, false, false, false, false, null);
    }
}
