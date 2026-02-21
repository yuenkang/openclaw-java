package com.openclaw.autoreply.reply;

import com.openclaw.autoreply.AutoReplyTypes;

import java.util.ArrayList;
import java.util.List;

/**
 * Info command handlers: /help, /commands, /status, /context, /whoami.
 * Mirrors {@code auto-reply/reply/commands-info.ts}.
 */
public final class CommandsInfo {

    private CommandsInfo() {
    }

    /* ── /help ──────────────────────────────────────────────── */

    /**
     * Build a /help response.
     */
    public static AutoReplyTypes.ReplyPayload handleHelp() {
        String text = """
                🤖 OpenClaw Agent Commands

                /help — Show this message
                /commands — List available commands
                /status — Show current session status
                /context — Show context usage
                /whoami — Show your identity
                /config — View/edit configuration
                /debug — View/edit debug settings
                /compact — Compact session history
                /ptt — Push-to-talk controls
                /approve — Resolve exec approvals
                /subagents — Manage sub-agents
                /stop — Stop current run
                /new, /reset — Start a new session""";
        return new AutoReplyTypes.ReplyPayload(text, null, null, null, false, false, false, false, null);
    }

    /* ── /whoami ────────────────────────────────────────────── */

    /**
     * Build a /whoami response.
     */
    public static AutoReplyTypes.ReplyPayload handleWhoami(
            String channel, String senderId, String senderUsername,
            String chatType, String from, String threadId) {

        List<String> lines = new ArrayList<>();
        lines.add("🧭 Identity");
        lines.add("Channel: " + (channel != null ? channel : "unknown"));

        if (senderId != null && !senderId.isEmpty()) {
            lines.add("User id: " + senderId);
        }
        if (senderUsername != null && !senderUsername.isEmpty()) {
            String handle = senderUsername.startsWith("@") ? senderUsername : "@" + senderUsername;
            lines.add("Username: " + handle);
        }
        if ("group".equals(chatType) && from != null && !from.isEmpty()) {
            lines.add("Chat: " + from);
        }
        if (threadId != null && !threadId.isEmpty()) {
            lines.add("Thread: " + threadId);
        }
        if (senderId != null && !senderId.isEmpty()) {
            lines.add("AllowFrom: " + senderId);
        }

        String text = String.join("\n", lines);
        return new AutoReplyTypes.ReplyPayload(text, null, null, null, false, false, false, false, null);
    }

    /* ── /commands pagination ──────────────────────────────── */

    /** Single button for a pagination keyboard row. */
    public record PaginationButton(String text, String callbackData) {
    }

    /**
     * Build a pagination keyboard for /commands.
     */
    public static List<PaginationButton> buildCommandsPaginationKeyboard(
            int currentPage, int totalPages, String agentId) {
        List<PaginationButton> buttons = new ArrayList<>();
        String suffix = agentId != null ? ":" + agentId : "";

        if (currentPage > 1) {
            buttons.add(new PaginationButton("◀ Prev", "commands_page_" + (currentPage - 1) + suffix));
        }
        buttons.add(new PaginationButton(currentPage + "/" + totalPages, "commands_page_noop" + suffix));
        if (currentPage < totalPages) {
            buttons.add(new PaginationButton("Next ▶", "commands_page_" + (currentPage + 1) + suffix));
        }
        return buttons;
    }
}
