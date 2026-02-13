package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.NumberFormat;
import java.util.*;

/**
 * Handle /context command — build and display a system-prompt / context
 * token budget report with per-file, per-tool, per-skill breakdowns.
 * Mirrors {@code auto-reply/reply/commands-context-report.ts}.
 */
public final class CommandsContextReport {

    private static final Logger log = LoggerFactory.getLogger(CommandsContextReport.class);
    private static final NumberFormat INT_FMT = NumberFormat.getIntegerInstance(Locale.US);

    private CommandsContextReport() {
    }

    static int estimateTokensFromChars(int chars) {
        return (int) Math.ceil(Math.max(0, chars) / 4.0);
    }

    static String formatInt(int n) {
        return INT_FMT.format(n);
    }

    static String formatCharsAndTokens(int chars) {
        return formatInt(chars) + " chars (~" + formatInt(estimateTokensFromChars(chars)) + " tok)";
    }

    static String parseContextArgs(String commandBodyNormalized) {
        if ("/context".equals(commandBodyNormalized))
            return "";
        if (commandBodyNormalized.startsWith("/context ")) {
            return commandBodyNormalized.substring(8).trim();
        }
        return "";
    }

    /**
     * Build the /context reply.
     */
    public static CommandsTypes.CommandHandlerResult buildContextReply(
            CommandsTypes.HandleCommandsParams params) {

        String args = parseContextArgs(params.command().commandBodyNormalized());
        String sub = args.isEmpty() ? "" : args.split("\\s+")[0].toLowerCase();

        if (sub.isEmpty() || "help".equals(sub)) {
            String text = String.join("\n",
                    "🧠 /context",
                    "",
                    "What counts as context (high-level), plus a breakdown mode.",
                    "",
                    "Try:",
                    "- /context list   (short breakdown)",
                    "- /context detail (per-file + per-tool + per-skill + system prompt size)",
                    "- /context json   (same, machine-readable)",
                    "",
                    "Inline shortcut = a command token inside a normal message "
                            + "(e.g. \"hey /status\"). It runs immediately (allowlisted senders only) "
                            + "and is stripped before the model sees the remaining text.");
            return new CommandsTypes.CommandHandlerResult(
                    Map.of("text", text), false);
        }

        if ("json".equals(sub)) {
            // Full report deferred
            return new CommandsTypes.CommandHandlerResult(
                    Map.of("text", "{\"report\": \"deferred\"}"), false);
        }

        if (!"list".equals(sub) && !"show".equals(sub)
                && !"detail".equals(sub) && !"deep".equals(sub)) {
            return new CommandsTypes.CommandHandlerResult(
                    Map.of("text", "Unknown /context mode.\n"
                            + "Use: /context, /context list, /context detail, or /context json"),
                    false);
        }

        // Simplified context report
        List<String> lines = new ArrayList<>();
        lines.add("🧠 Context breakdown");
        lines.add("Workspace: " + params.workspaceDir());
        lines.add("System prompt: (full report deferred)");
        lines.add("");

        @SuppressWarnings("unchecked")
        Map<String, Object> se = params.sessionEntry();
        Integer totalTokens = se != null && se.containsKey("totalTokens")
                ? ((Number) se.get("totalTokens")).intValue()
                : null;
        if (totalTokens != null) {
            lines.add("Session tokens (cached): " + formatInt(totalTokens));
        } else {
            lines.add("Session tokens (cached): unknown");
        }

        return new CommandsTypes.CommandHandlerResult(
                Map.of("text", String.join("\n", lines)), false);
    }
}
