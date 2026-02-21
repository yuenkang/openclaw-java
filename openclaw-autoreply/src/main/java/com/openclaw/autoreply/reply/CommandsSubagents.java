package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Duration;
import java.time.Instant;
import java.util.*;

/**
 * Handle /subagents command — list, stop, log, info, send subagents.
 * Mirrors {@code auto-reply/reply/commands-subagents.ts}.
 */
public final class CommandsSubagents {

    private static final Logger log = LoggerFactory.getLogger(CommandsSubagents.class);
    private static final String COMMAND = "/subagents";
    private static final Set<String> ACTIONS = Set.of("list", "stop", "log", "send", "info", "help");

    private CommandsSubagents() {
    }

    /** Subagent run record. */
    public record SubagentRunRecord(
            String runId,
            String childSessionKey,
            String task,
            String label,
            String cleanup,
            Long createdAt,
            Long startedAt,
            Long endedAt,
            Long archiveAtMs,
            boolean cleanupHandled,
            boolean abortedLastRun,
            SubagentOutcome outcome) {
    }

    public record SubagentOutcome(String status, String error) {
    }

    static String formatTimestamp(Long valueMs) {
        if (valueMs == null || valueMs <= 0)
            return "n/a";
        return Instant.ofEpochMilli(valueMs).toString();
    }

    static String formatAgeShort(long ms) {
        if (ms < 1000)
            return ms + "ms";
        long sec = ms / 1000;
        if (sec < 60)
            return sec + "s";
        long min = sec / 60;
        if (min < 60)
            return min + "m";
        long hr = min / 60;
        if (hr < 24)
            return hr + "h " + (min % 60) + "m";
        long days = hr / 24;
        return days + "d " + (hr % 24) + "h";
    }

    static String formatDurationShort(long ms) {
        return formatAgeShort(ms);
    }

    static String formatRunStatus(SubagentRunRecord run) {
        if (run.endedAt() != null && run.endedAt() > 0)
            return "✅ Done";
        if (run.startedAt() != null && run.startedAt() > 0)
            return "🔄 Running";
        return "⏳ Pending";
    }

    static String formatRunLabel(SubagentRunRecord run) {
        if (run.label() != null && !run.label().isEmpty())
            return run.label();
        if (run.task() != null && !run.task().isEmpty()) {
            return run.task().length() > 40 ? run.task().substring(0, 37) + "..." : run.task();
        }
        return run.runId().substring(0, Math.min(8, run.runId().length()));
    }

    static String buildSubagentsHelp() {
        return String.join("\n",
                "🧭 Subagents",
                "Usage:",
                "- /subagents list",
                "- /subagents stop <id|#|all>",
                "- /subagents log <id|#> [limit] [tools]",
                "- /subagents info <id|#>",
                "- /subagents send <id|#> <message>",
                "",
                "Ids: use the list index (#), runId prefix, or full session key.");
    }

    /**
     * Handle the /subagents command.
     */
    public static CommandsTypes.CommandHandlerResult handleSubagentsCommand(
            CommandsTypes.HandleCommandsParams params,
            boolean allowTextCommands) {

        if (!allowTextCommands)
            return null;

        String normalized = params.command().commandBodyNormalized();
        if (!normalized.startsWith(COMMAND))
            return null;

        if (!params.command().isAuthorizedSender()) {
            log.debug("Ignoring /subagents from unauthorized sender: {}",
                    params.command().senderId());
            return new CommandsTypes.CommandHandlerResult(null, false);
        }

        String rest = normalized.substring(COMMAND.length()).trim();
        String[] tokens = rest.isEmpty() ? new String[0] : rest.split("\\s+");
        String action = tokens.length > 0 ? tokens[0].toLowerCase() : "list";
        if (!ACTIONS.contains(action)) {
            return new CommandsTypes.CommandHandlerResult(
                    Map.of("text", buildSubagentsHelp()), false);
        }

        if ("help".equals(action)) {
            return new CommandsTypes.CommandHandlerResult(
                    Map.of("text", buildSubagentsHelp()), false);
        }

        if ("list".equals(action)) {
            // Full subagent registry integration deferred
            return new CommandsTypes.CommandHandlerResult(
                    Map.of("text", "🧭 Subagents: none for this session."), false);
        }

        if ("stop".equals(action)) {
            String target = tokens.length > 1 ? tokens[1] : null;
            if (target == null) {
                return new CommandsTypes.CommandHandlerResult(
                        Map.of("text", "⚙️ Usage: /subagents stop <id|#|all>"), false);
            }
            // Full stop integration deferred
            return new CommandsTypes.CommandHandlerResult(
                    Map.of("text", "⚙️ Subagent stop deferred."), false);
        }

        if ("info".equals(action)) {
            String target = tokens.length > 1 ? tokens[1] : null;
            if (target == null) {
                return new CommandsTypes.CommandHandlerResult(
                        Map.of("text", "ℹ️ Usage: /subagents info <id|#>"), false);
            }
            return new CommandsTypes.CommandHandlerResult(
                    Map.of("text", "ℹ️ Subagent info deferred."), false);
        }

        if ("log".equals(action)) {
            String target = tokens.length > 1 ? tokens[1] : null;
            if (target == null) {
                return new CommandsTypes.CommandHandlerResult(
                        Map.of("text", "📜 Usage: /subagents log <id|#> [limit]"), false);
            }
            return new CommandsTypes.CommandHandlerResult(
                    Map.of("text", "📜 Subagent log deferred."), false);
        }

        if ("send".equals(action)) {
            String target = tokens.length > 1 ? tokens[1] : null;
            String message = tokens.length > 2
                    ? String.join(" ", Arrays.copyOfRange(tokens, 2, tokens.length)).trim()
                    : null;
            if (target == null || message == null || message.isEmpty()) {
                return new CommandsTypes.CommandHandlerResult(
                        Map.of("text", "✉️ Usage: /subagents send <id|#> <message>"), false);
            }
            return new CommandsTypes.CommandHandlerResult(
                    Map.of("text", "✉️ Subagent send deferred."), false);
        }

        return new CommandsTypes.CommandHandlerResult(
                Map.of("text", buildSubagentsHelp()), false);
    }
}
