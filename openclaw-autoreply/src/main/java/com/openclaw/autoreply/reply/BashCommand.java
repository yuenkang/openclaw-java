package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Chat-level bash command handler — parse !/... and /bash ... commands,
 * run foreground/background shell sessions, poll, and stop active jobs.
 * Mirrors {@code auto-reply/reply/bash-command.ts}.
 */
public final class BashCommand {

    private static final Logger log = LoggerFactory.getLogger(BashCommand.class);
    private static final String CHAT_BASH_SCOPE_KEY = "chat:bash";
    private static final int DEFAULT_FOREGROUND_MS = 2000;
    private static final int MAX_FOREGROUND_MS = 30_000;

    private BashCommand() {
    }

    // --- Request model ---

    sealed interface BashRequest {
        record Help() implements BashRequest {
        }

        record Run(String command) implements BashRequest {
        }

        record Poll(String sessionId) implements BashRequest {
        }

        record Stop(String sessionId) implements BashRequest {
        }
    }

    // --- Active job tracking ---

    private static volatile Map<String, Object> activeJob = null;

    @SuppressWarnings("unchecked")
    static int resolveForegroundMs(Map<String, Object> cfg) {
        Object commands = cfg.get("commands");
        if (!(commands instanceof Map<?, ?> m))
            return DEFAULT_FOREGROUND_MS;
        Object raw = m.get("bashForegroundMs");
        if (!(raw instanceof Number n))
            return DEFAULT_FOREGROUND_MS;
        return Math.max(0, Math.min(n.intValue(), MAX_FOREGROUND_MS));
    }

    static String formatSessionSnippet(String sessionId) {
        if (sessionId == null)
            return "?";
        String trimmed = sessionId.trim();
        return trimmed.length() <= 12 ? trimmed : trimmed.substring(0, 8) + "…";
    }

    static String formatOutputBlock(String text) {
        String trimmed = text != null ? text.trim() : "";
        if (trimmed.isEmpty())
            return "(no output)";
        return "```txt\n" + trimmed + "\n```";
    }

    // --- Parser ---

    private static final Pattern BASH_CMD_RE = Pattern.compile(
            "^/bash(?:\\s*:\\s*|\\s+|$)([\\s\\S]*)$", Pattern.CASE_INSENSITIVE);

    static BashRequest parseBashRequest(String raw) {
        String trimmed = raw.stripLeading();
        String restSource;

        if (trimmed.toLowerCase().startsWith("/bash")) {
            Matcher m = BASH_CMD_RE.matcher(trimmed);
            if (!m.matches())
                return null;
            restSource = m.group(1) != null ? m.group(1) : "";
        } else if (trimmed.startsWith("!")) {
            restSource = trimmed.substring(1);
            if (restSource.stripLeading().startsWith(":")) {
                restSource = restSource.stripLeading().substring(1);
            }
        } else {
            return null;
        }

        String rest = restSource.stripLeading();
        if (rest.isEmpty())
            return new BashRequest.Help();

        String[] parts = rest.split("\\s+", 2);
        String token = parts[0].toLowerCase();
        String remainder = parts.length > 1 ? parts[1].trim() : "";

        return switch (token) {
            case "poll" -> new BashRequest.Poll(remainder.isEmpty() ? null : remainder);
            case "stop" -> new BashRequest.Stop(remainder.isEmpty() ? null : remainder);
            case "help" -> new BashRequest.Help();
            default -> new BashRequest.Run(rest);
        };
    }

    static Map<String, Object> buildUsageReply() {
        return Map.of("text", String.join("\n",
                "⚙️ Usage:",
                "- ! <command>",
                "- !poll | ! poll",
                "- !stop | ! stop",
                "- /bash ... (alias; same subcommands as !)"));
    }

    static String formatElevatedUnavailableMessage(
            boolean runtimeSandboxed,
            List<Map<String, String>> failures,
            String sessionKey) {
        List<String> lines = new ArrayList<>();
        lines.add("elevated is not available right now (runtime="
                + (runtimeSandboxed ? "sandboxed" : "direct") + ").");
        if (failures != null && !failures.isEmpty()) {
            String failStr = failures.stream()
                    .map(f -> f.get("gate") + " (" + f.get("key") + ")")
                    .reduce((a, b) -> a + ", " + b).orElse("");
            lines.add("Failing gates: " + failStr);
        } else {
            lines.add("Failing gates: enabled (tools.elevated.enabled / "
                    + "agents.list[].tools.elevated.enabled), "
                    + "allowFrom (tools.elevated.allowFrom.<provider>).");
        }
        lines.add("Fix-it keys:");
        lines.add("- tools.elevated.enabled");
        lines.add("- tools.elevated.allowFrom.<provider>");
        lines.add("- agents.list[].tools.elevated.enabled");
        lines.add("- agents.list[].tools.elevated.allowFrom.<provider>");
        if (sessionKey != null) {
            lines.add("See: `openclaw sandbox explain --session " + sessionKey + "`");
        }
        return String.join("\n", lines);
    }

    /**
     * Handle a bash chat command (!command, /bash command).
     *
     * @param ctx        inbound message context
     * @param cfg        agent config
     * @param agentId    resolved agent id (nullable)
     * @param sessionKey current session key
     * @param isGroup    whether this is a group message
     * @param elevated   elevated mode state
     * @return reply payload
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<Map<String, Object>> handleBashChatCommand(
            Map<String, Object> ctx,
            Map<String, Object> cfg,
            String agentId,
            String sessionKey,
            boolean isGroup,
            Map<String, Object> elevated) {

        // Check bash enabled
        Object commands = cfg.get("commands");
        boolean bashEnabled = false;
        if (commands instanceof Map<?, ?> m) {
            bashEnabled = Boolean.TRUE.equals(m.get("bash"));
        }
        if (!bashEnabled) {
            return CompletableFuture.completedFuture(Map.of("text",
                    "⚠️ bash is disabled. Set commands.bash=true to enable."));
        }

        // Check elevated
        boolean elevEnabled = Boolean.TRUE.equals(elevated.get("enabled"));
        boolean elevAllowed = Boolean.TRUE.equals(elevated.get("allowed"));
        if (!elevEnabled || !elevAllowed) {
            List<Map<String, String>> failures = (List<Map<String, String>>) elevated.getOrDefault("failures",
                    List.of());
            return CompletableFuture.completedFuture(Map.of("text",
                    formatElevatedUnavailableMessage(false, failures, sessionKey)));
        }

        // Parse command
        String rawBody = resolveRawCommandBody(ctx, cfg, agentId, isGroup).trim();
        BashRequest request = parseBashRequest(rawBody);
        if (request == null) {
            return CompletableFuture.completedFuture(
                    Map.of("text", "⚠️ Unrecognized bash request."));
        }

        if (request instanceof BashRequest.Help) {
            return CompletableFuture.completedFuture(buildUsageReply());
        }

        if (request instanceof BashRequest.Poll poll) {
            String sid = poll.sessionId();
            if (sid == null || sid.isEmpty()) {
                return CompletableFuture.completedFuture(
                        Map.of("text", "⚙️ No active bash job."));
            }
            // Full process registry integration deferred
            return CompletableFuture.completedFuture(
                    Map.of("text", "⚙️ Bash poll: session registry integration deferred."));
        }

        if (request instanceof BashRequest.Stop stop) {
            String sid = stop.sessionId();
            if (sid == null || sid.isEmpty()) {
                return CompletableFuture.completedFuture(
                        Map.of("text", "⚙️ No active bash job."));
            }
            // Full process registry integration deferred
            return CompletableFuture.completedFuture(
                    Map.of("text", "⚙️ Bash stop: session registry integration deferred."));
        }

        if (request instanceof BashRequest.Run run) {
            String command = run.command().trim();
            if (command.isEmpty()) {
                return CompletableFuture.completedFuture(buildUsageReply());
            }
            // Full exec tool integration deferred
            log.info("Chat bash: would execute '{}' for session {}", command, sessionKey);
            return CompletableFuture.completedFuture(
                    Map.of("text", "⚙️ bash: exec tool integration deferred for: " + command));
        }

        return CompletableFuture.completedFuture(buildUsageReply());
    }

    private static String resolveRawCommandBody(
            Map<String, Object> ctx, Map<String, Object> cfg,
            String agentId, boolean isGroup) {
        String source = coalesce(
                (String) ctx.get("CommandBody"),
                (String) ctx.get("RawBody"),
                (String) ctx.get("Body"));
        if (source == null)
            source = "";
        String stripped = Mentions.stripStructuralPrefixes(source);
        return isGroup ? Mentions.stripMentions(stripped, List.of()) : stripped;
    }

    private static String coalesce(String... values) {
        for (String v : values)
            if (v != null)
                return v;
        return "";
    }

    /** Reset active job state (for testing). */
    public static void resetForTests() {
        activeJob = null;
    }
}
