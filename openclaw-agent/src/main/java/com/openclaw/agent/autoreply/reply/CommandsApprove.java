package com.openclaw.agent.autoreply.reply;

import java.util.Map;

/**
 * Parse and handle /approve chat commands for exec approval resolution.
 * Mirrors {@code auto-reply/reply/commands-approve.ts}.
 */
public final class CommandsApprove {

    private CommandsApprove() {
    }

    private static final String COMMAND = "/approve";

    /** Map of decision aliases to canonical decision values. */
    private static final Map<String, String> DECISION_ALIASES = Map.ofEntries(
            Map.entry("allow", "allow-once"),
            Map.entry("once", "allow-once"),
            Map.entry("allow-once", "allow-once"),
            Map.entry("allowonce", "allow-once"),
            Map.entry("always", "allow-always"),
            Map.entry("allow-always", "allow-always"),
            Map.entry("allowalways", "allow-always"),
            Map.entry("deny", "deny"),
            Map.entry("reject", "deny"),
            Map.entry("block", "deny"));

    /** Parsed /approve command result. */
    public sealed interface ParsedApproveCommand {
        record Ok(String id, String decision) implements ParsedApproveCommand {
        }

        record Err(String error) implements ParsedApproveCommand {
        }
    }

    private static final String USAGE = "Usage: /approve <id> allow-once|allow-always|deny";

    /**
     * Parse a raw message text as a /approve command.
     *
     * @return parsed command or null if not a /approve command
     */
    public static ParsedApproveCommand parseApproveCommand(String raw) {
        if (raw == null)
            return null;
        String trimmed = raw.trim();
        if (!trimmed.toLowerCase().startsWith(COMMAND))
            return null;

        String rest = trimmed.substring(COMMAND.length()).trim();
        if (rest.isEmpty()) {
            return new ParsedApproveCommand.Err(USAGE);
        }

        String[] tokens = rest.split("\\s+");
        if (tokens.length < 2) {
            return new ParsedApproveCommand.Err(USAGE);
        }

        String first = tokens[0].toLowerCase();
        String second = tokens[1].toLowerCase();

        // /approve allow-once <id>
        if (DECISION_ALIASES.containsKey(first)) {
            String id = rest.substring(tokens[0].length()).trim();
            return new ParsedApproveCommand.Ok(id, DECISION_ALIASES.get(first));
        }
        // /approve <id> allow-once
        if (DECISION_ALIASES.containsKey(second)) {
            return new ParsedApproveCommand.Ok(tokens[0], DECISION_ALIASES.get(second));
        }

        return new ParsedApproveCommand.Err(USAGE);
    }

    /**
     * Build a human-readable "resolved by" label for audit.
     */
    public static String buildResolvedByLabel(String channel, String senderId) {
        String sender = senderId != null ? senderId : "unknown";
        return channel + ":" + sender;
    }
}
