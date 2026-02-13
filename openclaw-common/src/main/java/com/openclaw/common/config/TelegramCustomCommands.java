package com.openclaw.common.config;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Telegram custom command validation and normalization.
 * Corresponds to TypeScript's telegram-custom-commands.ts.
 */
public final class TelegramCustomCommands {

    private TelegramCustomCommands() {
    }

    public static final Pattern COMMAND_NAME_PATTERN = Pattern.compile("^[a-z0-9_]{1,32}$");

    // =========================================================================
    // Types
    // =========================================================================

    public record CustomCommandInput(String command, String description) {
    }

    public record CustomCommandIssue(int index, String field, String message) {
    }

    public record ResolveResult(
            List<CustomCommandEntry> commands,
            List<CustomCommandIssue> issues) {
    }

    public record CustomCommandEntry(String command, String description) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Normalize a Telegram command name: trim, strip leading '/', lowercase.
     */
    public static String normalizeCommandName(String value) {
        if (value == null)
            return "";
        String trimmed = value.trim();
        if (trimmed.isEmpty())
            return "";
        String withoutSlash = trimmed.startsWith("/") ? trimmed.substring(1) : trimmed;
        return withoutSlash.trim().toLowerCase();
    }

    /**
     * Normalize a Telegram command description: trim.
     */
    public static String normalizeCommandDescription(String value) {
        return value != null ? value.trim() : "";
    }

    /**
     * Resolve and validate a list of custom Telegram commands.
     *
     * @param commands         raw command input list
     * @param reservedCommands set of reserved native command names
     * @param checkReserved    whether to check against reserved commands (default
     *                         true)
     * @param checkDuplicates  whether to check for duplicate names (default true)
     */
    public static ResolveResult resolveTelegramCustomCommands(
            List<CustomCommandInput> commands,
            Set<String> reservedCommands,
            boolean checkReserved,
            boolean checkDuplicates) {

        List<CustomCommandInput> entries = commands != null ? commands : List.of();
        Set<String> reserved = reservedCommands != null ? reservedCommands : Set.of();
        Set<String> seen = new HashSet<>();
        List<CustomCommandEntry> resolved = new ArrayList<>();
        List<CustomCommandIssue> issues = new ArrayList<>();

        for (int i = 0; i < entries.size(); i++) {
            CustomCommandInput entry = entries.get(i);
            String normalized = normalizeCommandName(entry != null ? entry.command() : null);

            if (normalized.isEmpty()) {
                issues.add(new CustomCommandIssue(i, "command",
                        "Telegram custom command is missing a command name."));
                continue;
            }

            if (!COMMAND_NAME_PATTERN.matcher(normalized).matches()) {
                issues.add(new CustomCommandIssue(i, "command",
                        "Telegram custom command \"/" + normalized +
                                "\" is invalid (use a-z, 0-9, underscore; max 32 chars)."));
                continue;
            }

            if (checkReserved && reserved.contains(normalized)) {
                issues.add(new CustomCommandIssue(i, "command",
                        "Telegram custom command \"/" + normalized +
                                "\" conflicts with a native command."));
                continue;
            }

            if (checkDuplicates && seen.contains(normalized)) {
                issues.add(new CustomCommandIssue(i, "command",
                        "Telegram custom command \"/" + normalized + "\" is duplicated."));
                continue;
            }

            String description = normalizeCommandDescription(entry != null ? entry.description() : null);
            if (description.isEmpty()) {
                issues.add(new CustomCommandIssue(i, "description",
                        "Telegram custom command \"/" + normalized + "\" is missing a description."));
                continue;
            }

            if (checkDuplicates)
                seen.add(normalized);
            resolved.add(new CustomCommandEntry(normalized, description));
        }

        return new ResolveResult(resolved, issues);
    }

    /** Convenience overload with default flags (check all). */
    public static ResolveResult resolveTelegramCustomCommands(
            List<CustomCommandInput> commands, Set<String> reservedCommands) {
        return resolveTelegramCustomCommands(commands, reservedCommands, true, true);
    }
}
