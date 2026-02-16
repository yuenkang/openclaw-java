package com.openclaw.app.commands;

/**
 * Functional interface for command handlers.
 * Mirrors TypeScript's {@code CommandHandler} type from
 * {@code commands-types.ts}.
 */
@FunctionalInterface
public interface CommandHandler {
    /**
     * Handle a command.
     *
     * @param args the command arguments (text after the command name), may be
     *             empty
     * @param ctx  the command context (session, sender, config, authorization)
     * @return CommandResult with reply text and optional buttons,
     *         or null if the command is not handled by this handler
     */
    CommandResult handle(String args, CommandContext ctx);
}
