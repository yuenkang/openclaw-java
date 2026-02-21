package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

/**
 * Core command dispatcher — routes to individual command handlers.
 * Mirrors {@code auto-reply/reply/commands-core.ts}.
 */
public final class CommandsCore {

    private static final Logger log = LoggerFactory.getLogger(CommandsCore.class);

    private CommandsCore() {
    }

    /**
     * Functional interface for a command handler.
     */
    @FunctionalInterface
    public interface CommandHandler {
        /**
         * Handle a command.
         *
         * @return result or null if not handled
         */
        CommandsTypes.CommandHandlerResult handle(CommandsTypes.HandleCommandsParams params,
                boolean allowTextCommands);
    }

    /** Lazily-initialized handler list. */
    private static List<CommandHandler> handlers;

    private static List<CommandHandler> getHandlers() {
        if (handlers == null) {
            handlers = List.of(
                    // Plugin commands first
                    (params, allow) -> null, // plugin placeholder
                    // Built-in commands
                    (params, allow) -> null, // bash placeholder
                    (params, allow) -> null, // activation placeholder
                    (params, allow) -> null, // sendpolicy placeholder
                    (params, allow) -> null, // usage placeholder
                    (params, allow) -> null, // restart placeholder
                    (params, allow) -> null, // tts placeholder
                    (params, allow) -> null, // help placeholder
                    (params, allow) -> null, // commands list placeholder
                    (params, allow) -> null, // status placeholder
                    (params, allow) -> null, // allowlist placeholder
                    (params, allow) -> null, // approve placeholder
                    (params, allow) -> null, // context placeholder
                    (params, allow) -> null, // whoami placeholder
                    (params, allow) -> null, // subagents placeholder
                    (params, allow) -> null, // config placeholder
                    (params, allow) -> null, // debug placeholder
                    (params, allow) -> null, // models placeholder
                    (params, allow) -> null, // stop placeholder
                    (params, allow) -> null, // compact placeholder
                    (params, allow) -> null // abort trigger placeholder
            );
        }
        return handlers;
    }

    /**
     * Process all command handlers for a given incoming command.
     *
     * @return handler result with shouldContinue and optional reply
     */
    public static CommandsTypes.CommandHandlerResult handleCommands(
            CommandsTypes.HandleCommandsParams params) {

        CommandsTypes.CommandContext command = params.command();
        // Check /new or /reset
        String normalized = command != null ? command.commandBodyNormalized() : null;
        boolean resetRequested = normalized != null
                && normalized.matches("^/(new|reset)(\\s|$).*");

        if (resetRequested && command != null && !command.isAuthorizedSender()) {
            log.debug("Ignoring /reset from unauthorized sender: {}",
                    command.senderId() != null ? command.senderId() : "<unknown>");
            return new CommandsTypes.CommandHandlerResult(null, false);
        }

        // Iterate handlers
        boolean allowTextCommands = true; // simplified — full check deferred
        for (CommandHandler handler : getHandlers()) {
            CommandsTypes.CommandHandlerResult result = handler.handle(params, allowTextCommands);
            if (result != null)
                return result;
        }

        // No handler matched — continue to LLM
        return new CommandsTypes.CommandHandlerResult(null, true);
    }
}
