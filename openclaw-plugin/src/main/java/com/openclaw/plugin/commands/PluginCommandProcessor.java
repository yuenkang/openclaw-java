package com.openclaw.plugin.commands;

import com.openclaw.common.config.OpenClawConfig;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

/**
 * Plugin command processor — manages slash commands registered by plugins.
 * Commands bypass the LLM agent and are processed before built-in commands.
 * Corresponds to TypeScript's plugins/commands.ts.
 */
@Slf4j
public class PluginCommandProcessor {

    private static final int MAX_ARGS_LENGTH = 4096;

    private static final Set<String> RESERVED_COMMANDS = Set.of(
            "help", "commands", "status", "whoami", "context",
            "stop", "restart", "reset", "new", "compact",
            "config", "debug", "allowlist", "activation",
            "skill", "subagents", "model", "models", "queue",
            "send", "bash", "exec",
            "think", "verbose", "reasoning", "elevated",
            "usage");

    private final Map<String, RegisteredCommand> commands = new ConcurrentHashMap<>();
    private volatile boolean locked = false;

    // =========================================================================
    // Types
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RegisteredCommand {
        private String pluginId;
        private String name;
        private String description;
        private boolean acceptsArgs;
        private boolean requireAuth;
        private Function<CommandContext, CommandResult> handler;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CommandContext {
        private String senderId;
        private String channel;
        private boolean authorizedSender;
        private String args;
        private String commandBody;
        private OpenClawConfig config;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CommandResult {
        private String text;
        private boolean success;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RegistrationResult {
        private boolean ok;
        private String error;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MatchResult {
        private RegisteredCommand command;
        private String args;
    }

    // =========================================================================
    // Registration
    // =========================================================================

    /**
     * Register a plugin command. Returns error if name is invalid or reserved.
     */
    public RegistrationResult register(String pluginId, RegisteredCommand command) {
        if (locked) {
            return new RegistrationResult(false,
                    "Cannot register commands while processing is in progress");
        }

        String validationError = validateCommandName(command.getName());
        if (validationError != null) {
            return new RegistrationResult(false, validationError);
        }

        String key = "/" + command.getName().toLowerCase();
        if (commands.containsKey(key)) {
            RegisteredCommand existing = commands.get(key);
            return new RegistrationResult(false,
                    String.format("Command \"%s\" already registered by plugin \"%s\"",
                            command.getName(), existing.getPluginId()));
        }

        command.setPluginId(pluginId);
        commands.put(key, command);
        log.debug("Registered plugin command: {} (plugin: {})", key, pluginId);
        return new RegistrationResult(true, null);
    }

    // =========================================================================
    // Matching & execution
    // =========================================================================

    /**
     * Check if a command body matches a registered plugin command.
     */
    public MatchResult match(String commandBody) {
        if (commandBody == null)
            return null;
        String trimmed = commandBody.trim();
        if (!trimmed.startsWith("/"))
            return null;

        int spaceIdx = trimmed.indexOf(' ');
        String commandName = spaceIdx == -1 ? trimmed : trimmed.substring(0, spaceIdx);
        String args = spaceIdx == -1 ? null : trimmed.substring(spaceIdx + 1).trim();

        String key = commandName.toLowerCase();
        RegisteredCommand command = commands.get(key);
        if (command == null)
            return null;

        // If command doesn't accept args but args were provided, don't match
        if (args != null && !args.isEmpty() && !command.isAcceptsArgs())
            return null;

        return MatchResult.builder()
                .command(command)
                .args(args)
                .build();
    }

    /**
     * Execute a matched plugin command.
     */
    public CommandResult execute(MatchResult match, String senderId, String channel,
            boolean isAuthorized, String commandBody,
            OpenClawConfig config) {
        RegisteredCommand command = match.getCommand();

        // Check authorization
        boolean requireAuth = command.isRequireAuth();
        if (requireAuth && !isAuthorized) {
            log.debug("Plugin command /{} blocked: unauthorized sender {}",
                    command.getName(), senderId);
            return CommandResult.builder()
                    .text("⚠️ This command requires authorization.")
                    .success(false)
                    .build();
        }

        String sanitizedArgs = sanitizeArgs(match.getArgs());

        CommandContext ctx = CommandContext.builder()
                .senderId(senderId)
                .channel(channel)
                .authorizedSender(isAuthorized)
                .args(sanitizedArgs)
                .commandBody(commandBody)
                .config(config)
                .build();

        locked = true;
        try {
            if (command.getHandler() != null) {
                CommandResult result = command.getHandler().apply(ctx);
                log.debug("Plugin command /{} executed successfully", command.getName());
                return result;
            }
            return CommandResult.builder()
                    .text("Command handler not available.")
                    .success(false)
                    .build();
        } catch (Exception e) {
            log.error("Plugin command /{} error: {}", command.getName(), e.getMessage());
            return CommandResult.builder()
                    .text("⚠️ Command failed. Please try again later.")
                    .success(false)
                    .build();
        } finally {
            locked = false;
        }
    }

    // =========================================================================
    // Queries
    // =========================================================================

    public List<RegisteredCommand> listCommands() {
        return List.copyOf(commands.values());
    }

    public void clear() {
        commands.clear();
    }

    public void clearForPlugin(String pluginId) {
        commands.entrySet().removeIf(e -> pluginId.equals(e.getValue().getPluginId()));
    }

    // =========================================================================
    // Validation & sanitization
    // =========================================================================

    public static String validateCommandName(String name) {
        if (name == null || name.isBlank()) {
            return "Command name cannot be empty";
        }
        String trimmed = name.trim().toLowerCase();
        if (!trimmed.matches("^[a-z][a-z0-9_-]*$")) {
            return "Command name must start with a letter and contain only letters, numbers, hyphens, and underscores";
        }
        if (RESERVED_COMMANDS.contains(trimmed)) {
            return String.format("Command name \"%s\" is reserved by a built-in command", trimmed);
        }
        return null;
    }

    static String sanitizeArgs(String args) {
        if (args == null)
            return null;
        if (args.length() > MAX_ARGS_LENGTH) {
            args = args.substring(0, MAX_ARGS_LENGTH);
        }
        StringBuilder sb = new StringBuilder(args.length());
        for (char c : args.toCharArray()) {
            boolean isControl = (c <= 0x1f && c != '\t' && c != '\n') || c == 0x7f;
            if (!isControl)
                sb.append(c);
        }
        return sb.toString();
    }
}
