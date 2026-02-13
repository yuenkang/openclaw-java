package com.openclaw.agent.autoreply;

import java.util.*;

/**
 * Command registry types — command scopes, categories, argument
 * definitions, argument parsing modes, and chat command definitions.
 * Mirrors {@code auto-reply/commands-registry.types.ts}.
 */
public final class CommandsRegistryTypes {

    private CommandsRegistryTypes() {
    }

    /** Command scope: text-only, native-only, or both. */
    public enum CommandScope {
        TEXT, NATIVE, BOTH
    }

    /** Command category for grouping in help/status output. */
    public enum CommandCategory {
        SESSION, OPTIONS, STATUS, MANAGEMENT, MEDIA, TOOLS, DOCKS
    }

    /** Argument type. */
    public enum CommandArgType {
        STRING, NUMBER, BOOLEAN
    }

    /** A single argument choice. */
    public record CommandArgChoice(String value, String label) {
        public CommandArgChoice(String value) {
            this(value, value);
        }
    }

    /** A single command argument definition. */
    public record CommandArgDefinition(
            String name,
            String description,
            CommandArgType type,
            boolean required,
            List<CommandArgChoice> choices,
            boolean captureRemaining) {
    }

    /** Argument menu spec for interactive argument selection. */
    public record CommandArgMenuSpec(String arg, String title) {
    }

    /** Parsed command arguments. */
    public record CommandArgs(String raw, Map<String, Object> values) {
    }

    /** A registered chat command definition. */
    public record ChatCommandDefinition(
            String key,
            String nativeName,
            String description,
            List<String> textAliases,
            boolean acceptsArgs,
            List<CommandArgDefinition> args,
            String argsParsing,
            CommandArgMenuSpec argsMenu,
            CommandScope scope,
            CommandCategory category) {
    }

    /** A native command spec for registration with providers. */
    public record NativeCommandSpec(
            String name,
            String description,
            boolean acceptsArgs,
            List<CommandArgDefinition> args) {
    }

    /** Options for normalizing command body text. */
    public record CommandNormalizeOptions(String botUsername) {
    }

    /** Pre-compiled command detection state. */
    public record CommandDetectionState(
            Set<String> exact,
            java.util.regex.Pattern regex) {
    }
}
