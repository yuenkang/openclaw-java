package com.openclaw.agent.autoreply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.regex.*;

/**
 * Chat command data — define all built-in slash commands, dock commands,
 * and plugin command stubs. Command registry, alias registration, and
 * registry assertions.
 * Mirrors {@code auto-reply/commands-registry.data.ts}.
 */
public final class CommandsRegistryData {

    private static final Logger log = LoggerFactory.getLogger(CommandsRegistryData.class);

    private CommandsRegistryData() {
    }

    private static List<CommandsRegistryTypes.ChatCommandDefinition> cachedCommands = null;

    // --- builder ---

    private static CommandsRegistryTypes.ChatCommandDefinition define(
            String key, String nativeName, String description,
            CommandsRegistryTypes.CommandScope scope,
            CommandsRegistryTypes.CommandCategory category,
            List<String> textAliases,
            boolean acceptsArgs,
            List<CommandsRegistryTypes.CommandArgDefinition> args,
            String argsParsing) {

        scope = scope != null ? scope
                : (nativeName != null
                        ? (textAliases != null && !textAliases.isEmpty()
                                ? CommandsRegistryTypes.CommandScope.BOTH
                                : CommandsRegistryTypes.CommandScope.NATIVE)
                        : CommandsRegistryTypes.CommandScope.TEXT);
        String parsing = argsParsing != null ? argsParsing
                : (args != null && !args.isEmpty() ? "positional" : "none");
        boolean accepts = acceptsArgs || (args != null && !args.isEmpty());

        return new CommandsRegistryTypes.ChatCommandDefinition(
                key, nativeName, description,
                textAliases != null ? textAliases : List.of(),
                accepts, args, parsing, null, scope, category);
    }

    // --- built-in commands ---

    public static synchronized List<CommandsRegistryTypes.ChatCommandDefinition> getChatCommands() {
        if (cachedCommands != null)
            return cachedCommands;
        List<CommandsRegistryTypes.ChatCommandDefinition> cmds = buildChatCommands();
        cachedCommands = cmds;
        return cmds;
    }

    public static void invalidateCache() {
        cachedCommands = null;
    }

    private static List<CommandsRegistryTypes.ChatCommandDefinition> buildChatCommands() {
        List<CommandsRegistryTypes.ChatCommandDefinition> cmds = new ArrayList<>();

        cmds.add(define("help", "help", "Show available commands.",
                null, CommandsRegistryTypes.CommandCategory.STATUS,
                List.of("/help"), false, null, null));
        cmds.add(define("commands", "commands", "List all slash commands.",
                null, CommandsRegistryTypes.CommandCategory.STATUS,
                List.of("/commands"), false, null, null));
        cmds.add(define("skill", "skill", "Run a skill by name.",
                null, CommandsRegistryTypes.CommandCategory.TOOLS,
                List.of("/skill"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("name", "Skill name",
                                CommandsRegistryTypes.CommandArgType.STRING, true, null, false),
                        new CommandsRegistryTypes.CommandArgDefinition("input", "Skill input",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, true)),
                null));
        cmds.add(define("status", "status", "Show current status.",
                null, CommandsRegistryTypes.CommandCategory.STATUS,
                List.of("/status"), false, null, null));
        cmds.add(define("allowlist", null, "List/add/remove allowlist entries.",
                CommandsRegistryTypes.CommandScope.TEXT,
                CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                List.of("/allowlist"), true, null, null));
        cmds.add(define("approve", "approve", "Approve or deny exec requests.",
                null, CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                List.of("/approve"), true, null, null));
        cmds.add(define("context", "context", "Explain how context is built and used.",
                null, CommandsRegistryTypes.CommandCategory.STATUS,
                List.of("/context"), true, null, null));
        cmds.add(define("tts", "tts", "Control text-to-speech (TTS).",
                null, CommandsRegistryTypes.CommandCategory.MEDIA,
                List.of("/tts"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("action", "TTS action",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("on", "On"),
                                        new CommandsRegistryTypes.CommandArgChoice("off", "Off"),
                                        new CommandsRegistryTypes.CommandArgChoice("status", "Status"),
                                        new CommandsRegistryTypes.CommandArgChoice("provider", "Provider"),
                                        new CommandsRegistryTypes.CommandArgChoice("limit", "Limit"),
                                        new CommandsRegistryTypes.CommandArgChoice("summary", "Summary"),
                                        new CommandsRegistryTypes.CommandArgChoice("audio", "Audio"),
                                        new CommandsRegistryTypes.CommandArgChoice("help", "Help")),
                                false),
                        new CommandsRegistryTypes.CommandArgDefinition("value", "Provider, limit, or text",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, true)),
                null));
        cmds.add(define("whoami", "whoami", "Show your sender id.",
                null, CommandsRegistryTypes.CommandCategory.STATUS,
                List.of("/whoami"), false, null, null));
        cmds.add(define("subagents", "subagents", "List/stop/log/info subagent runs for this session.",
                null, CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                List.of("/subagents"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("action", "list | stop | log | info | send",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("list"),
                                        new CommandsRegistryTypes.CommandArgChoice("stop"),
                                        new CommandsRegistryTypes.CommandArgChoice("log"),
                                        new CommandsRegistryTypes.CommandArgChoice("info"),
                                        new CommandsRegistryTypes.CommandArgChoice("send")),
                                false),
                        new CommandsRegistryTypes.CommandArgDefinition("target", "Run id, index, or session key",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, false),
                        new CommandsRegistryTypes.CommandArgDefinition("value", "Additional input (limit/message)",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, true)),
                null));
        cmds.add(define("config", "config", "Show or set config values.",
                null, CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                List.of("/config"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("action", "show | get | set | unset",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("show"),
                                        new CommandsRegistryTypes.CommandArgChoice("get"),
                                        new CommandsRegistryTypes.CommandArgChoice("set"),
                                        new CommandsRegistryTypes.CommandArgChoice("unset")),
                                false),
                        new CommandsRegistryTypes.CommandArgDefinition("path", "Config path",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, false),
                        new CommandsRegistryTypes.CommandArgDefinition("value", "Value for set",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, true)),
                "none"));
        cmds.add(define("debug", "debug", "Set runtime debug overrides.",
                null, CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                List.of("/debug"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("action", "show | reset | set | unset",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("show"),
                                        new CommandsRegistryTypes.CommandArgChoice("reset"),
                                        new CommandsRegistryTypes.CommandArgChoice("set"),
                                        new CommandsRegistryTypes.CommandArgChoice("unset")),
                                false),
                        new CommandsRegistryTypes.CommandArgDefinition("path", "Debug path",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, false),
                        new CommandsRegistryTypes.CommandArgDefinition("value", "Value for set",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, true)),
                "none"));
        cmds.add(define("usage", "usage", "Usage footer or cost summary.",
                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                List.of("/usage"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("mode", "off, tokens, full, or cost",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("off"),
                                        new CommandsRegistryTypes.CommandArgChoice("tokens"),
                                        new CommandsRegistryTypes.CommandArgChoice("full"),
                                        new CommandsRegistryTypes.CommandArgChoice("cost")),
                                false)),
                null));
        cmds.add(define("stop", "stop", "Stop the current run.",
                null, CommandsRegistryTypes.CommandCategory.SESSION,
                List.of("/stop"), false, null, null));
        cmds.add(define("restart", "restart", "Restart OpenClaw.",
                null, CommandsRegistryTypes.CommandCategory.TOOLS,
                List.of("/restart"), false, null, null));
        cmds.add(define("activation", "activation", "Set group activation mode.",
                null, CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                List.of("/activation"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("mode", "mention or always",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("mention"),
                                        new CommandsRegistryTypes.CommandArgChoice("always")),
                                false)),
                null));
        cmds.add(define("send", "send", "Set send policy.",
                null, CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                List.of("/send"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("mode", "on, off, or inherit",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("on"),
                                        new CommandsRegistryTypes.CommandArgChoice("off"),
                                        new CommandsRegistryTypes.CommandArgChoice("inherit")),
                                false)),
                null));
        cmds.add(define("reset", "reset", "Reset the current session.",
                null, CommandsRegistryTypes.CommandCategory.SESSION,
                List.of("/reset"), true, null, null));
        cmds.add(define("new", "new", "Start a new session.",
                null, CommandsRegistryTypes.CommandCategory.SESSION,
                List.of("/new"), true, null, null));
        cmds.add(define("compact", null, "Compact the session context.",
                CommandsRegistryTypes.CommandScope.TEXT,
                CommandsRegistryTypes.CommandCategory.SESSION,
                List.of("/compact"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("instructions", "Extra compaction instructions",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, true)),
                null));
        cmds.add(define("think", "think", "Set thinking level.",
                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                List.of("/think"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("level",
                                "off, minimal, low, medium, high, xhigh",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("off"),
                                        new CommandsRegistryTypes.CommandArgChoice("minimal"),
                                        new CommandsRegistryTypes.CommandArgChoice("low"),
                                        new CommandsRegistryTypes.CommandArgChoice("medium"),
                                        new CommandsRegistryTypes.CommandArgChoice("high"),
                                        new CommandsRegistryTypes.CommandArgChoice("xhigh")),
                                false)),
                null));
        cmds.add(define("verbose", "verbose", "Toggle verbose mode.",
                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                List.of("/verbose"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("mode", "on or off",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("on"),
                                        new CommandsRegistryTypes.CommandArgChoice("off")),
                                false)),
                null));
        cmds.add(define("reasoning", "reasoning", "Toggle reasoning visibility.",
                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                List.of("/reasoning"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("mode", "on, off, or stream",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("on"),
                                        new CommandsRegistryTypes.CommandArgChoice("off"),
                                        new CommandsRegistryTypes.CommandArgChoice("stream")),
                                false)),
                null));
        cmds.add(define("elevated", "elevated", "Toggle elevated mode.",
                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                List.of("/elevated"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("mode", "on, off, ask, or full",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("on"),
                                        new CommandsRegistryTypes.CommandArgChoice("off"),
                                        new CommandsRegistryTypes.CommandArgChoice("ask"),
                                        new CommandsRegistryTypes.CommandArgChoice("full")),
                                false)),
                null));
        cmds.add(define("exec", "exec", "Set exec defaults for this session.",
                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                List.of("/exec"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("options",
                                "host=... security=... ask=... node=...",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, false)),
                "none"));
        cmds.add(define("model", "model", "Show or set the model.",
                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                List.of("/model"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("model", "Model id (provider/model or id)",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, false)),
                null));
        cmds.add(define("models", "models", "List model providers or provider models.",
                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                List.of("/models"), true, null, "none"));
        cmds.add(define("queue", "queue", "Adjust queue settings.",
                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                List.of("/queue"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("mode", "queue mode",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("steer"),
                                        new CommandsRegistryTypes.CommandArgChoice("interrupt"),
                                        new CommandsRegistryTypes.CommandArgChoice("followup"),
                                        new CommandsRegistryTypes.CommandArgChoice("collect"),
                                        new CommandsRegistryTypes.CommandArgChoice("steer-backlog")),
                                false),
                        new CommandsRegistryTypes.CommandArgDefinition("debounce", "debounce duration (e.g. 500ms, 2s)",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, false),
                        new CommandsRegistryTypes.CommandArgDefinition("cap", "queue cap",
                                CommandsRegistryTypes.CommandArgType.NUMBER, false, null, false),
                        new CommandsRegistryTypes.CommandArgDefinition("drop", "drop policy",
                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                List.of(new CommandsRegistryTypes.CommandArgChoice("old"),
                                        new CommandsRegistryTypes.CommandArgChoice("new"),
                                        new CommandsRegistryTypes.CommandArgChoice("summarize")),
                                false)),
                "none"));
        cmds.add(define("bash", null, "Run host shell commands (host-only).",
                CommandsRegistryTypes.CommandScope.TEXT,
                CommandsRegistryTypes.CommandCategory.TOOLS,
                List.of("/bash"), false,
                List.of(
                        new CommandsRegistryTypes.CommandArgDefinition("command", "Shell command",
                                CommandsRegistryTypes.CommandArgType.STRING, false, null, true)),
                null));

        // Register aliases
        registerAlias(cmds, "whoami", "/id");
        registerAlias(cmds, "think", "/thinking", "/t");
        registerAlias(cmds, "verbose", "/v");
        registerAlias(cmds, "reasoning", "/reason");
        registerAlias(cmds, "elevated", "/elev");

        return cmds;
    }

    static void registerAlias(List<CommandsRegistryTypes.ChatCommandDefinition> commands,
            String key, String... aliases) {
        for (int i = 0; i < commands.size(); i++) {
            CommandsRegistryTypes.ChatCommandDefinition cmd = commands.get(i);
            if (cmd.key().equals(key)) {
                Set<String> existing = new HashSet<>();
                cmd.textAliases().forEach(a -> existing.add(a.trim().toLowerCase()));
                List<String> newAliases = new ArrayList<>(cmd.textAliases());
                for (String alias : aliases) {
                    String trimmed = alias.trim();
                    if (trimmed.isEmpty() || existing.contains(trimmed.toLowerCase()))
                        continue;
                    existing.add(trimmed.toLowerCase());
                    newAliases.add(trimmed);
                }
                commands.set(i, new CommandsRegistryTypes.ChatCommandDefinition(
                        cmd.key(), cmd.nativeName(), cmd.description(),
                        newAliases, cmd.acceptsArgs(), cmd.args(), cmd.argsParsing(),
                        cmd.argsMenu(), cmd.scope(), cmd.category()));
                return;
            }
        }
    }

    /**
     * Return the set of surface IDs that support native commands.
     */
    public static Set<String> getNativeCommandSurfaces() {
        // Channel dock integration deferred
        return Set.of();
    }
}
