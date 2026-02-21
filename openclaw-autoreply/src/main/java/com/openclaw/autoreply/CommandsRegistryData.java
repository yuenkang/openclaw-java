package com.openclaw.autoreply;

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

                cmds.add(define("help", "help", "显示可用命令",
                                null, CommandsRegistryTypes.CommandCategory.STATUS,
                                List.of("/help"), false, null, null));
                cmds.add(define("commands", "commands", "列出所有斜杠命令",
                                null, CommandsRegistryTypes.CommandCategory.STATUS,
                                List.of("/commands"), false, null, null));
                cmds.add(define("skill", "skill", "按名称运行技能",
                                null, CommandsRegistryTypes.CommandCategory.TOOLS,
                                List.of("/skill"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("name", "Skill name",
                                                                CommandsRegistryTypes.CommandArgType.STRING, true, null,
                                                                false),
                                                new CommandsRegistryTypes.CommandArgDefinition("input", "Skill input",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, true)),
                                null));
                cmds.add(define("status", "status", "查看当前状态",
                                null, CommandsRegistryTypes.CommandCategory.STATUS,
                                List.of("/status"), false, null, null));
                cmds.add(define("allowlist", null, "管理白名单（列出/添加/移除）",
                                CommandsRegistryTypes.CommandScope.TEXT,
                                CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                                List.of("/allowlist"), true, null, null));
                cmds.add(define("approve", "approve", "审批或拒绝执行请求",
                                null, CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                                List.of("/approve"), true, null, null));
                cmds.add(define("context", "context", "查看当前上下文详情",
                                null, CommandsRegistryTypes.CommandCategory.STATUS,
                                List.of("/context"), true, null, null));
                cmds.add(define("tts", "tts", "语音合成 (TTS) 控制",
                                null, CommandsRegistryTypes.CommandCategory.MEDIA,
                                List.of("/tts"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("action", "TTS action",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice("on",
                                                                                "On"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "off", "Off"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "status", "Status"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "provider", "Provider"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "limit", "Limit"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "summary", "Summary"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "audio", "Audio"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "help", "Help")),
                                                                false),
                                                new CommandsRegistryTypes.CommandArgDefinition("value",
                                                                "Provider, limit, or text",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, true)),
                                null));
                cmds.add(define("whoami", "whoami", "显示你的会话信息",
                                null, CommandsRegistryTypes.CommandCategory.STATUS,
                                List.of("/whoami"), false, null, null));
                cmds.add(define("subagents", "subagents", "管理子 Agent（列出/停止/日志/详情）",
                                null, CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                                List.of("/subagents"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("action",
                                                                "list | stop | log | info | send",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice(
                                                                                "list"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "stop"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "log"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "info"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "send")),
                                                                false),
                                                new CommandsRegistryTypes.CommandArgDefinition("target",
                                                                "Run id, index, or session key",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, false),
                                                new CommandsRegistryTypes.CommandArgDefinition("value",
                                                                "Additional input (limit/message)",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, true)),
                                null));
                cmds.add(define("config", "config", "查看或设置运行时配置",
                                null, CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                                List.of("/config"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("action",
                                                                "show | get | set | unset",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice(
                                                                                "show"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "get"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "set"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "unset")),
                                                                false),
                                                new CommandsRegistryTypes.CommandArgDefinition("path", "Config path",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, false),
                                                new CommandsRegistryTypes.CommandArgDefinition("value", "Value for set",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, true)),
                                "none"));
                cmds.add(define("debug", "debug", "设置运行时 debug 覆盖",
                                null, CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                                List.of("/debug"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("action",
                                                                "show | reset | set | unset",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice(
                                                                                "show"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "reset"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "set"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "unset")),
                                                                false),
                                                new CommandsRegistryTypes.CommandArgDefinition("path", "Debug path",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, false),
                                                new CommandsRegistryTypes.CommandArgDefinition("value", "Value for set",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, true)),
                                "none"));
                cmds.add(define("usage", "usage", "查看用量统计与费用",
                                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                                List.of("/usage"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("mode",
                                                                "off, tokens, full, or cost",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice(
                                                                                "off"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "tokens"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "full"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "cost")),
                                                                false)),
                                null));
                cmds.add(define("stop", "stop", "停止当前运行",
                                null, CommandsRegistryTypes.CommandCategory.SESSION,
                                List.of("/stop"), false, null, null));
                cmds.add(define("restart", "restart", "重启 OpenClaw",
                                null, CommandsRegistryTypes.CommandCategory.TOOLS,
                                List.of("/restart"), false, null, null));
                cmds.add(define("activation", "activation", "设置群组激活模式",
                                null, CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                                List.of("/activation"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("mode",
                                                                "mention or always",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice(
                                                                                "mention"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "always")),
                                                                false)),
                                null));
                cmds.add(define("send", "send", "设置发送策略",
                                null, CommandsRegistryTypes.CommandCategory.MANAGEMENT,
                                List.of("/send"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("mode",
                                                                "on, off, or inherit",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice(
                                                                                "on"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "off"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "inherit")),
                                                                false)),
                                null));
                cmds.add(define("reset", "reset", "重置当前会话",
                                null, CommandsRegistryTypes.CommandCategory.SESSION,
                                List.of("/reset"), true, null, null));
                cmds.add(define("new", "new", "创建新会话",
                                null, CommandsRegistryTypes.CommandCategory.SESSION,
                                List.of("/new"), true, null, null));
                cmds.add(define("compact", null, "压缩会话上下文",
                                CommandsRegistryTypes.CommandScope.TEXT,
                                CommandsRegistryTypes.CommandCategory.SESSION,
                                List.of("/compact"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("instructions",
                                                                "Extra compaction instructions",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, true)),
                                null));
                cmds.add(define("think", "think", "设置思考深度",
                                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                                List.of("/think"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("level",
                                                                "off, minimal, low, medium, high, xhigh",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice(
                                                                                "off"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "minimal"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "low"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "medium"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "high"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "xhigh")),
                                                                false)),
                                null));
                cmds.add(define("verbose", "verbose", "切换详细模式",
                                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                                List.of("/verbose"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("mode", "on or off",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice(
                                                                                "on"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "off")),
                                                                false)),
                                null));
                cmds.add(define("reasoning", "reasoning", "切换推理过程显示",
                                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                                List.of("/reasoning"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("mode",
                                                                "on, off, or stream",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice(
                                                                                "on"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "off"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "stream")),
                                                                false)),
                                null));
                cmds.add(define("elevated", "elevated", "切换高级权限模式",
                                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                                List.of("/elevated"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("mode",
                                                                "on, off, ask, or full",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice(
                                                                                "on"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "off"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "ask"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "full")),
                                                                false)),
                                null));
                cmds.add(define("exec", "exec", "设置本会话的执行参数",
                                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                                List.of("/exec"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("options",
                                                                "host=... security=... ask=... node=...",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, false)),
                                "none"));
                cmds.add(define("model", "model", "查看或切换模型",
                                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                                List.of("/model"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("model",
                                                                "Model id (provider/model or id)",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, false)),
                                null));
                cmds.add(define("models", "models", "列出模型 Provider 或其可用模型",
                                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                                List.of("/models"), true, null, "none"));
                cmds.add(define("queue", "queue", "调整消息队列设置",
                                null, CommandsRegistryTypes.CommandCategory.OPTIONS,
                                List.of("/queue"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("mode", "queue mode",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice(
                                                                                "steer"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "interrupt"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "followup"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "collect"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "steer-backlog")),
                                                                false),
                                                new CommandsRegistryTypes.CommandArgDefinition("debounce",
                                                                "debounce duration (e.g. 500ms, 2s)",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, false),
                                                new CommandsRegistryTypes.CommandArgDefinition("cap", "queue cap",
                                                                CommandsRegistryTypes.CommandArgType.NUMBER, false,
                                                                null, false),
                                                new CommandsRegistryTypes.CommandArgDefinition("drop", "drop policy",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                List.of(new CommandsRegistryTypes.CommandArgChoice(
                                                                                "old"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "new"),
                                                                                new CommandsRegistryTypes.CommandArgChoice(
                                                                                                "summarize")),
                                                                false)),
                                "none"));
                cmds.add(define("bash", null, "执行主机 shell 命令",
                                CommandsRegistryTypes.CommandScope.TEXT,
                                CommandsRegistryTypes.CommandCategory.TOOLS,
                                List.of("/bash"), false,
                                List.of(
                                                new CommandsRegistryTypes.CommandArgDefinition("command",
                                                                "Shell command",
                                                                CommandsRegistryTypes.CommandArgType.STRING, false,
                                                                null, true)),
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
