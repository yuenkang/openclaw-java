package com.openclaw.autoreply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.regex.*;

/**
 * Command registry — text alias resolution, command detection,
 * argument parsing/serialization, native command spec listing,
 * and command normalization.
 * Mirrors {@code auto-reply/commands-registry.ts}.
 */
public final class CommandsRegistry {

    private static final Logger log = LoggerFactory.getLogger(CommandsRegistry.class);

    private CommandsRegistry() {
    }

    // --- text alias cache ---

    private record TextAliasSpec(String key, String canonical, boolean acceptsArgs) {
    }

    private static Map<String, TextAliasSpec> cachedTextAliasMap = null;
    private static List<CommandsRegistryTypes.ChatCommandDefinition> cachedTextAliasCommands = null;

    private static synchronized Map<String, TextAliasSpec> getTextAliasMap() {
        List<CommandsRegistryTypes.ChatCommandDefinition> commands = CommandsRegistryData.getChatCommands();
        if (cachedTextAliasMap != null && cachedTextAliasCommands == commands)
            return cachedTextAliasMap;

        Map<String, TextAliasSpec> map = new LinkedHashMap<>();
        for (var cmd : commands) {
            String canonical = !cmd.textAliases().isEmpty()
                    ? cmd.textAliases().get(0).trim()
                    : "/" + cmd.key();
            boolean accepts = cmd.acceptsArgs();
            for (String alias : cmd.textAliases()) {
                String normalized = alias.trim().toLowerCase();
                if (normalized.isEmpty())
                    continue;
                map.putIfAbsent(normalized, new TextAliasSpec(cmd.key(), canonical, accepts));
            }
        }
        cachedTextAliasMap = map;
        cachedTextAliasCommands = commands;
        return map;
    }

    // --- public API ---

    public static List<CommandsRegistryTypes.ChatCommandDefinition> listChatCommands(
            List<SkillCommands.SkillCommandSpec> skillCommands) {
        List<CommandsRegistryTypes.ChatCommandDefinition> base = new ArrayList<>(
                CommandsRegistryData.getChatCommands());
        if (skillCommands != null && !skillCommands.isEmpty()) {
            base.addAll(buildSkillCommandDefinitions(skillCommands));
        }
        return base;
    }

    public static boolean isCommandEnabled(Map<String, Object> cfg, String commandKey) {
        @SuppressWarnings("unchecked")
        Map<String, Object> commands = cfg.get("commands") instanceof Map<?, ?> m
                ? (Map<String, Object>) m
                : null;
        if (commands == null)
            return true;
        return switch (commandKey) {
            case "config" -> Boolean.TRUE.equals(commands.get("config"));
            case "debug" -> Boolean.TRUE.equals(commands.get("debug"));
            case "bash" -> Boolean.TRUE.equals(commands.get("bash"));
            default -> true;
        };
    }

    public static List<CommandsRegistryTypes.ChatCommandDefinition> listChatCommandsForConfig(
            Map<String, Object> cfg,
            List<SkillCommands.SkillCommandSpec> skillCommands) {
        List<CommandsRegistryTypes.ChatCommandDefinition> base = CommandsRegistryData.getChatCommands().stream()
                .filter(cmd -> isCommandEnabled(cfg, cmd.key()))
                .collect(java.util.stream.Collectors.toList());
        if (skillCommands != null && !skillCommands.isEmpty()) {
            base.addAll(buildSkillCommandDefinitions(skillCommands));
        }
        return base;
    }

    public static List<CommandsRegistryTypes.NativeCommandSpec> listNativeCommandSpecs(
            List<SkillCommands.SkillCommandSpec> skillCommands, String provider) {
        return listChatCommands(skillCommands).stream()
                .filter(cmd -> cmd.scope() != CommandsRegistryTypes.CommandScope.TEXT && cmd.nativeName() != null)
                .map(cmd -> new CommandsRegistryTypes.NativeCommandSpec(
                        resolveNativeName(cmd, provider),
                        cmd.description(), cmd.acceptsArgs(), cmd.args()))
                .toList();
    }

    public static CommandsRegistryTypes.ChatCommandDefinition findCommandByNativeName(
            String name, String provider) {
        String normalized = name.trim().toLowerCase();
        return CommandsRegistryData.getChatCommands().stream()
                .filter(cmd -> cmd.scope() != CommandsRegistryTypes.CommandScope.TEXT
                        && resolveNativeName(cmd, provider).toLowerCase().equals(normalized))
                .findFirst().orElse(null);
    }

    public static String buildCommandText(String commandName, String args) {
        String trimmed = args != null ? args.trim() : "";
        return trimmed.isEmpty() ? "/" + commandName : "/" + commandName + " " + trimmed;
    }

    // --- arg parsing ---

    public static CommandsRegistryTypes.CommandArgs parseCommandArgs(
            CommandsRegistryTypes.ChatCommandDefinition command, String raw) {
        String trimmed = raw != null ? raw.trim() : "";
        if (trimmed.isEmpty())
            return null;
        if (command.args() == null || command.args().isEmpty() || "none".equals(command.argsParsing())) {
            return new CommandsRegistryTypes.CommandArgs(trimmed, null);
        }
        return new CommandsRegistryTypes.CommandArgs(trimmed, parsePositionalArgs(command.args(), trimmed));
    }

    public static String serializeCommandArgs(
            CommandsRegistryTypes.ChatCommandDefinition command,
            CommandsRegistryTypes.CommandArgs args) {
        if (args == null)
            return null;
        String raw = args.raw() != null ? args.raw().trim() : "";
        if (!raw.isEmpty())
            return raw;
        if (args.values() == null || command.args() == null)
            return null;
        return formatPositionalArgs(command.args(), args.values());
    }

    // --- normalizer ---

    public static String normalizeCommandBody(String raw, CommandsRegistryTypes.CommandNormalizeOptions options) {
        String botUsername = options != null ? options.botUsername() : null;
        return CommandNormalizer.normalizeCommandBody(raw, botUsername);
    }

    public static boolean isCommandMessage(String raw) {
        String trimmed = normalizeCommandBody(raw, null);
        return trimmed.startsWith("/");
    }

    /** Resolve a text command from a raw message string. */
    public static ResolvedTextCommand resolveTextCommand(String raw, Map<String, Object> cfg) {
        String trimmed = normalizeCommandBody(raw, null).trim();
        String alias = maybeResolveTextAlias(trimmed, cfg);
        if (alias == null)
            return null;
        TextAliasSpec spec = getTextAliasMap().get(alias);
        if (spec == null)
            return null;
        var cmd = CommandsRegistryData.getChatCommands().stream()
                .filter(c -> c.key().equals(spec.key)).findFirst().orElse(null);
        if (cmd == null)
            return null;
        if (!spec.acceptsArgs)
            return new ResolvedTextCommand(cmd, null);
        String args = trimmed.substring(alias.length()).trim();
        return new ResolvedTextCommand(cmd, args.isEmpty() ? null : args);
    }

    /** Result of resolving a text command. */
    public record ResolvedTextCommand(
            CommandsRegistryTypes.ChatCommandDefinition command,
            String args) {
    }

    public static boolean isNativeCommandSurface(String surface) {
        if (surface == null || surface.isBlank())
            return false;
        return CommandsRegistryData.getNativeCommandSurfaces().contains(surface.toLowerCase());
    }

    public static boolean shouldHandleTextCommands(Map<String, Object> cfg,
            String surface, String commandSource) {
        if ("native".equals(commandSource))
            return true;
        @SuppressWarnings("unchecked")
        Map<String, Object> commands = cfg.get("commands") instanceof Map<?, ?> m
                ? (Map<String, Object>) m
                : null;
        if (commands == null || !Boolean.FALSE.equals(commands.get("text")))
            return true;
        return !isNativeCommandSurface(surface);
    }

    // --- private ---

    private static String maybeResolveTextAlias(String raw, Map<String, Object> cfg) {
        String trimmed = normalizeCommandBody(raw, null).trim();
        if (!trimmed.startsWith("/"))
            return null;
        String normalized = trimmed.toLowerCase();
        Map<String, TextAliasSpec> map = getTextAliasMap();
        if (map.containsKey(normalized))
            return normalized;

        Matcher m = Pattern.compile("^/([^\\s:]+)(?:\\s|$)").matcher(normalized);
        if (!m.find())
            return null;
        String tokenKey = "/" + m.group(1);
        return map.containsKey(tokenKey) ? tokenKey : null;
    }

    private static Map<String, Object> parsePositionalArgs(
            List<CommandsRegistryTypes.CommandArgDefinition> defs, String raw) {
        Map<String, Object> values = new LinkedHashMap<>();
        String trimmed = raw.trim();
        if (trimmed.isEmpty())
            return values;
        String[] tokens = trimmed.split("\\s+");
        int idx = 0;
        for (var def : defs) {
            if (idx >= tokens.length)
                break;
            if (def.captureRemaining()) {
                values.put(def.name(), String.join(" ", Arrays.copyOfRange(tokens, idx, tokens.length)));
                idx = tokens.length;
                break;
            }
            values.put(def.name(), tokens[idx]);
            idx++;
        }
        return values;
    }

    private static String formatPositionalArgs(
            List<CommandsRegistryTypes.CommandArgDefinition> defs, Map<String, Object> values) {
        List<String> parts = new ArrayList<>();
        for (var def : defs) {
            Object val = values.get(def.name());
            if (val == null)
                continue;
            String rendered = val.toString().trim();
            if (rendered.isEmpty())
                continue;
            parts.add(rendered);
            if (def.captureRemaining())
                break;
        }
        return parts.isEmpty() ? null : String.join(" ", parts);
    }

    private static String resolveNativeName(CommandsRegistryTypes.ChatCommandDefinition cmd, String provider) {
        if (cmd.nativeName() == null)
            return cmd.key();
        if (provider != null && "discord".equalsIgnoreCase(provider) && "tts".equals(cmd.key())) {
            return "voice";
        }
        return cmd.nativeName();
    }

    private static List<CommandsRegistryTypes.ChatCommandDefinition> buildSkillCommandDefinitions(
            List<SkillCommands.SkillCommandSpec> skills) {
        return skills.stream()
                .map(spec -> new CommandsRegistryTypes.ChatCommandDefinition(
                        "skill:" + spec.skillName(), spec.name(), spec.description(),
                        List.of("/" + spec.name()), true, null, "none",
                        null, CommandsRegistryTypes.CommandScope.BOTH, null))
                .toList();
    }
}
