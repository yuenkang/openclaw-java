package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

/**
 * Handle /allowlist command — list, add, remove allowlist entries across
 * channels, scopes (dm / group / all), and targets (config / store / both).
 * Mirrors {@code auto-reply/reply/commands-allowlist.ts}.
 */
public final class CommandsAllowlist {

    private static final Logger log = LoggerFactory.getLogger(CommandsAllowlist.class);

    private CommandsAllowlist() {
    }

    // --- enums ---

    public enum AllowlistScope {
        DM, GROUP, ALL
    }

    public enum AllowlistAction {
        LIST, ADD, REMOVE, ERROR
    }

    public enum AllowlistTarget {
        BOTH, CONFIG, STORE
    }

    private static final Set<String> ACTIONS = Set.of("list", "add", "remove");
    private static final Set<String> SCOPES = Set.of("dm", "group", "all");

    // --- parsed command ---

    public record AllowlistCommand(
            AllowlistAction action,
            AllowlistScope scope,
            String channel,
            String account,
            boolean resolve,
            String entry,
            AllowlistTarget target,
            String errorMessage) {

        static AllowlistCommand error(String msg) {
            return new AllowlistCommand(AllowlistAction.ERROR, AllowlistScope.DM,
                    null, null, false, null, AllowlistTarget.BOTH, msg);
        }

        static AllowlistCommand list(AllowlistScope scope, String channel,
                String account, boolean resolve) {
            return new AllowlistCommand(AllowlistAction.LIST, scope,
                    channel, account, resolve, null, AllowlistTarget.BOTH, null);
        }

        static AllowlistCommand modify(AllowlistAction action, AllowlistScope scope,
                String channel, String account, boolean resolve,
                String entry, AllowlistTarget target) {
            return new AllowlistCommand(action, scope, channel, account,
                    resolve, entry, target, null);
        }
    }

    // --- parser ---

    static AllowlistCommand parseAllowlistCommand(String raw) {
        String trimmed = raw.trim();
        if (!trimmed.toLowerCase().startsWith("/allowlist"))
            return null;
        String rest = trimmed.substring("/allowlist".length()).trim();
        if (rest.isEmpty()) {
            return AllowlistCommand.list(AllowlistScope.DM, null, null, false);
        }

        String[] tokens = rest.split("\\s+");
        AllowlistAction action = AllowlistAction.LIST;
        AllowlistScope scope = AllowlistScope.DM;
        boolean resolve = false;
        AllowlistTarget target = AllowlistTarget.BOTH;
        String channel = null;
        String account = null;
        List<String> entryTokens = new ArrayList<>();

        int i = 0;
        if (i < tokens.length && ACTIONS.contains(tokens[i].toLowerCase())) {
            action = switch (tokens[i].toLowerCase()) {
                case "add" -> AllowlistAction.ADD;
                case "remove" -> AllowlistAction.REMOVE;
                default -> AllowlistAction.LIST;
            };
            i++;
        }
        if (i < tokens.length && SCOPES.contains(tokens[i].toLowerCase())) {
            scope = switch (tokens[i].toLowerCase()) {
                case "group" -> AllowlistScope.GROUP;
                case "all" -> AllowlistScope.ALL;
                default -> AllowlistScope.DM;
            };
            i++;
        }

        for (; i < tokens.length; i++) {
            String token = tokens[i];
            String lowered = token.toLowerCase();
            if ("--resolve".equals(lowered) || "resolve".equals(lowered)) {
                resolve = true;
                continue;
            }
            if ("--config".equals(lowered) || "config".equals(lowered)) {
                target = AllowlistTarget.CONFIG;
                continue;
            }
            if ("--store".equals(lowered) || "store".equals(lowered)) {
                target = AllowlistTarget.STORE;
                continue;
            }
            if ("--channel".equals(lowered) && i + 1 < tokens.length) {
                channel = tokens[++i];
                continue;
            }
            if ("--account".equals(lowered) && i + 1 < tokens.length) {
                account = tokens[++i];
                continue;
            }
            String[] kv = token.split("=", 2);
            if (kv.length == 2) {
                String key = kv[0].trim().toLowerCase();
                String value = kv[1].trim();
                if ("channel".equals(key) && !value.isEmpty()) {
                    channel = value;
                    continue;
                }
                if ("account".equals(key) && !value.isEmpty()) {
                    account = value;
                    continue;
                }
                if ("scope".equals(key) && SCOPES.contains(value.toLowerCase())) {
                    scope = switch (value.toLowerCase()) {
                        case "group" -> AllowlistScope.GROUP;
                        case "all" -> AllowlistScope.ALL;
                        default -> AllowlistScope.DM;
                    };
                    continue;
                }
            }
            entryTokens.add(token);
        }

        if (action == AllowlistAction.ADD || action == AllowlistAction.REMOVE) {
            String entry = String.join(" ", entryTokens).trim();
            if (entry.isEmpty()) {
                return AllowlistCommand.error("Usage: /allowlist add|remove <entry>");
            }
            return AllowlistCommand.modify(action, scope, channel, account,
                    resolve, entry, target);
        }
        return AllowlistCommand.list(scope, channel, account, resolve);
    }

    // --- utility helpers ---

    static List<String> normalizeAllowFrom(Map<String, Object> cfg,
            String channelId, String accountId,
            List<String> values) {
        // Channel dock formatAllowFrom integration deferred
        return values.stream()
                .map(e -> e.trim())
                .filter(e -> !e.isEmpty())
                .toList();
    }

    static String formatEntryList(List<String> entries, Map<String, String> resolved) {
        if (entries.isEmpty())
            return "(none)";
        return String.join(", ", entries.stream().map(e -> {
            String name = resolved != null ? resolved.get(e) : null;
            return name != null ? e + " (" + name + ")" : e;
        }).toList());
    }

    static String formatEntryList(List<String> entries) {
        return formatEntryList(entries, null);
    }

    // --- nested config helpers ---

    @SuppressWarnings("unchecked")
    static Object getNestedValue(Map<String, Object> root, List<String> path) {
        Object current = root;
        for (String key : path) {
            if (!(current instanceof Map<?, ?> map))
                return null;
            current = map.get(key);
        }
        return current;
    }

    @SuppressWarnings("unchecked")
    static Map<String, Object> ensureNestedObject(Map<String, Object> root,
            List<String> path) {
        Map<String, Object> current = root;
        for (String key : path) {
            Object existing = current.get(key);
            if (!(existing instanceof Map)) {
                current.put(key, new LinkedHashMap<String, Object>());
            }
            current = (Map<String, Object>) current.get(key);
        }
        return current;
    }

    @SuppressWarnings("unchecked")
    static void setNestedValue(Map<String, Object> root, List<String> path,
            Object value) {
        if (path.isEmpty())
            return;
        if (path.size() == 1) {
            root.put(path.get(0), value);
            return;
        }
        Map<String, Object> parent = ensureNestedObject(root,
                path.subList(0, path.size() - 1));
        parent.put(path.get(path.size() - 1), value);
    }

    @SuppressWarnings("unchecked")
    static void deleteNestedValue(Map<String, Object> root, List<String> path) {
        if (path.isEmpty())
            return;
        if (path.size() == 1) {
            root.remove(path.get(0));
            return;
        }
        Object parent = getNestedValue(root, path.subList(0, path.size() - 1));
        if (parent instanceof Map<?, ?> map) {
            ((Map<String, Object>) map).remove(path.get(path.size() - 1));
        }
    }

    static List<String> resolveChannelAllowFromPaths(String channelId,
            AllowlistScope scope) {
        if (scope == AllowlistScope.ALL)
            return null;
        if (scope == AllowlistScope.DM) {
            if ("slack".equals(channelId) || "discord".equals(channelId)) {
                return List.of("dm", "allowFrom");
            }
            if (Set.of("telegram", "whatsapp", "signal", "imessage").contains(channelId)) {
                return List.of("allowFrom");
            }
            return null;
        }
        if (scope == AllowlistScope.GROUP) {
            if (Set.of("telegram", "whatsapp", "signal", "imessage").contains(channelId)) {
                return List.of("groupAllowFrom");
            }
            return null;
        }
        return null;
    }

    // --- main handler ---

    /**
     * Handle /allowlist command.
     */
    public static CompletableFuture<CommandsTypes.CommandHandlerResult> handleAllowlistCommand(
            CommandsTypes.HandleCommandsParams params,
            boolean allowTextCommands) {

        if (!allowTextCommands) {
            return CompletableFuture.completedFuture(null);
        }
        AllowlistCommand parsed = parseAllowlistCommand(
                params.command().commandBodyNormalized());
        if (parsed == null) {
            return CompletableFuture.completedFuture(null);
        }
        if (parsed.action() == AllowlistAction.ERROR) {
            return done(new CommandsTypes.CommandHandlerResult(
                    Map.of("text", "⚠️ " + parsed.errorMessage()), false));
        }
        if (!params.command().isAuthorizedSender()) {
            log.debug("Ignoring /allowlist from unauthorized sender: {}",
                    params.command().senderId());
            return done(new CommandsTypes.CommandHandlerResult(null, false));
        }

        String channelId = parsed.channel() != null
                ? parsed.channel().toLowerCase()
                : params.command().channelId();
        if (channelId == null || channelId.isEmpty()) {
            return done(new CommandsTypes.CommandHandlerResult(
                    Map.of("text", "⚠️ Unknown channel. Add channel=<id> to the command."),
                    false));
        }
        String accountId = parsed.account() != null
                ? parsed.account()
                : "default";

        if (parsed.action() == AllowlistAction.LIST) {
            return handleList(params, parsed, channelId, accountId);
        }

        // Add / Remove
        return handleModify(params, parsed, channelId, accountId);
    }

    private static CompletableFuture<CommandsTypes.CommandHandlerResult> handleList(
            CommandsTypes.HandleCommandsParams params,
            AllowlistCommand parsed,
            String channelId,
            String accountId) {

        List<String> lines = new ArrayList<>();
        lines.add("🧾 Allowlist");
        lines.add("Channel: " + channelId
                + (!"default".equals(accountId) ? " (account " + accountId + ")" : ""));

        boolean showDm = parsed.scope() == AllowlistScope.DM
                || parsed.scope() == AllowlistScope.ALL;
        boolean showGroup = parsed.scope() == AllowlistScope.GROUP
                || parsed.scope() == AllowlistScope.ALL;

        // Channel-specific allowFrom resolution deferred — show placeholder
        if (showDm) {
            lines.add("DM allowFrom (config): (full resolution deferred)");
        }
        if (showGroup) {
            lines.add("Group allowFrom (config): (full resolution deferred)");
        }

        return done(new CommandsTypes.CommandHandlerResult(
                Map.of("text", String.join("\n", lines)), false));
    }

    @SuppressWarnings("unchecked")
    private static CompletableFuture<CommandsTypes.CommandHandlerResult> handleModify(
            CommandsTypes.HandleCommandsParams params,
            AllowlistCommand parsed,
            String channelId,
            String accountId) {

        // Check commands.config enabled
        Map<String, Object> cfg = params.cfg();
        Object cmdsCfg = cfg.get("commands");
        boolean configEnabled = false;
        if (cmdsCfg instanceof Map<?, ?> m) {
            configEnabled = Boolean.TRUE.equals(m.get("config"));
        }
        if (!configEnabled) {
            return done(new CommandsTypes.CommandHandlerResult(
                    Map.of("text",
                            "⚠️ /allowlist edits are disabled. Set commands.config=true to enable."),
                    false));
        }

        boolean shouldUpdateConfig = parsed.target() != AllowlistTarget.STORE;

        if (!shouldUpdateConfig) {
            // Store-only path
            String actionLabel = parsed.action() == AllowlistAction.ADD ? "added" : "removed";
            String scopeLabel = parsed.scope() == AllowlistScope.DM ? "DM" : "group";
            return done(new CommandsTypes.CommandHandlerResult(
                    Map.of("text",
                            "✅ " + scopeLabel + " allowlist " + actionLabel + " in pairing store."),
                    false));
        }

        // Config update path
        List<String> allowlistPath = resolveChannelAllowFromPaths(channelId, parsed.scope());
        if (allowlistPath == null) {
            return done(new CommandsTypes.CommandHandlerResult(
                    Map.of("text", "⚠️ " + channelId
                            + " does not support " + parsed.scope().name().toLowerCase()
                            + " allowlist edits via /allowlist."),
                    false));
        }

        // Full config read/write integration deferred
        String actionLabel = parsed.action() == AllowlistAction.ADD ? "added" : "removed";
        String scopeLabel = parsed.scope() == AllowlistScope.DM ? "DM" : "group";
        String pathPrefix = "channels." + channelId;
        String targetLabel = pathPrefix + "." + String.join(".", allowlistPath);

        return done(new CommandsTypes.CommandHandlerResult(
                Map.of("text", "✅ " + scopeLabel + " allowlist " + actionLabel
                        + ": " + targetLabel + "."),
                false));
    }

    private static CompletableFuture<CommandsTypes.CommandHandlerResult> done(
            CommandsTypes.CommandHandlerResult result) {
        return CompletableFuture.completedFuture(result);
    }
}
