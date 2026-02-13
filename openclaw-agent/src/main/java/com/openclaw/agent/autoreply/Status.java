package com.openclaw.agent.autoreply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Status and help message formatting — build structured status lines
 * (model, context, tokens, runtime, session, queue, voice, media, etc.)
 * and help/commands listing with category grouping and pagination.
 * Mirrors {@code auto-reply/status.ts}.
 */
public final class Status {

    private static final Logger log = LoggerFactory.getLogger(Status.class);
    private static final String DEFAULT_PROVIDER = "openai";
    private static final String DEFAULT_MODEL = "gpt-4o";
    private static final int DEFAULT_CONTEXT_TOKENS = 128_000;
    private static final int COMMANDS_PER_PAGE = 8;
    private static final String VERSION = "dev-java";

    private Status() {
    }

    // --- token formatting ---

    public static String formatTokenCount(long tokens) {
        if (tokens < 1_000)
            return String.valueOf(tokens);
        if (tokens < 10_000)
            return String.format("%.1fk", tokens / 1_000.0);
        if (tokens < 1_000_000)
            return Math.round(tokens / 1_000.0) + "k";
        return String.format("%.1fM", tokens / 1_000_000.0);
    }

    public static String formatContextUsageShort(Long total, Long contextTokens) {
        return "Context " + formatTokens(total, contextTokens);
    }

    private static String formatTokens(Long total, Long contextTokens) {
        Long ctx = contextTokens;
        if (total == null) {
            String ctxLabel = ctx != null ? formatTokenCount(ctx) : "?";
            return "?/" + ctxLabel;
        }
        String totalLabel = formatTokenCount(total);
        String ctxLabel = ctx != null ? formatTokenCount(ctx) : "?";
        Long pct = ctx != null && ctx > 0 ? Math.min(999, Math.round((double) total / ctx * 100)) : null;
        return totalLabel + "/" + ctxLabel + (pct != null ? " (" + pct + "%)" : "");
    }

    // --- age formatting ---

    private static String formatAge(Long ms) {
        if (ms == null || ms < 0)
            return "unknown";
        long minutes = Math.round(ms / 60_000.0);
        if (minutes < 1)
            return "just now";
        if (minutes < 60)
            return minutes + "m ago";
        long hours = Math.round(minutes / 60.0);
        if (hours < 48)
            return hours + "h ago";
        long days = Math.round(hours / 24.0);
        return days + "d ago";
    }

    // --- queue formatting ---

    private static String formatQueueDetails(Map<String, Object> queue) {
        if (queue == null)
            return "";
        Object depthRaw = queue.get("depth");
        String depth = depthRaw instanceof Number n ? "depth " + n.intValue() : null;
        Object showDetails = queue.get("showDetails");
        if (!Boolean.TRUE.equals(showDetails)) {
            return depth != null ? " (" + depth + ")" : "";
        }
        List<String> parts = new ArrayList<>();
        if (depth != null)
            parts.add(depth);
        Object debounceMs = queue.get("debounceMs");
        if (debounceMs instanceof Number n) {
            int ms = Math.max(0, n.intValue());
            String label = ms >= 1000
                    ? (ms % 1000 == 0 ? ms / 1000 + "s" : String.format("%.1fs", ms / 1000.0))
                    : ms + "ms";
            parts.add("debounce " + label);
        }
        Object cap = queue.get("cap");
        if (cap instanceof Number n)
            parts.add("cap " + n.intValue());
        Object dropPolicy = queue.get("dropPolicy");
        if (dropPolicy instanceof String s && !s.isBlank())
            parts.add("drop " + s);
        return parts.isEmpty() ? "" : " (" + String.join(" · ", parts) + ")";
    }

    // --- usage pair ---

    private static String formatUsagePair(Long input, Long output) {
        if (input == null && output == null)
            return null;
        String inputLabel = input != null ? formatTokenCount(input) : "?";
        String outputLabel = output != null ? formatTokenCount(output) : "?";
        return "🧮 Tokens: " + inputLabel + " in / " + outputLabel + " out";
    }

    // --- StatusArgs ---

    /** Arguments for building a status message. */
    public record StatusArgs(
            Map<String, Object> config,
            Map<String, Object> agent,
            Map<String, Object> sessionEntry,
            String sessionKey,
            String sessionScope,
            String groupActivation,
            String resolvedThink,
            String resolvedVerbose,
            String resolvedReasoning,
            String resolvedElevated,
            String modelAuth,
            String usageLine,
            String timeLine,
            Map<String, Object> queue,
            List<Map<String, Object>> mediaDecisions,
            String subagentsLine,
            boolean includeTranscriptUsage,
            Long now) {
    }

    /**
     * Build a full status message for /status command.
     */
    public static String buildStatusMessage(StatusArgs args) {
        long now = args.now() != null ? args.now() : System.currentTimeMillis();
        Map<String, Object> entry = args.sessionEntry();

        String provider = resolve(entry, "providerOverride",
                resolve(args.agent(), "provider", DEFAULT_PROVIDER));
        String model = resolve(entry, "modelOverride",
                resolve(args.agent(), "model", DEFAULT_MODEL));
        long contextTokens = resolveLong(entry, "contextTokens",
                resolveLong(args.agent(), "contextTokens", DEFAULT_CONTEXT_TOKENS));

        Long inputTokens = resolveNullLong(entry, "inputTokens");
        Long outputTokens = resolveNullLong(entry, "outputTokens");
        Long totalTokens = resolveNullLong(entry, "totalTokens");
        if (totalTokens == null && inputTokens != null && outputTokens != null) {
            totalTokens = inputTokens + outputTokens;
        }

        String thinkLevel = firstNonNull(args.resolvedThink(),
                resolve(args.agent(), "thinkingDefault", "off"));
        String verboseLevel = firstNonNull(args.resolvedVerbose(),
                resolve(args.agent(), "verboseDefault", "off"));
        String reasoningLevel = firstNonNull(args.resolvedReasoning(), "off");
        String elevatedLevel = firstNonNull(args.resolvedElevated(),
                resolve(entry, "elevatedLevel",
                        resolve(args.agent(), "elevatedDefault", "on")));

        String runtime = "direct";
        Long updatedAt = resolveNullLong(entry, "updatedAt");
        String sessionLine = "Session: " + (args.sessionKey() != null ? args.sessionKey() : "unknown")
                + " • " + (updatedAt != null ? "updated " + formatAge(now - updatedAt) : "no activity");

        boolean isGroupSession = isGroup(entry, args.sessionKey());
        String groupActivation = isGroupSession
                ? firstNonNull(args.groupActivation(),
                        resolve(entry, "groupActivation", "mention"))
                : null;

        String contextLine = "Context: " + formatTokens(totalTokens, contextTokens)
                + " · 🧹 Compactions: " + resolveLong(entry, "compactionCount", 0);

        String queueMode = args.queue() != null && args.queue().get("mode") instanceof String s
                ? s
                : "unknown";
        String queueDetails = formatQueueDetails(args.queue());

        String verboseLabel = "full".equals(verboseLevel) ? "verbose:full"
                : "on".equals(verboseLevel) ? "verbose" : null;
        String elevatedLabel = elevatedLevel != null && !"off".equals(elevatedLevel)
                ? ("on".equals(elevatedLevel) ? "elevated" : "elevated:" + elevatedLevel)
                : null;

        List<String> optionParts = new ArrayList<>();
        optionParts.add("Runtime: " + runtime);
        optionParts.add("Think: " + thinkLevel);
        if (verboseLabel != null)
            optionParts.add(verboseLabel);
        if (!"off".equals(reasoningLevel))
            optionParts.add("Reasoning: " + reasoningLevel);
        if (elevatedLabel != null)
            optionParts.add(elevatedLabel);
        String optionsLine = String.join(" · ", optionParts);

        List<String> activationParts = new ArrayList<>();
        if (groupActivation != null)
            activationParts.add("👥 Activation: " + groupActivation);
        activationParts.add("🪢 Queue: " + queueMode + queueDetails);
        String activationLine = String.join(" · ", activationParts);

        String modelLabel = model != null ? provider + "/" + model : "unknown";
        String authLabel = args.modelAuth() != null ? " · 🔑 " + args.modelAuth() : "";
        String modelLine = "🧠 Model: " + modelLabel + authLabel;
        String versionLine = "🦞 OpenClaw " + VERSION;
        String usagePair = formatUsagePair(inputTokens, outputTokens);

        List<String> lines = new ArrayList<>();
        lines.add(versionLine);
        if (args.timeLine() != null)
            lines.add(args.timeLine());
        lines.add(modelLine);
        if (usagePair != null)
            lines.add(usagePair);
        lines.add("📚 " + contextLine);
        if (args.usageLine() != null)
            lines.add(args.usageLine());
        lines.add("🧵 " + sessionLine);
        if (args.subagentsLine() != null)
            lines.add(args.subagentsLine());
        lines.add("⚙️ " + optionsLine);
        lines.add(activationLine);

        return String.join("\n", lines);
    }

    // --- help ---

    public static String buildHelpMessage(Map<String, Object> cfg) {
        List<String> lines = new ArrayList<>();
        lines.add("ℹ️ Help");
        lines.add("");
        lines.add("Session");
        lines.add("  /new  |  /reset  |  /compact [instructions]  |  /stop");
        lines.add("");

        List<String> opts = new ArrayList<>(List.of("/think <level>", "/model <id>", "/verbose on|off"));
        @SuppressWarnings("unchecked")
        Map<String, Object> commands = cfg != null && cfg.get("commands") instanceof Map<?, ?> m
                ? (Map<String, Object>) m
                : null;
        if (commands != null && Boolean.TRUE.equals(commands.get("config")))
            opts.add("/config");
        if (commands != null && Boolean.TRUE.equals(commands.get("debug")))
            opts.add("/debug");
        lines.add("Options");
        lines.add("  " + String.join("  |  ", opts));
        lines.add("");
        lines.add("Status");
        lines.add("  /status  |  /whoami  |  /context");
        lines.add("");
        lines.add("Skills");
        lines.add("  /skill <name> [input]");
        lines.add("");
        lines.add("More: /commands for full list");

        return String.join("\n", lines);
    }

    // --- commands listing ---

    /** Result of paginated commands listing. */
    public record CommandsMessageResult(
            String text,
            int totalPages,
            int currentPage,
            boolean hasNext,
            boolean hasPrev) {
    }

    public static String buildCommandsMessage(
            Map<String, Object> cfg,
            List<SkillCommands.SkillCommandSpec> skillCommands,
            Integer page, String surface) {
        return buildCommandsMessagePaginated(cfg, skillCommands, page, surface).text();
    }

    public static CommandsMessageResult buildCommandsMessagePaginated(
            Map<String, Object> cfg,
            List<SkillCommands.SkillCommandSpec> skillCommands,
            Integer page, String surface) {
        int pageNum = Math.max(1, page != null ? page : 1);
        boolean isTelegram = "telegram".equalsIgnoreCase(surface);

        List<CommandsRegistryTypes.ChatCommandDefinition> cmds = cfg != null
                ? CommandsRegistry.listChatCommandsForConfig(cfg, skillCommands)
                : CommandsRegistry.listChatCommands(skillCommands);

        List<String[]> items = buildCommandItems(cmds);

        if (!isTelegram) {
            List<String> lines = new ArrayList<>();
            lines.add("ℹ️ Slash commands");
            lines.add("");
            lines.addAll(formatCommandList(items));
            return new CommandsMessageResult(
                    String.join("\n", lines).trim(), 1, 1, false, false);
        }

        int total = items.size();
        int totalPages = Math.max(1, (int) Math.ceil((double) total / COMMANDS_PER_PAGE));
        int currentPage = Math.min(pageNum, totalPages);
        int start = (currentPage - 1) * COMMANDS_PER_PAGE;
        int end = Math.min(start + COMMANDS_PER_PAGE, total);
        List<String[]> pageItems = items.subList(start, end);

        List<String> lines = new ArrayList<>();
        lines.add("ℹ️ Commands (" + currentPage + "/" + totalPages + ")");
        lines.add("");
        lines.addAll(formatCommandList(pageItems));

        return new CommandsMessageResult(
                String.join("\n", lines).trim(),
                totalPages, currentPage,
                currentPage < totalPages, currentPage > 1);
    }

    // --- private helpers ---

    private static final Map<CommandsRegistryTypes.CommandCategory, String> CATEGORY_LABELS = Map.of(
            CommandsRegistryTypes.CommandCategory.SESSION, "Session",
            CommandsRegistryTypes.CommandCategory.OPTIONS, "Options",
            CommandsRegistryTypes.CommandCategory.STATUS, "Status",
            CommandsRegistryTypes.CommandCategory.MANAGEMENT, "Management",
            CommandsRegistryTypes.CommandCategory.MEDIA, "Media",
            CommandsRegistryTypes.CommandCategory.TOOLS, "Tools",
            CommandsRegistryTypes.CommandCategory.DOCKS, "Docks");

    private static final List<CommandsRegistryTypes.CommandCategory> CATEGORY_ORDER = List.of(
            CommandsRegistryTypes.CommandCategory.SESSION,
            CommandsRegistryTypes.CommandCategory.OPTIONS,
            CommandsRegistryTypes.CommandCategory.STATUS,
            CommandsRegistryTypes.CommandCategory.MANAGEMENT,
            CommandsRegistryTypes.CommandCategory.MEDIA,
            CommandsRegistryTypes.CommandCategory.TOOLS,
            CommandsRegistryTypes.CommandCategory.DOCKS);

    /** Build items as [label, text] pairs. */
    private static List<String[]> buildCommandItems(
            List<CommandsRegistryTypes.ChatCommandDefinition> commands) {
        Map<CommandsRegistryTypes.CommandCategory, List<CommandsRegistryTypes.ChatCommandDefinition>> grouped = new LinkedHashMap<>();
        for (var cat : CATEGORY_ORDER)
            grouped.put(cat, new ArrayList<>());
        for (var cmd : commands) {
            var cat = cmd.category() != null ? cmd.category() : CommandsRegistryTypes.CommandCategory.TOOLS;
            grouped.computeIfAbsent(cat, k -> new ArrayList<>()).add(cmd);
        }
        List<String[]> items = new ArrayList<>();
        for (var cat : CATEGORY_ORDER) {
            for (var cmd : grouped.getOrDefault(cat, List.of())) {
                items.add(new String[] { CATEGORY_LABELS.getOrDefault(cat, "Other"),
                        formatCommandEntry(cmd) });
            }
        }
        return items;
    }

    private static String formatCommandEntry(CommandsRegistryTypes.ChatCommandDefinition cmd) {
        String primary = cmd.nativeName() != null
                ? "/" + cmd.nativeName()
                : (!cmd.textAliases().isEmpty() ? cmd.textAliases().get(0).trim() : "/" + cmd.key());
        Set<String> seen = new HashSet<>();
        seen.add(primary.toLowerCase());
        List<String> aliases = cmd.textAliases().stream()
                .map(String::trim).filter(s -> !s.isEmpty())
                .filter(s -> !s.equalsIgnoreCase(primary))
                .filter(s -> seen.add(s.toLowerCase()))
                .collect(Collectors.toList());
        String aliasLabel = aliases.isEmpty() ? "" : " (" + String.join(", ", aliases) + ")";
        String scopeLabel = cmd.scope() == CommandsRegistryTypes.CommandScope.TEXT ? " [text]" : "";
        return primary + aliasLabel + scopeLabel + " - " + cmd.description();
    }

    private static List<String> formatCommandList(List<String[]> items) {
        List<String> lines = new ArrayList<>();
        String currentLabel = null;
        for (String[] item : items) {
            if (!item[0].equals(currentLabel)) {
                if (!lines.isEmpty())
                    lines.add("");
                lines.add(item[0]);
                currentLabel = item[0];
            }
            lines.add("  " + item[1]);
        }
        return lines;
    }

    // --- util ---

    private static String resolve(Map<String, Object> map, String key, String fallback) {
        if (map == null)
            return fallback;
        Object val = map.get(key);
        return val instanceof String s && !s.isBlank() ? s : fallback;
    }

    private static long resolveLong(Map<String, Object> map, String key, long fallback) {
        if (map == null)
            return fallback;
        Object val = map.get(key);
        return val instanceof Number n ? n.longValue() : fallback;
    }

    private static Long resolveNullLong(Map<String, Object> map, String key) {
        if (map == null)
            return null;
        Object val = map.get(key);
        return val instanceof Number n ? n.longValue() : null;
    }

    private static String firstNonNull(String... values) {
        for (String v : values)
            if (v != null && !v.isBlank())
                return v;
        return "off";
    }

    private static boolean isGroup(Map<String, Object> entry, String sessionKey) {
        if (entry != null) {
            Object chatType = entry.get("chatType");
            if ("group".equals(chatType) || "channel".equals(chatType))
                return true;
        }
        if (sessionKey != null) {
            return sessionKey.contains(":group:") || sessionKey.contains(":channel:");
        }
        return false;
    }
}
