package com.openclaw.agent.runtime;

import java.util.*;

/**
 * Tool display name/emoji/detail resolution for UI presentation.
 * Mirrors {@code agents/tool-display.ts}.
 */
public final class ToolDisplay {

    private ToolDisplay() {
    }

    // --- Public result type ---

    public record ToolDisplayInfo(
            String name,
            String emoji,
            String title,
            String label,
            String verb,
            String detail) {
    }

    // --- Config ---

    /** Detail key label overrides (e.g. "agentId" → "agent"). */
    private static final Map<String, String> DETAIL_LABEL_OVERRIDES = Map.ofEntries(
            Map.entry("agentId", "agent"),
            Map.entry("sessionKey", "session"),
            Map.entry("targetId", "target"),
            Map.entry("targetUrl", "url"),
            Map.entry("nodeId", "node"),
            Map.entry("requestId", "request"),
            Map.entry("messageId", "message"),
            Map.entry("threadId", "thread"),
            Map.entry("channelId", "channel"),
            Map.entry("guildId", "guild"),
            Map.entry("userId", "user"),
            Map.entry("runTimeoutSeconds", "timeout"),
            Map.entry("timeoutSeconds", "timeout"),
            Map.entry("includeTools", "tools"),
            Map.entry("pollQuestion", "poll"),
            Map.entry("maxChars", "max chars"));
    private static final int MAX_DETAIL_ENTRIES = 8;
    private static final String DEFAULT_EMOJI = "🧩";

    // --- Tool display spec map (simplified — JSON-driven in TS) ---

    private static final Map<String, ToolSpec> TOOL_MAP;

    static {
        Map<String, ToolSpec> m = new HashMap<>();
        m.put("read", new ToolSpec("📖", "Read", "Read", List.of("path"), null));
        m.put("write", new ToolSpec("✏️", "Write", "Write", List.of("path"), null));
        m.put("edit", new ToolSpec("✏️", "Edit", "Edit", List.of("path"), null));
        m.put("bash", new ToolSpec("💻", "Bash", "Bash", List.of("command"), null));
        m.put("glob", new ToolSpec("🔍", "Glob", "Glob", List.of("pattern"), null));
        m.put("grep", new ToolSpec("🔎", "Grep", "Grep", List.of("pattern", "path"), null));
        m.put("search", new ToolSpec("🔎", "Search", "Search", List.of("query"), null));
        m.put("apply_patch", new ToolSpec("🩹", "Apply Patch", "Apply Patch", List.of(), null));
        m.put("agent", new ToolSpec("🤖", "Agent", "Agent", List.of("agentId", "sessionKey"), null));
        m.put("mcp", new ToolSpec("🔌", "MCP", "MCP", List.of("method"), null));
        m.put("attach", new ToolSpec("📎", "Attach", "Attach", List.of("path"), null));
        m.put("list_dir", new ToolSpec("📂", "List Directory", "List Dir", List.of("path"), null));
        m.put("memory", new ToolSpec("🧠", "Memory", "Memory", List.of("query"), null));
        TOOL_MAP = Map.copyOf(m);
    }

    record ToolSpec(String emoji, String title, String label,
            List<String> detailKeys, Map<String, ActionSpec> actions) {
    }

    record ActionSpec(String label, List<String> detailKeys) {
    }

    // --- Public API ---

    /**
     * Resolve display information for a tool invocation.
     */
    public static ToolDisplayInfo resolveToolDisplay(String name, Object args, String meta) {
        String normalizedName = normalizeToolName(name);
        String key = normalizedName.toLowerCase();
        ToolSpec spec = TOOL_MAP.get(key);

        String emoji = spec != null ? spec.emoji() : DEFAULT_EMOJI;
        String title = spec != null ? spec.title() : defaultTitle(normalizedName);
        String label = spec != null ? spec.label() : title;

        // Resolve action
        String action = extractAction(args);
        ActionSpec actionSpec = resolveActionSpec(spec, action);
        String verb = normalizeVerb(actionSpec != null ? actionSpec.label() : action);

        // Resolve detail
        String detail = null;
        if ("read".equals(key)) {
            detail = resolveReadDetail(args);
        }
        if (detail == null && ("write".equals(key) || "edit".equals(key)
                || "attach".equals(key))) {
            detail = resolveWriteDetail(args);
        }

        List<String> detailKeys = actionSpec != null && actionSpec.detailKeys() != null
                ? actionSpec.detailKeys()
                : (spec != null && spec.detailKeys() != null ? spec.detailKeys() : List.of());
        if (detail == null && !detailKeys.isEmpty()) {
            detail = resolveDetailFromKeys(args, detailKeys);
        }
        if (detail == null && meta != null) {
            detail = meta;
        }
        if (detail != null) {
            detail = shortenHomePath(detail);
        }

        return new ToolDisplayInfo(normalizedName, emoji, title, label, verb, detail);
    }

    /** Format a tool display into "emoji label: verb · detail". */
    public static String formatToolSummary(ToolDisplayInfo display) {
        String detail = formatToolDetail(display);
        return detail != null
                ? display.emoji() + " " + display.label() + ": " + detail
                : display.emoji() + " " + display.label();
    }

    /** Format just the detail part: "verb · detail". */
    public static String formatToolDetail(ToolDisplayInfo display) {
        List<String> parts = new ArrayList<>();
        if (display.verb() != null)
            parts.add(display.verb());
        if (display.detail() != null)
            parts.add(display.detail());
        return parts.isEmpty() ? null : String.join(" · ", parts);
    }

    // --- Internal helpers ---

    private static String normalizeToolName(String name) {
        return (name == null || name.isBlank()) ? "tool" : name.trim();
    }

    private static String defaultTitle(String name) {
        String cleaned = name.replace('_', ' ').trim();
        if (cleaned.isEmpty())
            return "Tool";
        String[] parts = cleaned.split("\\s+");
        StringBuilder sb = new StringBuilder();
        for (String part : parts) {
            if (sb.length() > 0)
                sb.append(' ');
            if (part.length() <= 2 && part.toUpperCase().equals(part)) {
                sb.append(part);
            } else {
                sb.append(Character.toUpperCase(part.charAt(0))).append(part.substring(1));
            }
        }
        return sb.toString();
    }

    private static String normalizeVerb(String value) {
        if (value == null || value.isBlank())
            return null;
        return value.trim().replace('_', ' ');
    }

    @SuppressWarnings("unchecked")
    private static String extractAction(Object args) {
        if (args instanceof Map<?, ?> map) {
            Object action = map.get("action");
            return action instanceof String s ? s.trim() : null;
        }
        return null;
    }

    private static ActionSpec resolveActionSpec(ToolSpec spec, String action) {
        if (spec == null || action == null || spec.actions() == null)
            return null;
        return spec.actions().get(action);
    }

    @SuppressWarnings("unchecked")
    private static String resolveReadDetail(Object args) {
        if (!(args instanceof Map<?, ?> map))
            return null;
        Object pathObj = map.get("path");
        if (!(pathObj instanceof String path))
            return null;
        Object offsetObj = map.get("offset");
        Object limitObj = map.get("limit");
        if (offsetObj instanceof Number offset && limitObj instanceof Number limit) {
            return path + ":" + offset.intValue() + "-" + (offset.intValue() + limit.intValue());
        }
        return path;
    }

    @SuppressWarnings("unchecked")
    private static String resolveWriteDetail(Object args) {
        if (!(args instanceof Map<?, ?> map))
            return null;
        Object pathObj = map.get("path");
        return pathObj instanceof String s ? s : null;
    }

    @SuppressWarnings("unchecked")
    private static String resolveDetailFromKeys(Object args, List<String> keys) {
        List<String[]> entries = new ArrayList<>();
        for (String key : keys) {
            Object value = lookupValueByPath(args, key);
            String display = coerceDisplayValue(value);
            if (display != null) {
                entries.add(new String[] { formatDetailKey(key), display });
            }
        }
        if (entries.isEmpty())
            return null;
        if (entries.size() == 1)
            return entries.get(0)[1];

        Set<String> seen = new LinkedHashSet<>();
        List<String[]> unique = new ArrayList<>();
        for (String[] entry : entries) {
            String token = entry[0] + ":" + entry[1];
            if (seen.add(token))
                unique.add(entry);
        }
        if (unique.isEmpty())
            return null;

        StringBuilder sb = new StringBuilder();
        int limit = Math.min(unique.size(), MAX_DETAIL_ENTRIES);
        for (int i = 0; i < limit; i++) {
            if (i > 0)
                sb.append(" · ");
            sb.append(unique.get(i)[0]).append(' ').append(unique.get(i)[1]);
        }
        return sb.toString();
    }

    @SuppressWarnings("unchecked")
    private static Object lookupValueByPath(Object args, String path) {
        if (!(args instanceof Map))
            return null;
        Object current = args;
        for (String segment : path.split("\\.")) {
            if (segment.isEmpty() || !(current instanceof Map))
                return null;
            current = ((Map<String, Object>) current).get(segment);
        }
        return current;
    }

    private static String coerceDisplayValue(Object value) {
        if (value == null)
            return null;
        if (value instanceof String s) {
            String trimmed = s.trim();
            if (trimmed.isEmpty())
                return null;
            String firstLine = trimmed.split("\\r?\\n")[0].trim();
            if (firstLine.isEmpty())
                return null;
            return firstLine.length() > 160 ? firstLine.substring(0, 157) + "…" : firstLine;
        }
        if (value instanceof Boolean b)
            return b ? "true" : null;
        if (value instanceof Number n) {
            double d = n.doubleValue();
            if (!Double.isFinite(d) || d == 0)
                return null;
            return String.valueOf(n);
        }
        if (value instanceof List<?> list) {
            List<String> values = new ArrayList<>();
            for (Object item : list) {
                String display = coerceDisplayValue(item);
                if (display != null)
                    values.add(display);
            }
            if (values.isEmpty())
                return null;
            String preview = String.join(", ", values.subList(0, Math.min(3, values.size())));
            return values.size() > 3 ? preview + "…" : preview;
        }
        return null;
    }

    private static String formatDetailKey(String raw) {
        String[] segments = raw.split("\\.");
        String last = segments[segments.length - 1];
        String override = DETAIL_LABEL_OVERRIDES.get(last);
        if (override != null)
            return override;
        String cleaned = last.replace('_', ' ').replace('-', ' ');
        String spaced = cleaned.replaceAll("([a-z0-9])([A-Z])", "$1 $2");
        String result = spaced.trim().toLowerCase();
        return result.isEmpty() ? last.toLowerCase() : result;
    }

    private static String shortenHomePath(String value) {
        String home = System.getProperty("user.home");
        if (home != null && value.startsWith(home)) {
            return "~" + value.substring(home.length());
        }
        return value;
    }
}
