package com.openclaw.agent.tools.policy;

import com.openclaw.agent.tools.policy.ToolPolicyTypes.*;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Tool name aliasing, grouping, profile resolution, owner-only policy, and
 * plugin group expansion.
 * Corresponds to TypeScript tool-policy.ts (logic).
 */
@Slf4j
public final class ToolPolicyUtils {

    private ToolPolicyUtils() {
    }

    // ── Name aliases ────────────────────────────────────────────────

    private static final Map<String, String> TOOL_NAME_ALIASES = Map.of(
            "bash", "exec",
            "apply-patch", "apply_patch");

    public static String normalizeToolName(String name) {
        if (name == null)
            return "";
        String normalized = name.trim().toLowerCase();
        return TOOL_NAME_ALIASES.getOrDefault(normalized, normalized);
    }

    public static List<String> normalizeToolList(List<String> list) {
        if (list == null)
            return List.of();
        return list.stream()
                .map(ToolPolicyUtils::normalizeToolName)
                .filter(s -> !s.isEmpty())
                .toList();
    }

    // ── Tool groups ─────────────────────────────────────────────────

    public static final Map<String, List<String>> TOOL_GROUPS;
    static {
        Map<String, List<String>> m = new LinkedHashMap<>();
        m.put("group:memory", List.of("memory_search", "memory_get"));
        m.put("group:web", List.of("web_search", "web_fetch"));
        m.put("group:fs", List.of("read", "write", "edit", "apply_patch"));
        m.put("group:runtime", List.of("exec", "process"));
        m.put("group:sessions",
                List.of("sessions_list", "sessions_history", "sessions_send", "sessions_spawn", "session_status"));
        m.put("group:ui", List.of("browser", "canvas"));
        m.put("group:automation", List.of("cron", "gateway"));
        m.put("group:messaging", List.of("message"));
        m.put("group:nodes", List.of("nodes"));
        m.put("group:openclaw", List.of(
                "browser", "canvas", "nodes", "cron", "message", "gateway",
                "agents_list", "sessions_list", "sessions_history", "sessions_send",
                "sessions_spawn", "session_status", "memory_search", "memory_get",
                "web_search", "web_fetch", "image"));
        TOOL_GROUPS = Collections.unmodifiableMap(m);
    }

    /** Expand group references in a tool list. */
    public static List<String> expandToolGroups(List<String> list) {
        List<String> normalized = normalizeToolList(list);
        List<String> expanded = new ArrayList<>();
        for (String value : normalized) {
            List<String> group = TOOL_GROUPS.get(value);
            if (group != null) {
                expanded.addAll(group);
            } else {
                expanded.add(value);
            }
        }
        return expanded.stream().distinct().toList();
    }

    // ── Tool profiles ───────────────────────────────────────────────

    private static final Map<ToolProfileId, ToolPolicy> TOOL_PROFILES;
    static {
        Map<ToolProfileId, ToolPolicy> m = new EnumMap<>(ToolProfileId.class);
        m.put(ToolProfileId.minimal, new ToolPolicy(List.of("session_status"), null));
        m.put(ToolProfileId.coding, new ToolPolicy(
                List.of("group:fs", "group:runtime", "group:sessions", "group:memory", "image"), null));
        m.put(ToolProfileId.messaging, new ToolPolicy(
                List.of("group:messaging", "sessions_list", "sessions_history", "sessions_send", "session_status"),
                null));
        m.put(ToolProfileId.full, new ToolPolicy());
        TOOL_PROFILES = Collections.unmodifiableMap(m);
    }

    public static ToolPolicy resolveToolProfilePolicy(String profile) {
        if (profile == null || profile.isBlank())
            return null;
        try {
            ToolProfileId id = ToolProfileId.valueOf(profile.trim().toLowerCase());
            ToolPolicy resolved = TOOL_PROFILES.get(id);
            if (resolved == null || (resolved.allow() == null && resolved.deny() == null))
                return null;
            return new ToolPolicy(
                    resolved.allow() != null ? new ArrayList<>(resolved.allow()) : null,
                    resolved.deny() != null ? new ArrayList<>(resolved.deny()) : null);
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    // ── Owner-only policy ───────────────────────────────────────────

    private static final Set<String> OWNER_ONLY_TOOL_NAMES = Set.of("whatsapp_login");

    public static boolean isOwnerOnlyToolName(String name) {
        return OWNER_ONLY_TOOL_NAMES.contains(normalizeToolName(name));
    }

    // ── Explicit allowlist collection ───────────────────────────────

    @SafeVarargs
    public static List<String> collectExplicitAllowlist(ToolPolicy... policies) {
        List<String> entries = new ArrayList<>();
        for (ToolPolicy p : policies) {
            if (p == null || p.allow() == null)
                continue;
            for (String v : p.allow()) {
                if (v != null && !v.isBlank())
                    entries.add(v.trim());
            }
        }
        return entries;
    }

    // ── Plugin group expansion ──────────────────────────────────────

    public static List<String> expandPluginGroups(List<String> list, PluginToolGroups groups) {
        if (list == null || list.isEmpty())
            return list;
        List<String> expanded = new ArrayList<>();
        for (String entry : list) {
            String normalized = normalizeToolName(entry);
            if ("group:plugins".equals(normalized)) {
                if (!groups.all().isEmpty())
                    expanded.addAll(groups.all());
                else
                    expanded.add(normalized);
                continue;
            }
            List<String> tools = groups.byPlugin().get(normalized);
            if (tools != null && !tools.isEmpty()) {
                expanded.addAll(tools);
            } else {
                expanded.add(normalized);
            }
        }
        return new ArrayList<>(new LinkedHashSet<>(expanded));
    }

    public static ToolPolicy expandPolicyWithPluginGroups(ToolPolicy policy, PluginToolGroups groups) {
        if (policy == null)
            return null;
        return new ToolPolicy(
                expandPluginGroups(policy.allow(), groups),
                expandPluginGroups(policy.deny(), groups));
    }

    // ── Build plugin tool groups from tool list ─────────────────────

    public static <T> PluginToolGroups buildPluginToolGroups(
            List<T> tools,
            Function<T, String> nameExtractor,
            Function<T, String> pluginIdExtractor) {
        List<String> all = new ArrayList<>();
        Map<String, List<String>> byPlugin = new LinkedHashMap<>();
        for (T tool : tools) {
            String pid = pluginIdExtractor.apply(tool);
            if (pid == null)
                continue;
            String name = normalizeToolName(nameExtractor.apply(tool));
            all.add(name);
            byPlugin.computeIfAbsent(pid.toLowerCase(), k -> new ArrayList<>()).add(name);
        }
        return new PluginToolGroups(all, byPlugin);
    }

    // ── Strip plugin-only allowlist ─────────────────────────────────

    public static AllowlistResolution stripPluginOnlyAllowlist(
            ToolPolicy policy, PluginToolGroups groups, Set<String> coreTools) {
        if (policy == null || policy.allow() == null || policy.allow().isEmpty()) {
            return new AllowlistResolution(policy, List.of(), false);
        }
        List<String> normalized = normalizeToolList(policy.allow());
        if (normalized.isEmpty()) {
            return new AllowlistResolution(policy, List.of(), false);
        }
        Set<String> pluginIds = groups.byPlugin().keySet();
        Set<String> pluginTools = new HashSet<>(groups.all());
        List<String> unknownAllowlist = new ArrayList<>();
        boolean hasCoreEntry = false;
        for (String entry : normalized) {
            if ("*".equals(entry)) {
                hasCoreEntry = true;
                continue;
            }
            boolean isPluginEntry = "group:plugins".equals(entry)
                    || pluginIds.contains(entry) || pluginTools.contains(entry);
            List<String> expanded = expandToolGroups(List.of(entry));
            boolean isCoreEntry = expanded.stream().anyMatch(coreTools::contains);
            if (isCoreEntry)
                hasCoreEntry = true;
            if (!isCoreEntry && !isPluginEntry)
                unknownAllowlist.add(entry);
        }
        boolean strippedAllowlist = !hasCoreEntry;
        ToolPolicy resultPolicy = strippedAllowlist
                ? new ToolPolicy(null, policy.deny())
                : policy;
        return new AllowlistResolution(resultPolicy,
                unknownAllowlist.stream().distinct().toList(), strippedAllowlist);
    }
}
