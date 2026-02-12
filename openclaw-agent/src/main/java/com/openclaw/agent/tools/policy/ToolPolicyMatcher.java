package com.openclaw.agent.tools.policy;

import com.openclaw.agent.tools.policy.ToolPolicyTypes.ToolPolicy;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.regex.Pattern;

/**
 * Tool policy matching: compile allow/deny patterns, filter tool lists, check
 * single tool access.
 * Corresponds to TypeScript pi-tools.policy.ts (pattern matching + filtering).
 */
@Slf4j
public final class ToolPolicyMatcher {

    private ToolPolicyMatcher() {
    }

    /** Compiled pattern for matching tool names. */
    private sealed interface CompiledPattern permits AllPattern, ExactPattern, RegexPattern {
    }

    private record AllPattern() implements CompiledPattern {
    }

    private record ExactPattern(String value) implements CompiledPattern {
    }

    private record RegexPattern(Pattern value) implements CompiledPattern {
    }

    private static CompiledPattern compilePattern(String pattern) {
        String normalized = ToolPolicyUtils.normalizeToolName(pattern);
        if (normalized.isEmpty())
            return new ExactPattern("");
        if ("*".equals(normalized))
            return new AllPattern();
        if (!normalized.contains("*"))
            return new ExactPattern(normalized);
        String escaped = Pattern.quote(normalized).replace("\\*", ".*");
        return new RegexPattern(Pattern.compile("^" + escaped + "$"));
    }

    private static List<CompiledPattern> compilePatterns(List<String> patterns) {
        if (patterns == null || patterns.isEmpty())
            return List.of();
        return ToolPolicyUtils.expandToolGroups(patterns).stream()
                .map(ToolPolicyMatcher::compilePattern)
                .filter(p -> !(p instanceof ExactPattern e && e.value().isEmpty()))
                .toList();
    }

    private static boolean matchesAny(String name, List<CompiledPattern> patterns) {
        for (CompiledPattern p : patterns) {
            if (p instanceof AllPattern)
                return true;
            if (p instanceof ExactPattern e && name.equals(e.value()))
                return true;
            if (p instanceof RegexPattern r && r.value().matcher(name).matches())
                return true;
        }
        return false;
    }

    /**
     * Check if a tool name is allowed by a single policy.
     */
    public static boolean isToolAllowedByPolicy(String name, ToolPolicy policy) {
        if (policy == null)
            return true;
        String normalized = ToolPolicyUtils.normalizeToolName(name);
        List<CompiledPattern> deny = compilePatterns(policy.deny());
        List<CompiledPattern> allow = compilePatterns(policy.allow());
        if (matchesAny(normalized, deny))
            return false;
        if (allow.isEmpty())
            return true;
        if (matchesAny(normalized, allow))
            return true;
        // apply_patch is allowed if exec is allowed
        if ("apply_patch".equals(normalized) && matchesAny("exec", allow))
            return true;
        return false;
    }

    /**
     * Check if a tool name is allowed by all policies.
     */
    public static boolean isToolAllowedByPolicies(String name, ToolPolicy... policies) {
        for (ToolPolicy p : policies) {
            if (!isToolAllowedByPolicy(name, p))
                return false;
        }
        return true;
    }

    /**
     * Check if a tool name is allowed by all policies in a list.
     */
    public static boolean isToolAllowedByPolicies(String name, List<ToolPolicy> policies) {
        for (ToolPolicy p : policies) {
            if (!isToolAllowedByPolicy(name, p))
                return false;
        }
        return true;
    }

    /**
     * Filter a list of tool names by a policy.
     */
    public static List<String> filterToolNamesByPolicy(List<String> toolNames, ToolPolicy policy) {
        if (policy == null)
            return toolNames;
        List<CompiledPattern> deny = compilePatterns(policy.deny());
        List<CompiledPattern> allow = compilePatterns(policy.allow());
        return toolNames.stream().filter(name -> {
            String normalized = ToolPolicyUtils.normalizeToolName(name);
            if (matchesAny(normalized, deny))
                return false;
            if (allow.isEmpty())
                return true;
            if (matchesAny(normalized, allow))
                return true;
            if ("apply_patch".equals(normalized) && matchesAny("exec", allow))
                return true;
            return false;
        }).toList();
    }

    // ── Subagent default deny list ──────────────────────────────────

    private static final List<String> DEFAULT_SUBAGENT_TOOL_DENY = List.of(
            "sessions_list", "sessions_history", "sessions_send", "sessions_spawn",
            "gateway", "agents_list", "whatsapp_login", "session_status", "cron",
            "memory_search", "memory_get");

    /**
     * Resolve the tool policy for subagent sessions.
     */
    public static ToolPolicy resolveSubagentToolPolicy(Map<String, Object> toolsConfig) {
        List<String> deny = new ArrayList<>(DEFAULT_SUBAGENT_TOOL_DENY);
        if (toolsConfig != null) {
            Object configuredDeny = toolsConfig.get("deny");
            if (configuredDeny instanceof List<?> l) {
                for (Object item : l) {
                    if (item instanceof String s)
                        deny.add(s);
                }
            }
        }
        List<String> allow = null;
        if (toolsConfig != null) {
            Object configuredAllow = toolsConfig.get("allow");
            if (configuredAllow instanceof List<?> l) {
                List<String> al = new ArrayList<>();
                for (Object item : l) {
                    if (item instanceof String s)
                        al.add(s);
                }
                if (!al.isEmpty())
                    allow = al;
            }
        }
        return new ToolPolicy(allow, deny);
    }
}
