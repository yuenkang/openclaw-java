package com.openclaw.agent.sandbox;

import com.openclaw.agent.sandbox.SandboxTypes.*;

import java.util.*;
import java.util.regex.Pattern;

/**
 * Sandbox tool policy — pattern matching and per-agent policy resolution.
 * Corresponds to TypeScript sandbox/tool-policy.ts.
 */
public final class SandboxToolPolicyResolver {

    private SandboxToolPolicyResolver() {
    }

    // ── Pattern matching ────────────────────────────────────────────

    private sealed interface CompiledPattern {
        record All() implements CompiledPattern {
        }

        record Exact(String value) implements CompiledPattern {
        }

        record Regex(Pattern pattern) implements CompiledPattern {
        }
    }

    private static CompiledPattern compilePattern(String pattern) {
        String normalized = pattern == null ? "" : pattern.trim().toLowerCase();
        if (normalized.isEmpty()) {
            return new CompiledPattern.Exact("");
        }
        if ("*".equals(normalized)) {
            return new CompiledPattern.All();
        }
        if (!normalized.contains("*")) {
            return new CompiledPattern.Exact(normalized);
        }
        // Convert wildcard to regex
        String escaped = Pattern.quote(normalized).replace("\\*", ".*");
        return new CompiledPattern.Regex(Pattern.compile("^" + escaped + "$"));
    }

    private static List<CompiledPattern> compilePatterns(List<String> patterns) {
        if (patterns == null || patterns.isEmpty()) {
            return List.of();
        }
        return patterns.stream()
                .map(SandboxToolPolicyResolver::compilePattern)
                .filter(p -> !(p instanceof CompiledPattern.Exact e) || !e.value().isEmpty())
                .toList();
    }

    private static boolean matchesAny(String name, List<CompiledPattern> patterns) {
        for (CompiledPattern p : patterns) {
            if (p instanceof CompiledPattern.All)
                return true;
            if (p instanceof CompiledPattern.Exact e && name.equals(e.value()))
                return true;
            if (p instanceof CompiledPattern.Regex r && r.pattern().matcher(name).matches())
                return true;
        }
        return false;
    }

    /**
     * Check if a tool is allowed by the sandbox tool policy.
     */
    public static boolean isToolAllowed(SandboxToolPolicy policy, String name) {
        String normalized = name == null ? "" : name.trim().toLowerCase();
        List<CompiledPattern> deny = compilePatterns(policy.getDeny());
        if (matchesAny(normalized, deny)) {
            return false;
        }
        List<CompiledPattern> allow = compilePatterns(policy.getAllow());
        if (allow.isEmpty()) {
            return true;
        }
        return matchesAny(normalized, allow);
    }

    // ── Policy resolution ───────────────────────────────────────────

    /**
     * Resolve sandbox tool policy for a specific agent, merging agent → global →
     * default.
     */
    public static SandboxToolPolicyResolved resolveSandboxToolPolicyForAgent(
            Map<String, Object> agentToolConfig,
            Map<String, Object> globalToolConfig) {

        @SuppressWarnings("unchecked")
        List<String> agentAllow = agentToolConfig != null
                ? (List<String>) agentToolConfig.get("allow")
                : null;
        @SuppressWarnings("unchecked")
        List<String> agentDeny = agentToolConfig != null
                ? (List<String>) agentToolConfig.get("deny")
                : null;
        @SuppressWarnings("unchecked")
        List<String> globalAllow = globalToolConfig != null
                ? (List<String>) globalToolConfig.get("allow")
                : null;
        @SuppressWarnings("unchecked")
        List<String> globalDeny = globalToolConfig != null
                ? (List<String>) globalToolConfig.get("deny")
                : null;

        SandboxToolPolicySource allowSource;
        if (agentAllow != null) {
            allowSource = SandboxToolPolicySource.builder()
                    .source(ToolPolicySourceType.AGENT)
                    .key("agents.list[].tools.sandbox.tools.allow").build();
        } else if (globalAllow != null) {
            allowSource = SandboxToolPolicySource.builder()
                    .source(ToolPolicySourceType.GLOBAL)
                    .key("tools.sandbox.tools.allow").build();
        } else {
            allowSource = SandboxToolPolicySource.builder()
                    .source(ToolPolicySourceType.DEFAULT)
                    .key("tools.sandbox.tools.allow").build();
        }

        SandboxToolPolicySource denySource;
        if (agentDeny != null) {
            denySource = SandboxToolPolicySource.builder()
                    .source(ToolPolicySourceType.AGENT)
                    .key("agents.list[].tools.sandbox.tools.deny").build();
        } else if (globalDeny != null) {
            denySource = SandboxToolPolicySource.builder()
                    .source(ToolPolicySourceType.GLOBAL)
                    .key("tools.sandbox.tools.deny").build();
        } else {
            denySource = SandboxToolPolicySource.builder()
                    .source(ToolPolicySourceType.DEFAULT)
                    .key("tools.sandbox.tools.deny").build();
        }

        List<String> deny = agentDeny != null ? agentDeny
                : globalDeny != null ? globalDeny
                        : new ArrayList<>(SandboxConstants.DEFAULT_TOOL_DENY);

        List<String> allow = agentAllow != null ? agentAllow
                : globalAllow != null ? globalAllow
                        : new ArrayList<>(SandboxConstants.DEFAULT_TOOL_ALLOW);

        // Ensure "image" is always allowed unless explicitly denied
        Set<String> denyLower = new HashSet<>();
        deny.stream().map(String::toLowerCase).forEach(denyLower::add);
        Set<String> allowLower = new HashSet<>();
        allow.stream().map(String::toLowerCase).forEach(allowLower::add);

        if (!denyLower.contains("image") && !allowLower.contains("image")) {
            allow = new ArrayList<>(allow);
            allow.add("image");
        }

        return SandboxToolPolicyResolved.builder()
                .allow(allow)
                .deny(deny)
                .sources(SandboxToolPolicyResolved.PolicySources.builder()
                        .allow(allowSource)
                        .deny(denySource)
                        .build())
                .build();
    }
}
