package com.openclaw.agent.tools;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Tool execution policy — controls which tools are allowed/denied based on
 * config.
 * Corresponds to TypeScript's tool-policy.ts / pi-tools.policy.ts.
 *
 * <p>
 * The policy is resolved from sandbox config or agent-specific overrides. Tools
 * can be globally allowed/denied, or scoped per-pattern (glob-like matching).
 * </p>
 */
@Slf4j
public class ToolPolicy {

    /**
     * Default allow-all policy (used when no sandbox/tool restrictions configured).
     */
    public static final ToolPolicy ALLOW_ALL = new ToolPolicy(Collections.emptySet(), Collections.emptySet(), true);

    private final Set<String> allowPatterns;
    private final Set<String> denyPatterns;
    private final boolean defaultAllow;

    public ToolPolicy(Set<String> allowPatterns, Set<String> denyPatterns, boolean defaultAllow) {
        this.allowPatterns = allowPatterns != null ? allowPatterns : Collections.emptySet();
        this.denyPatterns = denyPatterns != null ? denyPatterns : Collections.emptySet();
        this.defaultAllow = defaultAllow;
    }

    /**
     * Build a ToolPolicy from sandbox config.
     * If sandbox tools.allow is set, only those tools are allowed (whitelist mode).
     * If sandbox tools.deny is set, all tools except denied are allowed (blacklist
     * mode).
     */
    public static ToolPolicy fromConfig(OpenClawConfig cfg) {
        if (cfg == null || cfg.getSandbox() == null || cfg.getSandbox().getTools() == null) {
            return ALLOW_ALL;
        }
        OpenClawConfig.SandboxToolsConfig toolsCfg = cfg.getSandbox().getTools();
        Set<String> allow = toolsCfg.getAllow() != null
                ? new HashSet<>(toolsCfg.getAllow())
                : Collections.emptySet();
        Set<String> deny = toolsCfg.getDeny() != null
                ? new HashSet<>(toolsCfg.getDeny())
                : Collections.emptySet();

        // If allow list is set, default to deny-all (whitelist mode)
        boolean defaultPermit = allow.isEmpty();
        return new ToolPolicy(allow, deny, defaultPermit);
    }

    /**
     * Check if a tool is allowed by this policy.
     *
     * @param toolName the tool name to check
     * @return true if allowed, false if denied
     */
    public boolean isAllowed(String toolName) {
        if (toolName == null || toolName.isBlank())
            return false;

        // Explicit deny always wins
        if (matchesAny(toolName, denyPatterns)) {
            log.debug("Tool '{}' denied by deny-list", toolName);
            return false;
        }

        // Explicit allow
        if (!allowPatterns.isEmpty()) {
            boolean allowed = matchesAny(toolName, allowPatterns);
            if (!allowed) {
                log.debug("Tool '{}' not in allow-list", toolName);
            }
            return allowed;
        }

        return defaultAllow;
    }

    /**
     * Filter a collection of tool names, returning only those allowed.
     */
    public List<String> filterAllowed(Collection<String> toolNames) {
        List<String> result = new ArrayList<>();
        for (String name : toolNames) {
            if (isAllowed(name)) {
                result.add(name);
            }
        }
        return result;
    }

    /**
     * Simple glob-like matching: supports * as wildcard.
     */
    private boolean matchesAny(String toolName, Set<String> patterns) {
        for (String pattern : patterns) {
            if (matchesGlob(toolName, pattern)) {
                return true;
            }
        }
        return false;
    }

    private boolean matchesGlob(String name, String pattern) {
        if (pattern.equals("*"))
            return true;
        if (pattern.equals(name))
            return true;

        // Simple wildcard: "bash*" matches "bash", "bash.exec", etc.
        if (pattern.endsWith("*")) {
            String prefix = pattern.substring(0, pattern.length() - 1);
            return name.startsWith(prefix);
        }
        if (pattern.startsWith("*")) {
            String suffix = pattern.substring(1);
            return name.endsWith(suffix);
        }
        return false;
    }

    @Override
    public String toString() {
        if (this == ALLOW_ALL)
            return "ToolPolicy[ALLOW_ALL]";
        return String.format("ToolPolicy[allow=%s, deny=%s, default=%s]",
                allowPatterns, denyPatterns, defaultAllow ? "allow" : "deny");
    }
}
