package com.openclaw.common.config;

/**
 * Agent concurrency limits.
 * Corresponds to TypeScript's agent-limits.ts.
 */
public final class AgentLimits {

    private AgentLimits() {
    }

    public static final int DEFAULT_AGENT_MAX_CONCURRENT = 4;
    public static final int DEFAULT_SUBAGENT_MAX_CONCURRENT = 8;

    /**
     * Resolve the maximum agent concurrency from config, falling back to default.
     */
    public static int resolveAgentMaxConcurrent(OpenClawConfig cfg) {
        if (cfg == null || cfg.getAgents() == null || cfg.getAgents().getDefaults() == null) {
            return DEFAULT_AGENT_MAX_CONCURRENT;
        }
        Integer raw = cfg.getAgents().getDefaults().getMaxConcurrent();
        if (raw != null) {
            return Math.max(1, raw);
        }
        return DEFAULT_AGENT_MAX_CONCURRENT;
    }

    /**
     * Resolve the maximum sub-agent concurrency from config, falling back to
     * default.
     */
    public static int resolveSubagentMaxConcurrent(OpenClawConfig cfg) {
        if (cfg == null || cfg.getAgents() == null || cfg.getAgents().getDefaults() == null) {
            return DEFAULT_SUBAGENT_MAX_CONCURRENT;
        }
        var subagents = cfg.getAgents().getDefaults().getSubagents();
        if (subagents != null && subagents.getMaxConcurrent() != null) {
            return Math.max(1, subagents.getMaxConcurrent());
        }
        return DEFAULT_SUBAGENT_MAX_CONCURRENT;
    }
}
