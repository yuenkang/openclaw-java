package com.openclaw.agent.runtime;

import com.openclaw.common.config.OpenClawConfig;

/**
 * Agent timeout resolution from config with overrides.
 * Mirrors {@code agents/timeout.ts}.
 */
public final class AgentTimeout {

    private AgentTimeout() {
    }

    public static final int DEFAULT_AGENT_TIMEOUT_SECONDS = 600;
    /** 30 days in milliseconds — used as "no timeout" sentinel. */
    public static final long NO_TIMEOUT_MS = 30L * 24 * 60 * 60 * 1000;

    /**
     * Resolve agent timeout in seconds from configuration.
     */
    public static int resolveAgentTimeoutSeconds(OpenClawConfig cfg) {
        if (cfg != null) {
            try {
                var agents = cfg.getAgents();
                if (agents != null && agents.getDefaults() != null) {
                    // AgentDefaults doesn't have timeoutSeconds yet;
                    // will use getter when the field is added.
                }
            } catch (Exception ignored) {
            }
        }
        return DEFAULT_AGENT_TIMEOUT_SECONDS;
    }

    /**
     * Resolve agent timeout in milliseconds with override support.
     *
     * @param cfg             config (may be null)
     * @param overrideMs      explicit override in milliseconds (0 means no timeout)
     * @param overrideSeconds explicit override in seconds (0 means no timeout)
     * @param minMs           minimum allowed value in milliseconds
     */
    public static long resolveAgentTimeoutMs(
            OpenClawConfig cfg,
            Long overrideMs,
            Long overrideSeconds,
            Long minMs) {
        long min = Math.max(minMs != null ? minMs : 1, 1);
        long defaultMs = (long) resolveAgentTimeoutSeconds(cfg) * 1000;

        if (overrideMs != null) {
            if (overrideMs == 0)
                return NO_TIMEOUT_MS;
            if (overrideMs < 0)
                return defaultMs;
            return Math.max(overrideMs, min);
        }
        if (overrideSeconds != null) {
            if (overrideSeconds == 0)
                return NO_TIMEOUT_MS;
            if (overrideSeconds < 0)
                return defaultMs;
            return Math.max(overrideSeconds * 1000, min);
        }
        return Math.max(defaultMs, min);
    }
}
