package com.openclaw.common.config;

import java.util.List;

/**
 * Main session key resolution utilities.
 * Corresponds to TypeScript's sessions/main-session.ts.
 */
public final class MainSessionKey {

    private MainSessionKey() {
    }

    // =========================================================================
    // Constants
    // =========================================================================

    public static final String DEFAULT_MAIN_KEY = "main";

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Normalize a main key value.
     */
    public static String normalizeMainKey(String mainKey) {
        if (mainKey == null || mainKey.isBlank()) {
            return DEFAULT_MAIN_KEY;
        }
        return mainKey.trim().toLowerCase();
    }

    /**
     * Normalize an agent ID (delegates to AgentDirs).
     */
    public static String normalizeAgentId(String id) {
        return AgentDirs.normalizeAgentId(id);
    }

    /**
     * Build the canonical session key for an agent's main session.
     */
    public static String buildAgentMainSessionKey(String agentId, String mainKey) {
        String normalizedAgentId = normalizeAgentId(agentId);
        String normalizedMainKey = normalizeMainKey(mainKey);
        return "agent:" + normalizedAgentId + ":" + normalizedMainKey;
    }

    /**
     * Resolve the main session key from configuration.
     */
    public static String resolveMainSessionKey(OpenClawConfig cfg) {
        if (cfg == null) {
            return buildAgentMainSessionKey(AgentDirs.DEFAULT_AGENT_ID, null);
        }

        var sessionCfg = cfg.getSession();
        if (sessionCfg != null && "global".equals(sessionCfg.getScope())) {
            return "global";
        }

        var agents = cfg.getAgents();
        List<OpenClawConfig.AgentEntry> list = (agents != null && agents.getEntries() != null)
                ? agents.getEntries()
                : List.of();

        String defaultAgentId = list.stream()
                .filter(a -> a != null && Boolean.TRUE.equals(a.getDefaultAgent()))
                .map(OpenClawConfig.AgentEntry::getId)
                .findFirst()
                .orElse(list.isEmpty() ? AgentDirs.DEFAULT_AGENT_ID
                        : (list.get(0).getId() != null ? list.get(0).getId()
                                : AgentDirs.DEFAULT_AGENT_ID));

        String agentId = normalizeAgentId(defaultAgentId);
        String mainKey = normalizeMainKey(sessionCfg != null ? sessionCfg.getMainKey() : null);
        return buildAgentMainSessionKey(agentId, mainKey);
    }

    /**
     * Resolve the agent-specific main session key.
     */
    public static String resolveAgentMainSessionKey(OpenClawConfig cfg, String agentId) {
        String mainKey = null;
        if (cfg != null && cfg.getSession() != null) {
            mainKey = cfg.getSession().getMainKey();
        }
        return buildAgentMainSessionKey(agentId, mainKey);
    }

    /**
     * Resolve an explicit agent session key, if agentId is provided.
     */
    public static String resolveExplicitAgentSessionKey(OpenClawConfig cfg, String agentId) {
        if (agentId == null || agentId.trim().isEmpty()) {
            return null;
        }
        return resolveAgentMainSessionKey(cfg, agentId);
    }

    /**
     * Canonicalize session key aliases (e.g. "main") to the canonical agent main
     * key.
     */
    public static String canonicalizeMainSessionAlias(OpenClawConfig cfg, String agentId,
            String sessionKey) {
        String raw = sessionKey != null ? sessionKey.trim() : "";
        if (raw.isEmpty()) {
            return raw;
        }

        String normalizedAgentId = normalizeAgentId(agentId);
        String mainKey = normalizeMainKey(cfg != null && cfg.getSession() != null
                ? cfg.getSession().getMainKey()
                : null);
        String agentMainSessionKey = buildAgentMainSessionKey(normalizedAgentId, mainKey);
        String agentMainAliasKey = buildAgentMainSessionKey(normalizedAgentId, "main");

        boolean isMainAlias = raw.equals("main")
                || raw.equals(mainKey)
                || raw.equals(agentMainSessionKey)
                || raw.equals(agentMainAliasKey);

        if (cfg != null && cfg.getSession() != null
                && "global".equals(cfg.getSession().getScope()) && isMainAlias) {
            return "global";
        }
        if (isMainAlias) {
            return agentMainSessionKey;
        }
        return raw;
    }

    /**
     * Extract agent ID from a session key string.
     * Expected format: "agent:{agentId}:{rest}"
     */
    public static String resolveAgentIdFromSessionKey(String sessionKey) {
        if (sessionKey == null || sessionKey.isBlank()) {
            return AgentDirs.DEFAULT_AGENT_ID;
        }
        if (sessionKey.startsWith("agent:")) {
            String[] parts = sessionKey.split(":", 3);
            if (parts.length >= 2 && !parts[1].isBlank()) {
                return normalizeAgentId(parts[1]);
            }
        }
        return AgentDirs.DEFAULT_AGENT_ID;
    }
}
