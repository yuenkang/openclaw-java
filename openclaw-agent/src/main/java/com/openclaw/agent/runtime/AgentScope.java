package com.openclaw.agent.runtime;

import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.common.config.OpenClawConfig.AgentEntry;

import java.util.*;

/**
 * Agent scope resolution — lists agents, resolves config, workspace, model.
 * Corresponds to TypeScript's agent-scope.ts.
 */
public class AgentScope {

    public static final String DEFAULT_AGENT_ID = "default";

    /**
     * Resolved agent configuration with all fields from entry + defaults.
     */
    public record ResolvedAgentConfig(
            String id,
            String name,
            String description,
            String model,
            String workspace,
            List<String> modelFallbacks) {
    }

    // =========================================================================
    // Agent listing
    // =========================================================================

    /**
     * List all agent entries from config.
     */
    public static List<AgentEntry> listAgents(OpenClawConfig cfg) {
        if (cfg == null || cfg.getAgents() == null)
            return List.of();
        return cfg.getAgents().getEntries();
    }

    /**
     * List all agent IDs from config, normalized.
     * Returns [DEFAULT_AGENT_ID] if none configured.
     */
    public static List<String> listAgentIds(OpenClawConfig cfg) {
        List<AgentEntry> agents = listAgents(cfg);
        if (agents.isEmpty())
            return List.of(DEFAULT_AGENT_ID);

        Set<String> seen = new LinkedHashSet<>();
        for (AgentEntry entry : agents) {
            String id = normalizeAgentId(entry.getId());
            seen.add(id);
        }
        return seen.isEmpty() ? List.of(DEFAULT_AGENT_ID) : new ArrayList<>(seen);
    }

    /**
     * Resolve the default agent ID.
     * Uses the first agent entry or DEFAULT_AGENT_ID.
     */
    public static String resolveDefaultAgentId(OpenClawConfig cfg) {
        List<AgentEntry> agents = listAgents(cfg);
        if (agents.isEmpty())
            return DEFAULT_AGENT_ID;
        String id = agents.get(0).getId();
        return id != null && !id.isBlank() ? normalizeAgentId(id) : DEFAULT_AGENT_ID;
    }

    // =========================================================================
    // Session → Agent resolution
    // =========================================================================

    /**
     * Parse agent ID from a session key.
     * Session keys format: "agentId:sessionId" or just "sessionId".
     */
    public static String resolveAgentIdFromSessionKey(String sessionKey) {
        if (sessionKey == null || sessionKey.isBlank())
            return DEFAULT_AGENT_ID;
        String trimmed = sessionKey.trim().toLowerCase();
        int colonIdx = trimmed.indexOf(':');
        if (colonIdx > 0) {
            return normalizeAgentId(trimmed.substring(0, colonIdx));
        }
        return DEFAULT_AGENT_ID;
    }

    /**
     * Resolve the agent ID for a session, considering default fallback.
     */
    public static String resolveSessionAgentId(String sessionKey, OpenClawConfig cfg) {
        String fromKey = resolveAgentIdFromSessionKey(sessionKey);
        if (!DEFAULT_AGENT_ID.equals(fromKey)) {
            return fromKey;
        }
        return resolveDefaultAgentId(cfg != null ? cfg : new OpenClawConfig());
    }

    // =========================================================================
    // Agent config resolution
    // =========================================================================

    /**
     * Find the AgentEntry for a given agent ID (case-insensitive).
     */
    public static AgentEntry resolveAgentEntry(OpenClawConfig cfg, String agentId) {
        String id = normalizeAgentId(agentId);
        for (AgentEntry entry : listAgents(cfg)) {
            if (id.equals(normalizeAgentId(entry.getId()))) {
                return entry;
            }
        }
        return null;
    }

    /**
     * Resolve full agent config for a given agent ID.
     */
    public static ResolvedAgentConfig resolveAgentConfig(OpenClawConfig cfg, String agentId) {
        AgentEntry entry = resolveAgentEntry(cfg, agentId);
        if (entry == null)
            return null;
        return new ResolvedAgentConfig(
                normalizeAgentId(entry.getId()),
                entry.getName(),
                entry.getDescription(),
                entry.getModelString(),
                null, // workspace from entry if available
                entry.getModelFallbacks());
    }

    // =========================================================================
    // Model resolution
    // =========================================================================

    /**
     * Resolve the primary model for an agent.
     * Falls back to agent defaults, then global config.
     */
    public static String resolveAgentModelPrimary(OpenClawConfig cfg, String agentId) {
        // Agent-specific model
        ResolvedAgentConfig agentCfg = resolveAgentConfig(cfg, agentId);
        if (agentCfg != null && agentCfg.model() != null && !agentCfg.model().isBlank()) {
            return agentCfg.model().trim();
        }
        // Agent defaults
        if (cfg.getAgents() != null && cfg.getAgents().getDefaults() != null) {
            var defaultsModel = cfg.getAgents().getDefaults().getModel();
            if (defaultsModel != null && defaultsModel.getPrimary() != null && !defaultsModel.getPrimary().isBlank()) {
                return defaultsModel.getPrimary().trim();
            }
        }
        return null;
    }

    /**
     * Resolve fallback models for an agent.
     * Agent-specific overrides agent-defaults.
     */
    public static List<String> resolveAgentModelFallbacks(OpenClawConfig cfg, String agentId) {
        // Agent-specific fallbacks
        ResolvedAgentConfig agentCfg = resolveAgentConfig(cfg, agentId);
        if (agentCfg != null && agentCfg.modelFallbacks() != null) {
            return agentCfg.modelFallbacks();
        }
        // Agent defaults fallbacks
        if (cfg.getAgents() != null && cfg.getAgents().getDefaults() != null) {
            var defaultsModel = cfg.getAgents().getDefaults().getModel();
            if (defaultsModel != null && defaultsModel.getFallbacks() != null)
                return defaultsModel.getFallbacks();
        }
        return List.of();
    }

    // =========================================================================
    // Workspace resolution
    // =========================================================================

    /**
     * Resolve the workspace directory for an agent.
     */
    public static String resolveAgentWorkspaceDir(OpenClawConfig cfg, String agentId) {
        ResolvedAgentConfig agentCfg = resolveAgentConfig(cfg, agentId);
        if (agentCfg != null && agentCfg.workspace() != null) {
            return AgentPaths.resolveUserPath(agentCfg.workspace());
        }
        return AgentPaths.resolveWorkspaceDir(agentId, null);
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    /**
     * Normalize an agent ID: trim, lowercase, default if blank.
     */
    public static String normalizeAgentId(String agentId) {
        if (agentId == null || agentId.isBlank())
            return DEFAULT_AGENT_ID;
        return agentId.trim().toLowerCase();
    }
}
