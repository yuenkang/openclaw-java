package com.openclaw.agent.runtime;

import java.nio.file.Path;

/**
 * Resolves standard directory paths for agent state and data.
 * Corresponds to TypeScript's agent-paths.ts + config/paths.ts.
 */
public class AgentPaths {

    public static final String DEFAULT_AGENT_ID = "default";
    private static final String DEFAULT_STATE_DIR_NAME = ".openclaw";

    /**
     * Resolve the global state directory.
     * Priority: OPENCLAW_STATE_DIR env → ~/.openclaw
     */
    public static String resolveStateDir() {
        String override = System.getenv("OPENCLAW_STATE_DIR");
        if (override != null && !override.isBlank()) {
            return resolveUserPath(override.trim());
        }
        return Path.of(System.getProperty("user.home"), DEFAULT_STATE_DIR_NAME).toString();
    }

    /**
     * Resolve the agent-specific directory (for agent state/data).
     * Priority: OPENCLAW_AGENT_DIR env → {stateDir}/agents/{agentId}/agent
     */
    public static String resolveAgentDir(String agentId) {
        String override = System.getenv("OPENCLAW_AGENT_DIR");
        if (override != null && !override.isBlank()) {
            return resolveUserPath(override.trim());
        }
        String id = agentId != null && !agentId.isBlank() ? agentId : DEFAULT_AGENT_ID;
        return Path.of(resolveStateDir(), "agents", id, "agent").toString();
    }

    /**
     * Resolve the agent's workspace directory.
     * Priority: explicit config → {stateDir}/workspace-{agentId} (non-default)
     * → {stateDir}/workspace (default agent)
     */
    public static String resolveWorkspaceDir(String agentId, String configuredWorkspace) {
        if (configuredWorkspace != null && !configuredWorkspace.isBlank()) {
            return resolveUserPath(configuredWorkspace.trim());
        }
        String id = agentId != null && !agentId.isBlank() ? agentId : DEFAULT_AGENT_ID;
        if (DEFAULT_AGENT_ID.equals(id)) {
            return Path.of(System.getProperty("user.home"), DEFAULT_STATE_DIR_NAME, "workspace")
                    .toString();
        }
        return Path.of(System.getProperty("user.home"), DEFAULT_STATE_DIR_NAME,
                "workspace-" + id).toString();
    }

    /**
     * Resolve the sessions directory for an agent.
     */
    public static String resolveSessionsDir(String agentId) {
        return Path.of(resolveAgentDir(agentId), "sessions").toString();
    }

    /**
     * Resolve ~ and environment variables in a path.
     */
    public static String resolveUserPath(String pathStr) {
        if (pathStr == null)
            return null;
        String resolved = pathStr.trim();
        if (resolved.startsWith("~/") || resolved.equals("~")) {
            resolved = System.getProperty("user.home") + resolved.substring(1);
        }
        return Path.of(resolved).toAbsolutePath().normalize().toString();
    }
}
