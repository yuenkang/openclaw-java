package com.openclaw.common.config;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

/**
 * Session file path resolution utilities.
 * Corresponds to TypeScript's sessions/paths.ts.
 */
public final class SessionPaths {

    private SessionPaths() {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Resolve the sessions directory for a given agent.
     */
    public static Path resolveAgentSessionsDir(String agentId) {
        String id = AgentDirs.normalizeAgentId(agentId != null ? agentId : AgentDirs.DEFAULT_AGENT_ID);
        return ConfigPaths.resolveStateDir().resolve("agents").resolve(id).resolve("sessions");
    }

    /**
     * Resolve the transcripts directory (default agent).
     */
    public static Path resolveSessionTranscriptsDir() {
        return resolveAgentSessionsDir(AgentDirs.DEFAULT_AGENT_ID);
    }

    /**
     * Resolve the transcripts directory for a specific agent.
     */
    public static Path resolveSessionTranscriptsDirForAgent(String agentId) {
        return resolveAgentSessionsDir(agentId);
    }

    /**
     * Resolve the default session store file path.
     */
    public static Path resolveDefaultSessionStorePath(String agentId) {
        return resolveAgentSessionsDir(agentId).resolve("sessions.json");
    }

    /**
     * Resolve the transcript file path for a session.
     */
    public static Path resolveSessionTranscriptPath(String sessionId, String agentId, Object topicId) {
        String safeTopicId = null;
        if (topicId instanceof String s) {
            safeTopicId = URLEncoder.encode(s, StandardCharsets.UTF_8);
        } else if (topicId instanceof Number n) {
            safeTopicId = String.valueOf(n.longValue());
        }
        String fileName = safeTopicId != null
                ? sessionId + "-topic-" + safeTopicId + ".jsonl"
                : sessionId + ".jsonl";
        return resolveAgentSessionsDir(agentId).resolve(fileName);
    }

    /**
     * Resolve the transcript file path for a session (no topic).
     */
    public static Path resolveSessionTranscriptPath(String sessionId, String agentId) {
        return resolveSessionTranscriptPath(sessionId, agentId, null);
    }

    /**
     * Resolve the session file path, using the entry's sessionFile if available.
     */
    public static Path resolveSessionFilePath(String sessionId, String sessionFile, String agentId) {
        if (sessionFile != null && !sessionFile.trim().isEmpty()) {
            return Path.of(sessionFile.trim());
        }
        return resolveSessionTranscriptPath(sessionId, agentId);
    }

    /**
     * Resolve the store path from configuration.
     */
    public static Path resolveStorePath(String store, String agentId) {
        String id = AgentDirs.normalizeAgentId(agentId != null ? agentId : AgentDirs.DEFAULT_AGENT_ID);
        if (store == null || store.isBlank()) {
            return resolveDefaultSessionStorePath(id);
        }
        String expanded = store;
        if (expanded.contains("{agentId}")) {
            expanded = expanded.replace("{agentId}", id);
        }
        if (expanded.startsWith("~")) {
            expanded = System.getProperty("user.home") + expanded.substring(1);
        }
        return Path.of(expanded).toAbsolutePath().normalize();
    }
}
