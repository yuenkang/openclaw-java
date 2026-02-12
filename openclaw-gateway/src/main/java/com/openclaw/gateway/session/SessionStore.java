package com.openclaw.gateway.session;

import com.openclaw.common.model.AcpSession;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * In-memory session store.
 * Corresponds to TypeScript's AcpSessionStore (acp/session.ts).
 */
@Slf4j
public class SessionStore {

    private final Map<String, AcpSession> sessions = new ConcurrentHashMap<>();
    private final Map<String, String> runToSession = new ConcurrentHashMap<>();

    /**
     * Create a new session.
     */
    public AcpSession createSession(String sessionKey, String cwd) {
        return createSession(null, sessionKey, cwd);
    }

    /**
     * Create a new session with an explicit ID.
     */
    public AcpSession createSession(String sessionId, String sessionKey, String cwd) {
        String id = sessionId != null ? sessionId : UUID.randomUUID().toString();
        AcpSession session = AcpSession.builder()
                .sessionId(id)
                .sessionKey(sessionKey)
                .cwd(cwd)
                .createdAt(System.currentTimeMillis())
                .build();
        sessions.put(id, session);
        log.debug("Session created: {} (key={})", id, sessionKey);
        return session;
    }

    /**
     * Retrieve a session by ID.
     */
    public Optional<AcpSession> getSession(String sessionId) {
        return Optional.ofNullable(sessions.get(sessionId));
    }

    /**
     * Find a session by its session key.
     */
    public Optional<AcpSession> findBySessionKey(String sessionKey) {
        return sessions.values().stream()
                .filter(s -> sessionKey.equals(s.getSessionKey()))
                .findFirst();
    }

    /**
     * Start an active run on a session.
     */
    public void startRun(String sessionId, String runId) {
        AcpSession session = sessions.get(sessionId);
        if (session != null) {
            session.startRun(runId);
            runToSession.put(runId, sessionId);
            log.debug("Run started: {} on session {}", runId, sessionId);
        }
    }

    /**
     * End the active run on a session.
     */
    public void endRun(String sessionId) {
        AcpSession session = sessions.get(sessionId);
        if (session != null) {
            String runId = session.getActiveRunId();
            session.endRun();
            if (runId != null) {
                runToSession.remove(runId);
            }
            log.debug("Run ended on session {}", sessionId);
        }
    }

    /**
     * Cancel the active run on a session.
     */
    public boolean cancelRun(String sessionId) {
        AcpSession session = sessions.get(sessionId);
        if (session == null) {
            return false;
        }
        boolean cancelled = session.cancel();
        if (cancelled) {
            log.info("Run cancelled on session {}", sessionId);
        }
        return cancelled;
    }

    /**
     * Find session ID by run ID.
     */
    public Optional<String> findSessionByRunId(String runId) {
        return Optional.ofNullable(runToSession.get(runId));
    }

    /**
     * List all sessions.
     */
    public List<AcpSession> listSessions() {
        return new ArrayList<>(sessions.values());
    }

    /**
     * Remove a session.
     */
    public boolean removeSession(String sessionId) {
        AcpSession removed = sessions.remove(sessionId);
        if (removed != null && removed.getActiveRunId() != null) {
            runToSession.remove(removed.getActiveRunId());
        }
        return removed != null;
    }

    /**
     * Update a session atomically using the provided consumer.
     */
    public boolean updateSession(String sessionId, java.util.function.Consumer<AcpSession> updater) {
        AcpSession session = sessions.get(sessionId);
        if (session == null) {
            return false;
        }
        updater.accept(session);
        session.setUpdatedAt(System.currentTimeMillis());
        log.debug("Session updated: {}", sessionId);
        return true;
    }

    /**
     * Returns the total number of sessions.
     */
    public int size() {
        return sessions.size();
    }
}
