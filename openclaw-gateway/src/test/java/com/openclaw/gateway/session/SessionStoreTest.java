package com.openclaw.gateway.session;

import com.openclaw.common.model.AcpSession;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class SessionStoreTest {

    private SessionStore store;

    @BeforeEach
    void setUp() {
        store = new SessionStore();
    }

    @Test
    void createSession_returnsNewSession() {
        AcpSession session = store.createSession("key-1", "/tmp");

        assertNotNull(session);
        assertNotNull(session.getSessionId());
        assertEquals("key-1", session.getSessionKey());
        assertEquals("/tmp", session.getCwd());
    }

    @Test
    void findBySessionKey_returnsExisting() {
        store.createSession("key-1", "/tmp");

        Optional<AcpSession> found = store.findBySessionKey("key-1");
        assertTrue(found.isPresent());
        assertEquals("key-1", found.get().getSessionKey());
    }

    @Test
    void findBySessionKey_notFound_returnsEmpty() {
        Optional<AcpSession> found = store.findBySessionKey("nonexistent");
        assertTrue(found.isEmpty());
    }

    @Test
    void removeSession_removesFromStore() {
        AcpSession session = store.createSession("key-1", "/tmp");
        assertEquals(1, store.size());

        store.removeSession(session.getSessionId());
        assertEquals(0, store.size());
    }

    @Test
    void listSessions_returnsAll() {
        store.createSession("key-1", "/tmp");
        store.createSession("key-2", "/home");

        List<AcpSession> sessions = store.listSessions();
        assertEquals(2, sessions.size());
    }

    @Test
    void startRun_tracksRunToSession() {
        AcpSession session = store.createSession("key-1", "/tmp");
        store.startRun(session.getSessionId(), "run-123");

        assertEquals("run-123", session.getActiveRunId());
    }

    @Test
    void cancelRun_cancelsSession() {
        AcpSession session = store.createSession("key-1", "/tmp");
        store.startRun(session.getSessionId(), "run-123");

        boolean cancelled = store.cancelRun(session.getSessionId());
        assertTrue(cancelled);
        assertTrue(session.isCancelled());
    }

    @Test
    void cancelRun_noActiveRun_returnsFalse() {
        store.createSession("key-1", "/tmp");
        boolean cancelled = store.cancelRun("nonexistent");
        assertFalse(cancelled);
    }
}
