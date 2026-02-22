package com.openclaw.node;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for NodeSubscriptionManager — subscribe/unsubscribe + event sending.
 */
class NodeSubscriptionManagerTest {

    private ObjectMapper objectMapper;
    private NodeSubscriptionManager manager;

    @BeforeEach
    void setUp() {
        objectMapper = new ObjectMapper();
        manager = new NodeSubscriptionManager(objectMapper);
    }

    @Test
    void subscribe_and_query() {
        manager.subscribe("node-1", "session-A");
        manager.subscribe("node-1", "session-B");

        Set<String> sessions = manager.getSubscribedSessions("node-1");
        assertEquals(2, sessions.size());
        assertTrue(sessions.contains("session-A"));
        assertTrue(sessions.contains("session-B"));

        Set<String> nodes = manager.getSubscribedNodes("session-A");
        assertEquals(1, nodes.size());
        assertTrue(nodes.contains("node-1"));
    }

    @Test
    void unsubscribe_removes_correctly() {
        manager.subscribe("node-1", "session-A");
        manager.subscribe("node-1", "session-B");
        manager.unsubscribe("node-1", "session-A");

        Set<String> sessions = manager.getSubscribedSessions("node-1");
        assertEquals(1, sessions.size());
        assertFalse(sessions.contains("session-A"));
        assertTrue(sessions.contains("session-B"));

        // session-A should no longer have any subscriptions
        assertEquals(0, manager.getSubscribedNodes("session-A").size());
    }

    @Test
    void unsubscribeAll_removesNode() {
        manager.subscribe("node-1", "session-A");
        manager.subscribe("node-1", "session-B");
        manager.subscribe("node-2", "session-A");

        manager.unsubscribeAll("node-1");

        assertEquals(0, manager.getSubscribedSessions("node-1").size());
        // session-A should still be subscribed to node-2
        assertEquals(1, manager.getSubscribedNodes("session-A").size());
        assertTrue(manager.getSubscribedNodes("session-A").contains("node-2"));
    }

    @Test
    void sendToAllSubscribed_invokesCallbackForEachNode() {
        manager.subscribe("node-1", "session-A");
        manager.subscribe("node-2", "session-A");

        AtomicInteger count = new AtomicInteger(0);
        List<String> sentNodeIds = new ArrayList<>();

        manager.sendToAllSubscribed("test-event", "{}", (nodeId, payloadJSON) -> {
            count.incrementAndGet();
            sentNodeIds.add(nodeId);
        });

        assertEquals(2, count.get());
        assertTrue(sentNodeIds.contains("node-1"));
        assertTrue(sentNodeIds.contains("node-2"));
    }

    @Test
    void sendToAllSubscribed_noSubscribers_noCallback() {
        AtomicInteger count = new AtomicInteger(0);
        manager.sendToAllSubscribed("test-event", "{}", (nodeId, payloadJSON) -> count.incrementAndGet());
        assertEquals(0, count.get());
    }

    @Test
    void clear_removesAll() {
        manager.subscribe("node-1", "session-A");
        manager.subscribe("node-2", "session-B");
        manager.clear();

        assertEquals(0, manager.getSubscribedSessions("node-1").size());
        assertEquals(0, manager.getSubscribedNodes("session-A").size());
    }

    @Test
    void subscribe_ignoresBlankValues() {
        manager.subscribe("", "session-A");
        manager.subscribe("node-1", "");
        manager.subscribe(null, "session-A");
        manager.subscribe("node-1", null);

        assertEquals(0, manager.getSubscribedSessions("node-1").size());
        assertEquals(0, manager.getSubscribedNodes("session-A").size());
    }
}
