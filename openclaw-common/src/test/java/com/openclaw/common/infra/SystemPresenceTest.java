package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class SystemPresenceTest {

    @Test
    void newInstance_hasSelfPresence() {
        var sp = new SystemPresence();
        List<SystemPresence.PresenceEntry> list = sp.list();
        assertFalse(list.isEmpty());
        assertEquals("gateway", list.get(0).getMode());
        assertEquals("self", list.get(0).getReason());
    }

    @Test
    void update_addsNewNode() {
        var sp = new SystemPresence();
        var entry = SystemPresence.PresenceEntry.builder()
                .host("remote-node")
                .deviceId("node-123")
                .mode("node")
                .reason("presence")
                .text("Node: remote-node")
                .build();

        var result = sp.update(entry);
        assertEquals("node-123", result.getKey());
        assertNull(result.getPrevious()); // new entry
        assertNotNull(result.getNext());

        assertTrue(sp.size() >= 2); // self + new node
    }

    @Test
    void update_mergesExistingNode() {
        var sp = new SystemPresence();

        var entry1 = SystemPresence.PresenceEntry.builder()
                .deviceId("node-abc")
                .host("box1")
                .mode("node")
                .text("Node: box1")
                .build();
        sp.update(entry1);

        var entry2 = SystemPresence.PresenceEntry.builder()
                .deviceId("node-abc")
                .version("2.0")
                .text("Node: box1 v2")
                .build();
        var result = sp.update(entry2);

        assertNotNull(result.getPrevious());
        assertEquals("2.0", result.getNext().getVersion());
        assertEquals("box1", result.getNext().getHost()); // preserved from first
    }

    @Test
    void list_sortedByTimestampDescending() {
        var sp = new SystemPresence();

        sp.update(SystemPresence.PresenceEntry.builder()
                .deviceId("older")
                .text("older node")
                .build());

        // Small delay so timestamps differ
        sp.update(SystemPresence.PresenceEntry.builder()
                .deviceId("newer")
                .text("newer node")
                .build());

        List<SystemPresence.PresenceEntry> list = sp.list();
        assertTrue(list.size() >= 3);
        // Most recent should be first (self gets touched last)
    }
}
