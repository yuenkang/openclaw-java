package com.openclaw.gateway.chat;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * In-memory queue of chat run entries per session.
 * <p>
 * Corresponds to TypeScript's {@code createChatRunRegistry()} in
 * server-chat.ts.
 * Each session can have a queue of pending chat runs.
 */
public class ChatRunRegistry {

    /**
     * A chat run entry linking a session key to a client-supplied run ID.
     */
    public record ChatRunEntry(String sessionKey, String clientRunId) {
    }

    private final Map<String, List<ChatRunEntry>> queues = new ConcurrentHashMap<>();

    /**
     * Append a run entry to the queue for the given session ID (agent run ID).
     */
    public void add(String sessionId, ChatRunEntry entry) {
        queues.computeIfAbsent(sessionId, k -> Collections.synchronizedList(new ArrayList<>()))
                .add(entry);
    }

    /**
     * Peek at the first queued run entry for a session without removing it.
     */
    public ChatRunEntry peek(String sessionId) {
        var queue = queues.get(sessionId);
        if (queue == null || queue.isEmpty()) {
            return null;
        }
        return queue.get(0);
    }

    /**
     * Remove and return the first queued run entry for a session.
     */
    public ChatRunEntry shift(String sessionId) {
        var queue = queues.get(sessionId);
        if (queue == null || queue.isEmpty()) {
            return null;
        }
        var entry = queue.remove(0);
        if (queue.isEmpty()) {
            queues.remove(sessionId);
        }
        return entry;
    }

    /**
     * Remove a specific run entry matching the client run ID (and optionally
     * session key).
     */
    public ChatRunEntry remove(String sessionId, String clientRunId, String sessionKey) {
        var queue = queues.get(sessionId);
        if (queue == null || queue.isEmpty()) {
            return null;
        }
        int idx = -1;
        for (int i = 0; i < queue.size(); i++) {
            var e = queue.get(i);
            if (e.clientRunId().equals(clientRunId)
                    && (sessionKey == null || e.sessionKey().equals(sessionKey))) {
                idx = i;
                break;
            }
        }
        if (idx < 0) {
            return null;
        }
        var entry = queue.remove(idx);
        if (queue.isEmpty()) {
            queues.remove(sessionId);
        }
        return entry;
    }

    /**
     * Clear all queued entries.
     */
    public void clear() {
        queues.clear();
    }
}
