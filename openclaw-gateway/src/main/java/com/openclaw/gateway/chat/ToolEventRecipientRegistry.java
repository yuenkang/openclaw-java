package com.openclaw.gateway.chat;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Tracks which WebSocket connections have subscribed to tool-verbose events
 * for a particular agent run.
 * <p>
 * Once a run reaches a final lifecycle phase, the recipient set is marked final
 * and will not accept new subscriptions.
 */
public class ToolEventRecipientRegistry {

    private final Map<String, Set<String>> recipients = new ConcurrentHashMap<>();
    private final Set<String> finalized = ConcurrentHashMap.newKeySet();

    /**
     * Register a connection ID as a recipient of tool events for the given run.
     */
    public void subscribe(String runId, String connectionId) {
        if (finalized.contains(runId)) {
            return;
        }
        recipients.computeIfAbsent(runId, k -> ConcurrentHashMap.newKeySet())
                .add(connectionId);
    }

    /**
     * Get all connection IDs subscribed to tool events for a run.
     */
    public Set<String> get(String runId) {
        return recipients.getOrDefault(runId, Set.of());
    }

    /**
     * Mark a run as finalized to prevent new subscriptions and schedule cleanup.
     */
    public void markFinal(String runId) {
        finalized.add(runId);
        recipients.remove(runId);
    }
}
