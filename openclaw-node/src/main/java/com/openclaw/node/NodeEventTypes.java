package com.openclaw.node;

/**
 * Node event type definitions.
 * Corresponds to TypeScript's server-node-events-types.ts.
 */
public final class NodeEventTypes {

    private NodeEventTypes() {}

    /**
     * An event sent from a node (or to a node).
     */
    public record NodeEvent(String event, String payloadJSON) {
        public NodeEvent {
            if (event == null || event.isBlank()) {
                throw new IllegalArgumentException("event name required");
            }
        }
    }

    /**
     * Context interface for node event handling — provides the callbacks
     * needed to process node-originated events (subscribe, broadcast, etc.).
     *
     * <p>Implementations are typically created in the gateway layer where
     * all the wiring (session store, broadcaster, etc.) is available.</p>
     */
    public interface NodeEventContext {
        /** Broadcast an event to all connected WebSocket clients. */
        void broadcast(String event, Object payload);

        /** Send an event to nodes subscribed to a specific session. */
        void sendToSession(String sessionKey, String event, Object payload);

        /** Subscribe a node to a session's events. */
        void subscribe(String nodeId, String sessionKey);

        /** Unsubscribe a node from a session's events. */
        void unsubscribe(String nodeId, String sessionKey);
    }
}
