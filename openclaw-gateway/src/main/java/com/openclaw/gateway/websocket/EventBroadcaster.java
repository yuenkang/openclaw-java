package com.openclaw.gateway.websocket;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.gateway.protocol.ProtocolTypes.EventFrame;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Broadcasts events to connected WebSocket clients.
 * Corresponds to TypeScript's server-broadcast.ts.
 */
@Slf4j
public class EventBroadcaster {

    private final Map<String, GatewayConnection> connections = new ConcurrentHashMap<>();
    private final ObjectMapper objectMapper;

    public EventBroadcaster(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    /**
     * Register a connection for broadcasting.
     */
    public void addConnection(GatewayConnection connection) {
        connections.put(connection.getConnectionId(), connection);
    }

    /**
     * Remove a connection from broadcasting.
     */
    public void removeConnection(String connectionId) {
        connections.remove(connectionId);
    }

    /**
     * Broadcast an event to ALL connected (handshake-completed) clients.
     */
    public void broadcastToAll(String event, Object payload) {
        var frame = EventFrame.of(event, payload);
        for (GatewayConnection conn : connections.values()) {
            if (conn.isConnected()) {
                try {
                    conn.sendEvent(frame, objectMapper);
                } catch (Exception e) {
                    log.warn("Failed to broadcast event {} to {}: {}", event, conn.getConnectionId(), e.getMessage());
                }
            }
        }
    }

    /**
     * Send an event to a specific connection by ID.
     */
    public void sendToConnection(String connectionId, String event, Object payload) {
        GatewayConnection conn = connections.get(connectionId);
        if (conn == null || !conn.isConnected()) {
            log.debug("Connection {} not found or not connected for event {}", connectionId, event);
            return;
        }
        try {
            conn.sendEvent(EventFrame.of(event, payload), objectMapper);
        } catch (Exception e) {
            log.warn("Failed to send event {} to {}: {}", event, connectionId, e.getMessage());
        }
    }

    /**
     * Send an event to multiple specific connections.
     */
    public void sendToConnections(Iterable<String> connectionIds, String event, Object payload) {
        var frame = EventFrame.of(event, payload);
        for (String connId : connectionIds) {
            GatewayConnection conn = connections.get(connId);
            if (conn != null && conn.isConnected()) {
                try {
                    conn.sendEvent(frame, objectMapper);
                } catch (Exception e) {
                    log.warn("Failed to send event {} to {}: {}", event, connId, e.getMessage());
                }
            }
        }
    }

    /**
     * Get count of connected clients.
     */
    public int getConnectedCount() {
        return (int) connections.values().stream()
                .filter(GatewayConnection::isConnected)
                .count();
    }
}
