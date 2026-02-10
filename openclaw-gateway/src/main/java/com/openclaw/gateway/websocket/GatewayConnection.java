package com.openclaw.gateway.websocket;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Represents a single WebSocket connection to the Gateway.
 * Tracks connection metadata and pending RPC responses.
 */
@Slf4j
@Getter
public class GatewayConnection {

    private final String connectionId;
    private final WebSocketSession session;
    private final long connectedAt;
    private final AtomicLong requestCounter = new AtomicLong(0);
    private final Map<String, CompletableFuture<JsonNode>> pendingResponses = new ConcurrentHashMap<>();

    /** Authenticated device ID (set after pairing). */
    private volatile String deviceId;
    /** Connection role (e.g. "admin", "user"). */
    private volatile String role;

    public GatewayConnection(String connectionId, WebSocketSession session) {
        this.connectionId = connectionId;
        this.session = session;
        this.connectedAt = System.currentTimeMillis();
    }

    /**
     * Set authentication info after pairing.
     */
    public void authenticate(String deviceId, String role) {
        this.deviceId = deviceId;
        this.role = role;
    }

    public boolean isAuthenticated() {
        return deviceId != null;
    }

    /**
     * Send a JSON-RPC request to the client and await response.
     */
    public CompletableFuture<JsonNode> sendRequest(String method, Object params, ObjectMapper mapper) {
        String id = String.valueOf(requestCounter.incrementAndGet());
        CompletableFuture<JsonNode> future = new CompletableFuture<>();
        pendingResponses.put(id, future);

        try {
            Map<String, Object> request = Map.of(
                    "jsonrpc", "2.0",
                    "method", method,
                    "params", params != null ? params : Map.of(),
                    "id", id);
            String json = mapper.writeValueAsString(request);
            if (session.isOpen()) {
                session.sendMessage(new TextMessage(json));
            } else {
                future.completeExceptionally(new IOException("Session closed"));
                pendingResponses.remove(id);
            }
        } catch (IOException e) {
            future.completeExceptionally(e);
            pendingResponses.remove(id);
        }

        return future;
    }

    /**
     * Send a notification (no response expected).
     */
    public void sendNotification(String method, Object params, ObjectMapper mapper) {
        try {
            Map<String, Object> notification = Map.of(
                    "jsonrpc", "2.0",
                    "method", method,
                    "params", params != null ? params : Map.of());
            String json = mapper.writeValueAsString(notification);
            if (session.isOpen()) {
                session.sendMessage(new TextMessage(json));
            }
        } catch (IOException e) {
            log.error("Failed to send notification to {}: {}", connectionId, e.getMessage());
        }
    }

    /**
     * Resolve a pending response future.
     */
    void resolveResponse(String id, JsonNode response) {
        CompletableFuture<JsonNode> future = pendingResponses.remove(id);
        if (future != null) {
            if (response.has("error")) {
                future.completeExceptionally(
                        new RuntimeException(response.get("error").get("message").asText()));
            } else {
                future.complete(response.get("result"));
            }
        }
    }
}
