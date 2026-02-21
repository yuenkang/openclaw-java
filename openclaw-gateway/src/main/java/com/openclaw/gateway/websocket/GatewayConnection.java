package com.openclaw.gateway.websocket;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.gateway.protocol.ProtocolTypes.*;
import com.openclaw.node.NodeConnection;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Represents a single WebSocket connection to the Gateway.
 * Aligns with TypeScript GatewayWsClient + handshake state.
 */
@Slf4j
@Getter
public class GatewayConnection implements NodeConnection {

    private final String connectionId;
    private final WebSocketSession session;
    private final long connectedAt;
    private final AtomicLong requestCounter = new AtomicLong(0);
    private final Map<String, CompletableFuture<JsonNode>> pendingResponses = new ConcurrentHashMap<>();

    /** Nonce sent in connect.challenge, must match in connect request. */
    private final String connectNonce;

    /** Handshake state: PENDING → CONNECTED or FAILED. */
    private volatile HandshakeState handshakeState = HandshakeState.PENDING;

    /** Connect params from client (set after successful handshake). */
    private volatile ConnectParams connectParams;

    /** Connection role: "operator" or "node". */
    private volatile String role;

    /**
     * Permission scopes: ["operator.admin"], ["operator.read", "operator.write"],
     * etc.
     */
    private volatile List<String> scopes;

    /** Auth method used (e.g. "none", "local", "token", "password"). */
    private volatile String authMethod;

    /** Remote client IP. */
    private volatile String clientIp;

    public GatewayConnection(String connectionId, WebSocketSession session) {
        this.connectionId = connectionId;
        this.session = session;
        this.connectedAt = System.currentTimeMillis();
        this.connectNonce = UUID.randomUUID().toString();
    }

    @Override
    public String getClientId() {
        if (connectParams != null && connectParams.getClient() != null) {
            return connectParams.getClient().getId();
        }
        return null;
    }

    /**
     * Complete the connect handshake.
     */
    public void completeHandshake(ConnectParams params, String authMethod) {
        this.connectParams = params;
        this.role = params.getRole() != null ? params.getRole() : "operator";
        this.scopes = params.getScopes();
        this.authMethod = authMethod;
        this.handshakeState = HandshakeState.CONNECTED;
    }

    /**
     * Mark handshake as failed.
     */
    public void failHandshake() {
        this.handshakeState = HandshakeState.FAILED;
    }

    public boolean isConnected() {
        return handshakeState == HandshakeState.CONNECTED;
    }

    public boolean isPending() {
        return handshakeState == HandshakeState.PENDING;
    }

    /**
     * Send a TS protocol response frame.
     */
    public void sendResponse(ResponseFrame response, ObjectMapper mapper) {
        sendJson(response, mapper);
    }

    /**
     * Send a TS protocol event frame.
     */
    public void sendEvent(EventFrame event, ObjectMapper mapper) {
        sendJson(event, mapper);
    }

    /**
     * Send a TS protocol request to the client and await response.
     * Uses the {type:"req", id, method, params} format.
     */
    public CompletableFuture<JsonNode> sendRequest(String method, Object params, ObjectMapper mapper) {
        String id = String.valueOf(requestCounter.incrementAndGet());
        CompletableFuture<JsonNode> future = new CompletableFuture<>();
        pendingResponses.put(id, future);

        try {
            var request = new RequestFrame("req", id, method, params);
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
     * Send a push event to the client (no response expected).
     */
    public void sendEvent(String event, Object payload, ObjectMapper mapper) {
        sendJson(EventFrame.of(event, payload), mapper);
    }

    /**
     * Resolve a pending response future from a response frame.
     */
    void resolveResponse(String id, JsonNode response) {
        CompletableFuture<JsonNode> future = pendingResponses.remove(id);
        if (future != null) {
            if (response.has("ok") && !response.get("ok").asBoolean()) {
                JsonNode error = response.get("error");
                String message = error != null && error.has("message")
                        ? error.get("message").asText()
                        : "unknown error";
                future.completeExceptionally(new RuntimeException(message));
            } else {
                future.complete(response.get("payload"));
            }
        }
    }

    private void sendJson(Object obj, ObjectMapper mapper) {
        try {
            String json = mapper.writeValueAsString(obj);
            if (session.isOpen()) {
                session.sendMessage(new TextMessage(json));
            }
        } catch (IOException e) {
            log.error("Failed to send to {}: {}", connectionId, e.getMessage());
        }
    }
}
