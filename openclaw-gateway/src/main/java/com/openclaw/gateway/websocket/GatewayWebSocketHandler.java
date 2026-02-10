package com.openclaw.gateway.websocket;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.common.model.JsonRpcMessage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.socket.*;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Gateway WebSocket handler.
 * Corresponds to TypeScript's server-ws.ts.
 *
 * <p>
 * Manages WebSocket connections and dispatches JSON-RPC messages
 * to registered method handlers.
 * </p>
 */
@Slf4j
public class GatewayWebSocketHandler extends TextWebSocketHandler {

    private final ObjectMapper objectMapper;
    private final GatewayMethodRouter methodRouter;
    private final Map<String, GatewayConnection> connections = new ConcurrentHashMap<>();

    public GatewayWebSocketHandler(ObjectMapper objectMapper, GatewayMethodRouter methodRouter) {
        this.objectMapper = objectMapper;
        this.methodRouter = methodRouter;
    }

    @Override
    public void afterConnectionEstablished(WebSocketSession session) {
        String connId = session.getId();
        GatewayConnection connection = new GatewayConnection(connId, session);
        connections.put(connId, connection);
        log.info("WebSocket connected: {} (remote={})", connId, session.getRemoteAddress());
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) {
        String connId = session.getId();
        String payload = message.getPayload();

        try {
            JsonNode node = objectMapper.readTree(payload);

            // Determine message type
            if (node.has("method")) {
                if (node.has("id")) {
                    // JSON-RPC Request
                    handleRequest(session, connId, node);
                } else {
                    // JSON-RPC Notification
                    handleNotification(connId, node);
                }
            } else if (node.has("result") || node.has("error")) {
                // JSON-RPC Response (from client)
                handleResponse(connId, node);
            } else {
                sendError(session, null, JsonRpcMessage.INVALID_REQUEST, "Invalid JSON-RPC message");
            }

        } catch (JsonProcessingException e) {
            log.warn("Failed to parse message from {}: {}", connId, e.getMessage());
            sendError(session, null, JsonRpcMessage.PARSE_ERROR, "Parse error");
        }
    }

    private void handleRequest(WebSocketSession session, String connId, JsonNode node) {
        String method = node.get("method").asText();
        String id = node.has("id") ? node.get("id").asText() : null;
        JsonNode params = node.get("params");

        log.debug("RPC request: method={}, id={}, conn={}", method, id, connId);

        GatewayConnection connection = connections.get(connId);
        if (connection == null) {
            sendError(session, id, JsonRpcMessage.INTERNAL_ERROR, "Connection not found");
            return;
        }

        // Dispatch to method router
        methodRouter.dispatch(method, params, connection)
                .thenAccept(result -> {
                    JsonRpcMessage.Response response = JsonRpcMessage.Response.success(id, result);
                    sendJson(session, response);
                })
                .exceptionally(ex -> {
                    Throwable cause = ex.getCause() != null ? ex.getCause() : ex;
                    int errorCode;
                    if (cause instanceof UnsupportedOperationException) {
                        errorCode = JsonRpcMessage.METHOD_NOT_FOUND;
                        log.debug("Method not found: {}", method);
                    } else {
                        errorCode = JsonRpcMessage.INTERNAL_ERROR;
                        log.error("Method {} failed: {}", method, cause.getMessage(), cause);
                    }
                    sendError(session, id, errorCode, cause.getMessage());
                    return null;
                });
    }

    private void handleNotification(String connId, JsonNode node) {
        String method = node.get("method").asText();
        JsonNode params = node.get("params");
        log.debug("RPC notification: method={}, conn={}", method, connId);

        GatewayConnection connection = connections.get(connId);
        if (connection != null) {
            methodRouter.handleNotification(method, params, connection);
        }
    }

    private void handleResponse(String connId, JsonNode node) {
        String id = node.has("id") ? node.get("id").asText() : null;
        log.debug("RPC response: id={}, conn={}", id, connId);

        GatewayConnection connection = connections.get(connId);
        if (connection != null) {
            connection.resolveResponse(id, node);
        }
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) {
        String connId = session.getId();
        connections.remove(connId);
        log.info("WebSocket disconnected: {} (status={})", connId, status);
    }

    @Override
    public void handleTransportError(WebSocketSession session, Throwable exception) {
        String connId = session.getId();
        log.error("WebSocket transport error on {}: {}", connId, exception.getMessage());
    }

    // --- Broadcast ---

    /**
     * Broadcast a notification to all connected clients.
     */
    public void broadcast(String method, Object params) {
        JsonRpcMessage.Notification notification = JsonRpcMessage.Notification.create(method, params);
        connections.values().forEach(conn -> sendJson(conn.getSession(), notification));
    }

    /**
     * Number of active connections.
     */
    public int getConnectionCount() {
        return connections.size();
    }

    // --- Helpers ---

    private void sendJson(WebSocketSession session, Object message) {
        try {
            String json = objectMapper.writeValueAsString(message);
            if (session.isOpen()) {
                session.sendMessage(new TextMessage(json));
            }
        } catch (IOException e) {
            log.error("Failed to send message: {}", e.getMessage());
        }
    }

    private void sendError(WebSocketSession session, String id, int code, String message) {
        sendJson(session, JsonRpcMessage.Response.error(id, code, message));
    }
}
