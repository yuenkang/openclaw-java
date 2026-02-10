package com.openclaw.gateway.websocket;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Routes JSON-RPC method calls to registered handlers.
 * Corresponds to TypeScript's server-methods/ dispatch pattern.
 */
@Slf4j
public class GatewayMethodRouter {

    @FunctionalInterface
    public interface MethodHandler {
        CompletableFuture<Object> handle(JsonNode params, GatewayConnection connection);
    }

    @FunctionalInterface
    public interface NotificationHandler {
        void handle(JsonNode params, GatewayConnection connection);
    }

    private final Map<String, MethodHandler> methodHandlers = new ConcurrentHashMap<>();
    private final Map<String, NotificationHandler> notificationHandlers = new ConcurrentHashMap<>();

    /**
     * Register a handler for a JSON-RPC method (expects response).
     */
    public void registerMethod(String method, MethodHandler handler) {
        methodHandlers.put(method, handler);
        log.debug("Registered method handler: {}", method);
    }

    /**
     * Register a handler for a JSON-RPC notification (no response).
     */
    public void registerNotification(String method, NotificationHandler handler) {
        notificationHandlers.put(method, handler);
        log.debug("Registered notification handler: {}", method);
    }

    /**
     * Dispatch a JSON-RPC request to the appropriate handler.
     */
    public CompletableFuture<Object> dispatch(String method, JsonNode params, GatewayConnection connection) {
        MethodHandler handler = methodHandlers.get(method);
        if (handler == null) {
            return CompletableFuture.failedFuture(
                    new UnsupportedOperationException("Method not found: " + method));
        }

        try {
            return handler.handle(params, connection);
        } catch (Exception e) {
            return CompletableFuture.failedFuture(e);
        }
    }

    /**
     * Handle a JSON-RPC notification.
     */
    public void handleNotification(String method, JsonNode params, GatewayConnection connection) {
        NotificationHandler handler = notificationHandlers.get(method);
        if (handler != null) {
            try {
                handler.handle(params, connection);
            } catch (Exception e) {
                log.error("Notification handler failed for {}: {}", method, e.getMessage(), e);
            }
        } else {
            log.debug("No handler for notification: {}", method);
        }
    }

    /**
     * List registered method names.
     */
    public java.util.Set<String> getRegisteredMethods() {
        return java.util.Collections.unmodifiableSet(methodHandlers.keySet());
    }
}
