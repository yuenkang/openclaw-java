package com.openclaw.gateway.runtime;

import com.openclaw.gateway.websocket.GatewayMethodRouter;
import lombok.Getter;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Enumerates all registered RPC methods for the gateway.
 * Corresponds to TS {@code gateway/server-methods-list.ts}.
 *
 * <p>
 * Used by the "methods.list" RPC call so clients can discover
 * available methods.
 */
public class MethodsList {

    /**
     * Description of a single RPC method.
     */
    @Getter
    public static class MethodInfo {
        private final String method;
        private final String description;
        private final String group;

        public MethodInfo(String method, String description, String group) {
            this.method = method;
            this.description = description;
            this.group = group;
        }
    }

    /**
     * Collect all registered methods from the router.
     */
    public static List<MethodInfo> listMethods(GatewayMethodRouter router) {
        if (router == null)
            return Collections.emptyList();

        return router.getRegisteredMethods().stream()
                .map(method -> new MethodInfo(
                        method,
                        describeMethod(method),
                        groupForMethod(method)))
                .sorted((a, b) -> a.method.compareTo(b.method))
                .collect(Collectors.toList());
    }

    /**
     * Build the payload for the methods.list RPC response.
     */
    public static List<Map<String, String>> buildMethodsListPayload(
            GatewayMethodRouter router) {
        return listMethods(router).stream()
                .map(m -> Map.of(
                        "method", m.method,
                        "description", m.description,
                        "group", m.group))
                .collect(Collectors.toList());
    }

    // ---- helpers ----

    private static String groupForMethod(String method) {
        int dot = method.indexOf('.');
        return dot > 0 ? method.substring(0, dot) : "core";
    }

    private static String describeMethod(String method) {
        return switch (method) {
            case "status" -> "Get gateway status";
            case "connect" -> "WebSocket handshake";
            case "config.get" -> "Get current configuration";
            case "config.set" -> "Update configuration";
            case "session.create" -> "Create a new session";
            case "sessions.list" -> "List all sessions";
            case "session.delete" -> "Delete a session";
            case "agent.run" -> "Run an agent turn";
            case "agent.message" -> "Send a message to an agent";
            case "agent.abort" -> "Abort an active agent run";
            case "cron.list" -> "List cron jobs";
            case "cron.create" -> "Create a cron job";
            case "cron.delete" -> "Delete a cron job";
            case "channels.list" -> "List channel statuses";
            case "channels.start" -> "Start a channel";
            case "channels.stop" -> "Stop a channel";
            case "methods.list" -> "List available RPC methods";
            default -> method;
        };
    }
}
