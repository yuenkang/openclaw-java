package com.openclaw.gateway.node;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.gateway.websocket.GatewayConnection;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.*;

/**
 * Manages connected node sessions and remote command invocation.
 * Corresponds to TypeScript's node-registry.ts.
 */
@Slf4j
public class NodeRegistry {

    private final Map<String, NodeSession> nodesById = new ConcurrentHashMap<>();
    private final Map<String, String> nodesByConn = new ConcurrentHashMap<>();
    private final Map<String, PendingInvoke> pendingInvokes = new ConcurrentHashMap<>();
    private final ObjectMapper objectMapper;
    private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor(r -> {
        Thread t = new Thread(r, "node-invoke-timeout");
        t.setDaemon(true);
        return t;
    });

    public NodeRegistry(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    /**
     * Register a connected node session.
     */
    public NodeSession register(GatewayConnection connection, Map<String, Object> metadata) {
        String connId = connection.getConnectionId();
        var params = connection.getConnectParams();
        String clientId = (params != null && params.getClient() != null)
                ? params.getClient().getId()
                : null;
        String nodeId = metadata.getOrDefault("nodeId",
                clientId != null ? clientId : connId).toString();

        NodeSession session = NodeSession.builder()
                .nodeId(nodeId)
                .connId(connId)
                .connection(connection)
                .displayName((String) metadata.get("displayName"))
                .platform((String) metadata.get("platform"))
                .version((String) metadata.get("version"))
                .coreVersion((String) metadata.get("coreVersion"))
                .uiVersion((String) metadata.get("uiVersion"))
                .deviceFamily((String) metadata.get("deviceFamily"))
                .modelIdentifier((String) metadata.get("modelIdentifier"))
                .remoteIp(connection.getClientIp())
                .caps(toStringList(metadata.get("caps")))
                .commands(toStringList(metadata.get("commands")))
                .connectedAtMs(System.currentTimeMillis())
                .build();

        nodesById.put(nodeId, session);
        nodesByConn.put(connId, nodeId);
        log.info("Node registered: nodeId={} connId={}", nodeId, connId);
        return session;
    }

    /**
     * Unregister a node by connection ID.
     */
    public String unregister(String connId) {
        String nodeId = nodesByConn.remove(connId);
        if (nodeId == null)
            return null;

        nodesById.remove(nodeId);

        // Cancel pending invokes for this node
        var it = pendingInvokes.entrySet().iterator();
        while (it.hasNext()) {
            var entry = it.next();
            PendingInvoke pending = entry.getValue();
            if (nodeId.equals(pending.nodeId)) {
                pending.future.completeExceptionally(
                        new RuntimeException("node disconnected (" + pending.command + ")"));
                if (pending.timeoutTask != null)
                    pending.timeoutTask.cancel(false);
                it.remove();
            }
        }
        log.info("Node unregistered: nodeId={} connId={}", nodeId, connId);
        return nodeId;
    }

    public List<NodeSession> listConnected() {
        return new ArrayList<>(nodesById.values());
    }

    public NodeSession get(String nodeId) {
        return nodesById.get(nodeId);
    }

    /**
     * Send an invoke request to a node and await the result.
     */
    public CompletableFuture<NodeInvokeResult> invoke(String nodeId, String command,
            Object params, Long timeoutMs,
            String idempotencyKey) {
        NodeSession node = nodesById.get(nodeId);
        if (node == null) {
            return CompletableFuture.completedFuture(
                    NodeInvokeResult.error("NOT_CONNECTED", "node not connected"));
        }

        String requestId = UUID.randomUUID().toString();
        CompletableFuture<NodeInvokeResult> future = new CompletableFuture<>();

        long timeout = timeoutMs != null ? timeoutMs : 30_000L;
        ScheduledFuture<?> timeoutTask = scheduler.schedule(() -> {
            PendingInvoke removed = pendingInvokes.remove(requestId);
            if (removed != null) {
                removed.future.complete(NodeInvokeResult.error("TIMEOUT", "node invoke timed out"));
            }
        }, timeout, TimeUnit.MILLISECONDS);

        pendingInvokes.put(requestId, PendingInvoke.builder()
                .nodeId(nodeId)
                .command(command)
                .future(future)
                .timeoutTask(timeoutTask)
                .build());

        // Send invoke request event to node
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("id", requestId);
        payload.put("nodeId", nodeId);
        payload.put("command", command);
        if (params != null) {
            try {
                payload.put("paramsJSON", objectMapper.writeValueAsString(params));
            } catch (Exception e) {
                payload.put("paramsJSON", null);
            }
        }
        payload.put("timeoutMs", timeout);
        if (idempotencyKey != null)
            payload.put("idempotencyKey", idempotencyKey);

        try {
            node.getConnection().sendEvent("node.invoke.request", payload, objectMapper);
        } catch (Exception e) {
            pendingInvokes.remove(requestId);
            timeoutTask.cancel(false);
            return CompletableFuture.completedFuture(
                    NodeInvokeResult.error("UNAVAILABLE", "failed to send invoke to node"));
        }

        return future;
    }

    /**
     * Handle invoke result from a node.
     */
    public boolean handleInvokeResult(String id, String nodeId, boolean ok,
            Object payload, String payloadJSON,
            Map<String, String> error) {
        PendingInvoke pending = pendingInvokes.remove(id);
        if (pending == null)
            return false;
        if (!nodeId.equals(pending.nodeId))
            return false;

        if (pending.timeoutTask != null)
            pending.timeoutTask.cancel(false);

        NodeInvokeResult result = ok
                ? NodeInvokeResult.success(payload, payloadJSON)
                : NodeInvokeResult.error(
                        error != null ? error.get("code") : null,
                        error != null ? error.get("message") : "unknown error");
        pending.future.complete(result);
        return true;
    }

    /**
     * Send an event to a specific node.
     */
    public boolean sendEvent(String nodeId, String event, Object payload) {
        NodeSession node = nodesById.get(nodeId);
        if (node == null)
            return false;
        try {
            node.getConnection().sendEvent(event, payload, objectMapper);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    // --- Helper ---

    @SuppressWarnings("unchecked")
    private List<String> toStringList(Object value) {
        if (value instanceof List<?> list) {
            return list.stream().map(String::valueOf).toList();
        }
        return List.of();
    }

    // --- Inner types ---

    @Data
    @Builder
    public static class NodeSession {
        private final String nodeId;
        private final String connId;
        private final GatewayConnection connection;
        private String displayName;
        private String platform;
        private String version;
        private String coreVersion;
        private String uiVersion;
        private String deviceFamily;
        private String modelIdentifier;
        private String remoteIp;
        private List<String> caps;
        private List<String> commands;
        private Map<String, Boolean> permissions;
        private String pathEnv;
        private long connectedAtMs;
    }

    @Data
    @Builder
    private static class PendingInvoke {
        private final String nodeId;
        private final String command;
        private final CompletableFuture<NodeInvokeResult> future;
        private final ScheduledFuture<?> timeoutTask;
    }

    @Data
    public static class NodeInvokeResult {
        private final boolean ok;
        private final Object payload;
        private final String payloadJSON;
        private final String errorCode;
        private final String errorMessage;

        public static NodeInvokeResult success(Object payload, String payloadJSON) {
            return new NodeInvokeResult(true, payload, payloadJSON, null, null);
        }

        public static NodeInvokeResult error(String code, String message) {
            return new NodeInvokeResult(false, null, null, code, message);
        }
    }
}
