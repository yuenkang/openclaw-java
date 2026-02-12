package com.openclaw.gateway.methods;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.gateway.node.DevicePairingService;
import com.openclaw.gateway.node.NodePairingService;
import com.openclaw.gateway.node.NodeRegistry;
import com.openclaw.gateway.websocket.EventBroadcaster;
import com.openclaw.gateway.websocket.GatewayConnection;
import com.openclaw.gateway.websocket.GatewayMethodRouter;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

/**
 * Registers node.* and device.* RPC method handlers.
 * Corresponds to TypeScript's server-methods/nodes.ts and devices.ts.
 *
 * Methods: node.pair.request, node.pair.list, node.pair.approve,
 * node.pair.reject,
 * node.pair.verify, node.rename, node.list, node.describe, node.invoke,
 * node.invoke.result, node.event,
 * device.pair.list, device.pair.approve, device.pair.reject,
 * device.token.rotate, device.token.revoke
 */
@Slf4j
public class NodeDeviceMethodRegistrar {

    private final GatewayMethodRouter router;
    private final NodeRegistry nodeRegistry;
    private final NodePairingService nodePairingService;
    private final DevicePairingService devicePairingService;
    private final EventBroadcaster broadcaster;
    private final ObjectMapper objectMapper;

    public NodeDeviceMethodRegistrar(GatewayMethodRouter router,
            NodeRegistry nodeRegistry,
            NodePairingService nodePairingService,
            DevicePairingService devicePairingService,
            EventBroadcaster broadcaster,
            ObjectMapper objectMapper) {
        this.router = router;
        this.nodeRegistry = nodeRegistry;
        this.nodePairingService = nodePairingService;
        this.devicePairingService = devicePairingService;
        this.broadcaster = broadcaster;
        this.objectMapper = objectMapper;
    }

    public void registerMethods() {
        // --- Node pairing ---
        router.registerMethod("node.pair.request", this::handleNodePairRequest);
        router.registerMethod("node.pair.list", this::handleNodePairList);
        router.registerMethod("node.pair.approve", this::handleNodePairApprove);
        router.registerMethod("node.pair.reject", this::handleNodePairReject);
        router.registerMethod("node.pair.verify", this::handleNodePairVerify);
        router.registerMethod("node.rename", this::handleNodeRename);

        // --- Node operations ---
        router.registerMethod("node.list", this::handleNodeList);
        router.registerMethod("node.describe", this::handleNodeDescribe);
        router.registerMethod("node.invoke", this::handleNodeInvoke);
        router.registerMethod("node.invoke.result", this::handleNodeInvokeResult);
        router.registerMethod("node.event", this::handleNodeEvent);

        // --- Device pairing ---
        router.registerMethod("device.pair.list", this::handleDevicePairList);
        router.registerMethod("device.pair.approve", this::handleDevicePairApprove);
        router.registerMethod("device.pair.reject", this::handleDevicePairReject);
        router.registerMethod("device.token.rotate", this::handleDeviceTokenRotate);
        router.registerMethod("device.token.revoke", this::handleDeviceTokenRevoke);

        log.info("Registered 16 node/device methods");
    }

    // ==================== Node Pairing ====================

    private CompletableFuture<Object> handleNodePairRequest(JsonNode params, GatewayConnection conn) {
        String nodeId = textOrNull(params, "nodeId");
        if (nodeId == null || nodeId.isBlank()) {
            return failedResult("nodeId required");
        }

        var req = NodePairingService.PairingRequest.builder()
                .nodeId(nodeId)
                .displayName(textOrNull(params, "displayName"))
                .platform(textOrNull(params, "platform"))
                .version(textOrNull(params, "version"))
                .coreVersion(textOrNull(params, "coreVersion"))
                .uiVersion(textOrNull(params, "uiVersion"))
                .deviceFamily(textOrNull(params, "deviceFamily"))
                .modelIdentifier(textOrNull(params, "modelIdentifier"))
                .caps(stringList(params, "caps"))
                .commands(stringList(params, "commands"))
                .remoteIp(textOrNull(params, "remoteIp"))
                .silent(params.path("silent").asBoolean(false))
                .build();

        var result = nodePairingService.requestPairing(req);

        if (result.isCreated() && result.getRequest() != null) {
            broadcaster.broadcastToAll("node.pair.requested", result.getRequest());
        }

        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleNodePairList(JsonNode params, GatewayConnection conn) {
        return CompletableFuture.completedFuture(nodePairingService.listPairings());
    }

    private CompletableFuture<Object> handleNodePairApprove(JsonNode params, GatewayConnection conn) {
        String requestId = textOrNull(params, "requestId");
        if (requestId == null)
            return failedResult("requestId required");

        var result = nodePairingService.approvePairing(requestId);
        if (result == null)
            return failedResult("unknown requestId");

        broadcaster.broadcastToAll("node.pair.resolved", Map.of(
                "requestId", requestId,
                "nodeId", result.getNode().getNodeId(),
                "decision", "approved",
                "ts", System.currentTimeMillis()));

        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleNodePairReject(JsonNode params, GatewayConnection conn) {
        String requestId = textOrNull(params, "requestId");
        if (requestId == null)
            return failedResult("requestId required");

        var result = nodePairingService.rejectPairing(requestId);
        if (result == null)
            return failedResult("unknown requestId");

        broadcaster.broadcastToAll("node.pair.resolved", Map.of(
                "requestId", requestId,
                "nodeId", result.getNodeId(),
                "decision", "rejected",
                "ts", System.currentTimeMillis()));

        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleNodePairVerify(JsonNode params, GatewayConnection conn) {
        String nodeId = textOrNull(params, "nodeId");
        String token = textOrNull(params, "token");
        if (nodeId == null || token == null)
            return failedResult("nodeId and token required");

        return CompletableFuture.completedFuture(nodePairingService.verifyToken(nodeId, token));
    }

    private CompletableFuture<Object> handleNodeRename(JsonNode params, GatewayConnection conn) {
        String nodeId = textOrNull(params, "nodeId");
        String displayName = textOrNull(params, "displayName");
        if (nodeId == null || displayName == null || displayName.isBlank()) {
            return failedResult("nodeId and displayName required");
        }

        var updated = nodePairingService.renamePairedNode(nodeId, displayName.trim());
        if (updated == null)
            return failedResult("unknown nodeId");

        return CompletableFuture.completedFuture(Map.of(
                "nodeId", updated.getNodeId(),
                "displayName", updated.getDisplayName()));
    }

    // ==================== Node Operations ====================

    private CompletableFuture<Object> handleNodeList(JsonNode params, GatewayConnection conn) {
        var deviceList = devicePairingService.listPairings();
        var pairedById = new LinkedHashMap<String, Map<String, Object>>();

        for (var device : deviceList.getPaired()) {
            if (isNodeRole(device)) {
                Map<String, Object> nodeInfo = new LinkedHashMap<>();
                nodeInfo.put("nodeId", device.getDeviceId());
                nodeInfo.put("displayName", device.getDisplayName());
                nodeInfo.put("platform", device.getPlatform());
                nodeInfo.put("caps", List.of());
                nodeInfo.put("commands", List.of());
                pairedById.put(device.getDeviceId(), nodeInfo);
            }
        }

        var connected = nodeRegistry.listConnected();
        var connectedById = new LinkedHashMap<String, NodeRegistry.NodeSession>();
        for (var n : connected) {
            connectedById.put(n.getNodeId(), n);
        }

        Set<String> allIds = new LinkedHashSet<>();
        allIds.addAll(pairedById.keySet());
        allIds.addAll(connectedById.keySet());

        List<Map<String, Object>> nodes = new ArrayList<>();
        for (String nodeId : allIds) {
            var paired = pairedById.get(nodeId);
            var live = connectedById.get(nodeId);

            Map<String, Object> node = new LinkedHashMap<>();
            node.put("nodeId", nodeId);
            node.put("displayName", live != null ? live.getDisplayName()
                    : (paired != null ? paired.get("displayName") : null));
            node.put("platform", live != null ? live.getPlatform()
                    : (paired != null ? paired.get("platform") : null));
            node.put("version", live != null ? live.getVersion() : null);
            node.put("coreVersion", live != null ? live.getCoreVersion() : null);
            node.put("uiVersion", live != null ? live.getUiVersion() : null);
            node.put("deviceFamily", live != null ? live.getDeviceFamily() : null);
            node.put("modelIdentifier", live != null ? live.getModelIdentifier() : null);
            node.put("remoteIp", live != null ? live.getRemoteIp()
                    : (paired != null ? paired.get("remoteIp") : null));
            node.put("caps", live != null ? live.getCaps() : List.of());
            node.put("commands", live != null ? live.getCommands() : List.of());
            node.put("pathEnv", live != null ? live.getPathEnv() : null);
            node.put("permissions", live != null ? live.getPermissions() : null);
            node.put("connectedAtMs", live != null ? live.getConnectedAtMs() : null);
            node.put("paired", paired != null);
            node.put("connected", live != null);
            nodes.add(node);
        }

        // Sort: connected first, then by displayName
        nodes.sort((a, b) -> {
            boolean ac = Boolean.TRUE.equals(a.get("connected"));
            boolean bc = Boolean.TRUE.equals(b.get("connected"));
            if (ac != bc)
                return ac ? -1 : 1;
            String an = String.valueOf(a.getOrDefault("displayName", a.get("nodeId"))).toLowerCase();
            String bn = String.valueOf(b.getOrDefault("displayName", b.get("nodeId"))).toLowerCase();
            return an.compareTo(bn);
        });

        return CompletableFuture.completedFuture(Map.of(
                "ts", System.currentTimeMillis(),
                "nodes", nodes));
    }

    private CompletableFuture<Object> handleNodeDescribe(JsonNode params, GatewayConnection conn) {
        String nodeId = textOrNull(params, "nodeId");
        if (nodeId == null || nodeId.isBlank())
            return failedResult("nodeId required");

        var deviceList = devicePairingService.listPairings();
        DevicePairingService.PairedDevice paired = null;
        for (var d : deviceList.getPaired()) {
            if (d.getDeviceId().equals(nodeId) && isNodeRole(d)) {
                paired = d;
                break;
            }
        }

        var live = nodeRegistry.get(nodeId);
        if (paired == null && live == null)
            return failedResult("unknown nodeId");

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ts", System.currentTimeMillis());
        result.put("nodeId", nodeId);
        result.put("displayName", live != null ? live.getDisplayName()
                : (paired != null ? paired.getDisplayName() : null));
        result.put("platform", live != null ? live.getPlatform()
                : (paired != null ? paired.getPlatform() : null));
        result.put("version", live != null ? live.getVersion() : null);
        result.put("coreVersion", live != null ? live.getCoreVersion() : null);
        result.put("uiVersion", live != null ? live.getUiVersion() : null);
        result.put("deviceFamily", live != null ? live.getDeviceFamily() : null);
        result.put("modelIdentifier", live != null ? live.getModelIdentifier() : null);
        result.put("remoteIp", live != null ? live.getRemoteIp()
                : (paired != null ? paired.getRemoteIp() : null));
        result.put("caps", live != null ? live.getCaps() : List.of());
        result.put("commands", live != null ? live.getCommands() : List.of());
        result.put("pathEnv", live != null ? live.getPathEnv() : null);
        result.put("permissions", live != null ? live.getPermissions() : null);
        result.put("connectedAtMs", live != null ? live.getConnectedAtMs() : null);
        result.put("paired", paired != null);
        result.put("connected", live != null);

        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleNodeInvoke(JsonNode params, GatewayConnection conn) {
        String nodeId = textOrNull(params, "nodeId");
        String command = textOrNull(params, "command");
        if (nodeId == null || command == null || nodeId.isBlank() || command.isBlank()) {
            return failedResult("nodeId and command required");
        }

        var node = nodeRegistry.get(nodeId);
        if (node == null)
            return failedResult("node not connected");

        Object invokeParams = null;
        if (params.has("params")) {
            invokeParams = objectMapper.convertValue(params.get("params"), Object.class);
        }
        Long timeoutMs = params.has("timeoutMs") ? params.get("timeoutMs").asLong() : null;
        String idempotencyKey = textOrNull(params, "idempotencyKey");

        return nodeRegistry.invoke(nodeId, command, invokeParams, timeoutMs, idempotencyKey)
                .thenApply(result -> {
                    if (!result.isOk()) {
                        Map<String, Object> errorResult = new LinkedHashMap<>();
                        errorResult.put("ok", false);
                        errorResult.put("error", Map.of(
                                "code", result.getErrorCode() != null ? result.getErrorCode() : "UNKNOWN",
                                "message", result.getErrorMessage() != null ? result.getErrorMessage() : "unknown"));
                        return (Object) errorResult;
                    }
                    Map<String, Object> okResult = new LinkedHashMap<>();
                    okResult.put("ok", true);
                    okResult.put("nodeId", nodeId);
                    okResult.put("command", command);
                    okResult.put("payload", result.getPayload());
                    okResult.put("payloadJSON", result.getPayloadJSON());
                    return (Object) okResult;
                });
    }

    @SuppressWarnings("unchecked")
    private CompletableFuture<Object> handleNodeInvokeResult(JsonNode params, GatewayConnection conn) {
        String id = textOrNull(params, "id");
        String nodeId = textOrNull(params, "nodeId");
        if (id == null || nodeId == null)
            return failedResult("id and nodeId required");

        boolean ok = params.path("ok").asBoolean(false);
        Object payload = params.has("payload")
                ? objectMapper.convertValue(params.get("payload"), Object.class)
                : null;
        String payloadJSON = textOrNull(params, "payloadJSON");
        Map<String, String> error = null;
        if (params.has("error") && !params.get("error").isNull()) {
            error = objectMapper.convertValue(params.get("error"), Map.class);
        }

        boolean handled = nodeRegistry.handleInvokeResult(id, nodeId, ok, payload, payloadJSON, error);
        if (!handled) {
            log.debug("Late invoke result ignored: id={} node={}", id, nodeId);
        }

        return CompletableFuture.completedFuture(Map.of("ok", true));
    }

    private CompletableFuture<Object> handleNodeEvent(JsonNode params, GatewayConnection conn) {
        String event = textOrNull(params, "event");
        if (event == null)
            return failedResult("event required");
        // Forward as broadcast
        Object payload = params.has("payload")
                ? objectMapper.convertValue(params.get("payload"), Object.class)
                : null;
        log.debug("Node event: {} payload={}", event, payload);
        return CompletableFuture.completedFuture(Map.of("ok", true));
    }

    // ==================== Device Pairing ====================

    private CompletableFuture<Object> handleDevicePairList(JsonNode params, GatewayConnection conn) {
        var list = devicePairingService.listPairings();
        List<Map<String, Object>> redactedPaired = list.getPaired().stream()
                .map(DevicePairingService::redactDevice)
                .collect(Collectors.toList());
        return CompletableFuture.completedFuture(Map.of(
                "pending", list.getPending(),
                "paired", redactedPaired));
    }

    private CompletableFuture<Object> handleDevicePairApprove(JsonNode params, GatewayConnection conn) {
        String requestId = textOrNull(params, "requestId");
        if (requestId == null)
            return failedResult("requestId required");

        var result = devicePairingService.approvePairing(requestId);
        if (result == null)
            return failedResult("unknown requestId");

        broadcaster.broadcastToAll("device.pair.resolved", Map.of(
                "requestId", requestId,
                "deviceId", result.getDevice().getDeviceId(),
                "decision", "approved",
                "ts", System.currentTimeMillis()));

        return CompletableFuture.completedFuture(Map.of(
                "requestId", requestId,
                "device", DevicePairingService.redactDevice(result.getDevice())));
    }

    private CompletableFuture<Object> handleDevicePairReject(JsonNode params, GatewayConnection conn) {
        String requestId = textOrNull(params, "requestId");
        if (requestId == null)
            return failedResult("requestId required");

        var result = devicePairingService.rejectPairing(requestId);
        if (result == null)
            return failedResult("unknown requestId");

        broadcaster.broadcastToAll("device.pair.resolved", Map.of(
                "requestId", requestId,
                "deviceId", result.getDeviceId(),
                "decision", "rejected",
                "ts", System.currentTimeMillis()));

        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleDeviceTokenRotate(JsonNode params, GatewayConnection conn) {
        String deviceId = textOrNull(params, "deviceId");
        String role = textOrNull(params, "role");
        if (deviceId == null || role == null)
            return failedResult("deviceId and role required");

        List<String> scopes = stringList(params, "scopes");
        var token = devicePairingService.rotateToken(deviceId, role, scopes.isEmpty() ? null : scopes);
        if (token == null)
            return failedResult("unknown deviceId/role");

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("deviceId", deviceId);
        result.put("role", token.getRole());
        result.put("token", token.getToken());
        result.put("scopes", token.getScopes());
        result.put("rotatedAtMs", token.getRotatedAtMs() != null ? token.getRotatedAtMs() : token.getCreatedAtMs());
        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleDeviceTokenRevoke(JsonNode params, GatewayConnection conn) {
        String deviceId = textOrNull(params, "deviceId");
        String role = textOrNull(params, "role");
        if (deviceId == null || role == null)
            return failedResult("deviceId and role required");

        var token = devicePairingService.revokeToken(deviceId, role);
        if (token == null)
            return failedResult("unknown deviceId/role");

        return CompletableFuture.completedFuture(Map.of(
                "deviceId", deviceId,
                "role", token.getRole(),
                "revokedAtMs", token.getRevokedAtMs() != null ? token.getRevokedAtMs() : System.currentTimeMillis()));
    }

    // ==================== Helpers ====================

    private boolean isNodeRole(DevicePairingService.PairedDevice device) {
        if ("node".equals(device.getRole()))
            return true;
        if (device.getRoles() != null && device.getRoles().contains("node"))
            return true;
        return false;
    }

    private String textOrNull(JsonNode node, String field) {
        JsonNode child = node.path(field);
        return child.isTextual() ? child.asText() : null;
    }

    private List<String> stringList(JsonNode node, String field) {
        JsonNode child = node.path(field);
        if (!child.isArray())
            return List.of();
        List<String> result = new ArrayList<>();
        for (JsonNode item : child) {
            result.add(item.asText());
        }
        return result;
    }

    private CompletableFuture<Object> failedResult(String message) {
        return CompletableFuture.failedFuture(new IllegalArgumentException(message));
    }
}
