package com.openclaw.gateway.websocket;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.gateway.auth.AuthService;
import com.openclaw.gateway.net.NetUtils;
import com.openclaw.gateway.protocol.ProtocolTypes;
import com.openclaw.gateway.protocol.ProtocolTypes.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.socket.*;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.net.InetAddress;
import java.util.*;
import java.util.concurrent.*;

/**
 * Gateway WebSocket handler with TS-aligned protocol.
 * Implements connect.challenge → connect → hello-ok handshake.
 *
 * <p>
 * Frame format (aligns with TypeScript protocol):
 * <ul>
 * <li>Request: {type:"req", id, method, params?}</li>
 * <li>Response: {type:"res", id, ok, payload?, error?}</li>
 * <li>Event: {type:"event", event, payload?}</li>
 * </ul>
 */
@Slf4j
public class GatewayWebSocketHandler extends TextWebSocketHandler {

    private final ObjectMapper objectMapper;
    private final GatewayMethodRouter methodRouter;
    private final AuthService authService;
    private final ConfigService configService;
    private final Map<String, GatewayConnection> connections = new ConcurrentHashMap<>();
    private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor();
    private final Map<String, ScheduledFuture<?>> handshakeTimers = new ConcurrentHashMap<>();

    /** Application start time for uptime calculation. */
    private final long startedAt = System.currentTimeMillis();

    public GatewayWebSocketHandler(ObjectMapper objectMapper, GatewayMethodRouter methodRouter,
            AuthService authService, ConfigService configService) {
        this.objectMapper = objectMapper;
        this.methodRouter = methodRouter;
        this.authService = authService;
        this.configService = configService;
    }

    @Override
    public void afterConnectionEstablished(WebSocketSession session) {
        String connId = session.getId();
        GatewayConnection connection = new GatewayConnection(connId, session);
        connections.put(connId, connection);

        Map<String, Object> attrs = session.getAttributes();
        String remoteAddr = (String) attrs.get("auth.remoteAddr");

        log.debug("ws:in:open conn={} remote={}", connId, remoteAddr);

        // Send connect.challenge event
        var challenge = EventFrame.of("connect.challenge", Map.of(
                "nonce", connection.getConnectNonce(),
                "ts", System.currentTimeMillis()));
        connection.sendEvent(challenge, objectMapper);

        // Start handshake timeout
        ScheduledFuture<?> timer = scheduler.schedule(() -> {
            if (connection.isPending()) {
                connection.failHandshake();
                log.warn("handshake timeout conn={} remote={}", connId, remoteAddr);
                closeQuietly(session, CloseStatus.POLICY_VIOLATION, "handshake timeout");
            }
        }, ProtocolTypes.HANDSHAKE_TIMEOUT_MS, TimeUnit.MILLISECONDS);
        handshakeTimers.put(connId, timer);
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) {
        String connId = session.getId();
        String payload = message.getPayload();

        try {
            JsonNode node = objectMapper.readTree(payload);
            GatewayConnection connection = connections.get(connId);
            if (connection == null) {
                return;
            }

            String frameType = node.has("type") ? node.get("type").asText() : null;

            // Response frame from client (for server-initiated requests)
            if ("res".equals(frameType)) {
                String id = node.has("id") ? node.get("id").asText() : null;
                if (id != null) {
                    connection.resolveResponse(id, node);
                }
                return;
            }

            // Must be a request frame
            if (!"req".equals(frameType)) {
                connection.sendResponse(
                        ResponseFrame.failure("invalid", ErrorCodes.INVALID_REQUEST, "invalid frame type"),
                        objectMapper);
                return;
            }

            String id = node.has("id") ? node.get("id").asText() : null;
            String method = node.has("method") ? node.get("method").asText() : null;

            if (id == null || id.isEmpty() || method == null || method.isEmpty()) {
                connection.sendResponse(
                        ResponseFrame.failure(id != null ? id : "invalid",
                                ErrorCodes.INVALID_REQUEST, "invalid request frame"),
                        objectMapper);
                return;
            }

            // Pre-handshake: only accept "connect" method
            if (!connection.isConnected()) {
                if (!"connect".equals(method)) {
                    connection.sendResponse(
                            ResponseFrame.failure(id, ErrorCodes.INVALID_REQUEST,
                                    "invalid handshake: first request must be connect"),
                            objectMapper);
                    connection.failHandshake();
                    closeQuietly(session, CloseStatus.POLICY_VIOLATION, "invalid handshake");
                    return;
                }
                handleConnect(connection, id, node.get("params"));
                return;
            }

            // Post-handshake: dispatch request
            log.debug("ws:in:req conn={} id={} method={}", connId, id, method);

            // Method authorization
            var authError = MethodAuthorizer.authorize(method, connection);
            if (authError != null) {
                connection.sendResponse(ResponseFrame.failure(id, authError), objectMapper);
                return;
            }

            // Dispatch to method router
            JsonNode params = node.get("params");
            methodRouter.dispatch(method, params, connection)
                    .thenAccept(result -> {
                        connection.sendResponse(ResponseFrame.success(id, result), objectMapper);
                        log.debug("ws:out:res conn={} id={} method={} ok=true", connId, id, method);
                    })
                    .exceptionally(ex -> {
                        Throwable cause = ex.getCause() != null ? ex.getCause() : ex;
                        String errorCode;
                        if (cause instanceof UnsupportedOperationException) {
                            errorCode = ErrorCodes.NOT_FOUND;
                            log.debug("method not found: {}", method);
                        } else {
                            errorCode = ErrorCodes.UNAVAILABLE;
                            log.error("method {} failed: {}", method, cause.getMessage(), cause);
                        }
                        connection.sendResponse(
                                ResponseFrame.failure(id, errorCode, cause.getMessage()),
                                objectMapper);
                        return null;
                    });

        } catch (JsonProcessingException e) {
            log.warn("parse error conn={}: {}", connId, e.getMessage());
            GatewayConnection conn = connections.get(connId);
            if (conn != null && !conn.isConnected()) {
                closeQuietly(session, CloseStatus.BAD_DATA, "parse error");
            }
        }
    }

    /**
     * Handle the "connect" handshake request.
     * Validates params → authenticates → sends hello-ok response.
     */
    private void handleConnect(GatewayConnection connection, String requestId, JsonNode paramsNode) {
        String connId = connection.getConnectionId();
        Map<String, Object> attrs = connection.getSession().getAttributes();
        String remoteAddr = (String) attrs.get("auth.remoteAddr");
        String forwardedFor = (String) attrs.get("auth.forwardedFor");
        String realIp = (String) attrs.get("auth.realIp");
        String hostHeader = (String) attrs.get("auth.host");
        String forwardedHost = (String) attrs.get("auth.forwardedHost");

        // Parse connect params
        ConnectParams connectParams;
        try {
            connectParams = objectMapper.treeToValue(paramsNode, ConnectParams.class);
        } catch (Exception e) {
            connection.sendResponse(
                    ResponseFrame.failure(requestId, ErrorCodes.INVALID_REQUEST,
                            "invalid connect params: " + e.getMessage()),
                    objectMapper);
            connection.failHandshake();
            closeQuietly(connection.getSession(), CloseStatus.POLICY_VIOLATION, "invalid connect params");
            return;
        }

        if (connectParams == null || connectParams.getClient() == null) {
            connection.sendResponse(
                    ResponseFrame.failure(requestId, ErrorCodes.INVALID_REQUEST,
                            "connect params and client info required"),
                    objectMapper);
            connection.failHandshake();
            closeQuietly(connection.getSession(), CloseStatus.POLICY_VIOLATION, "missing connect params");
            return;
        }

        String clientLabel = connectParams.getClient().getDisplayName() != null
                ? connectParams.getClient().getDisplayName()
                : connectParams.getClient().getId();

        // Protocol version negotiation
        if (connectParams.getMaxProtocol() < ProtocolTypes.PROTOCOL_VERSION
                || connectParams.getMinProtocol() > ProtocolTypes.PROTOCOL_VERSION) {
            connection.sendResponse(
                    ResponseFrame.failure(requestId, ErrorCodes.INVALID_REQUEST,
                            "protocol mismatch"),
                    objectMapper);
            connection.failHandshake();
            log.warn("protocol mismatch conn={} client={} min={} max={} expected={}",
                    connId, clientLabel,
                    connectParams.getMinProtocol(), connectParams.getMaxProtocol(),
                    ProtocolTypes.PROTOCOL_VERSION);
            closeQuietly(connection.getSession(), new CloseStatus(1002, "protocol mismatch"));
            return;
        }

        // Role validation
        String roleRaw = connectParams.getRole() != null ? connectParams.getRole() : "operator";
        if (!"operator".equals(roleRaw) && !"node".equals(roleRaw)) {
            connection.sendResponse(
                    ResponseFrame.failure(requestId, ErrorCodes.INVALID_REQUEST, "invalid role"),
                    objectMapper);
            connection.failHandshake();
            closeQuietly(connection.getSession(), CloseStatus.POLICY_VIOLATION, "invalid role");
            return;
        }
        connectParams.setRole(roleRaw);

        // Scopes
        List<String> scopes = connectParams.getScopes();
        if (scopes == null || scopes.isEmpty()) {
            scopes = "operator".equals(roleRaw)
                    ? List.of("operator.admin")
                    : List.of();
            connectParams.setScopes(scopes);
        }

        // Authentication
        String authMethod = "none";
        if (!authService.isAuthRequired()) {
            // No auth configured → auto-authenticate
            authMethod = "none";
        } else {
            // Check local access
            List<String> trustedProxies = resolveTrustedProxies();
            String clientIp = NetUtils.resolveGatewayClientIp(remoteAddr, forwardedFor, realIp, trustedProxies);
            boolean isLocal = NetUtils.isLocalDirectRequest(
                    clientIp, hostHeader, forwardedFor, realIp, forwardedHost, remoteAddr, trustedProxies);

            if (isLocal) {
                authMethod = "local";
            } else {
                // Check connect auth (token/password)
                ConnectAuth connectAuth = connectParams.getAuth();
                if (connectAuth != null) {
                    AuthService.ConnectAuth serviceAuth = new AuthService.ConnectAuth(
                            connectAuth.getToken(), connectAuth.getPassword());
                    AuthService.GatewayAuthResult result = authService.authorize(serviceAuth, remoteAddr, false);
                    if (result.ok()) {
                        authMethod = result.method() != null ? result.method() : "token";
                    } else {
                        // Auth failed
                        String reason = result.reason() != null ? result.reason() : "unauthorized";
                        connection.sendResponse(
                                ResponseFrame.failure(requestId, ErrorCodes.INVALID_REQUEST,
                                        "unauthorized: " + reason),
                                objectMapper);
                        connection.failHandshake();
                        log.warn("unauthorized conn={} client={} reason={}", connId, clientLabel, reason);
                        closeQuietly(connection.getSession(), CloseStatus.POLICY_VIOLATION, "unauthorized");
                        return;
                    }
                } else {
                    // No auth provided on remote connection
                    connection.sendResponse(
                            ResponseFrame.failure(requestId, ErrorCodes.INVALID_REQUEST,
                                    "unauthorized: authentication required"),
                            objectMapper);
                    connection.failHandshake();
                    log.warn("unauthorized conn={} client={} reason=no-auth-provided", connId, clientLabel);
                    closeQuietly(connection.getSession(), CloseStatus.POLICY_VIOLATION, "unauthorized");
                    return;
                }
            }
        }

        // Handshake success
        connection.completeHandshake(connectParams, authMethod);
        cancelHandshakeTimer(connId);

        // Build hello-ok
        var registeredMethods = new ArrayList<>(methodRouter.getRegisteredMethods());
        Collections.sort(registeredMethods);

        var helloOk = new HelloOk();
        helloOk.setType("hello-ok");
        helloOk.setProtocol(ProtocolTypes.PROTOCOL_VERSION);
        helloOk.setServer(new ServerInfo(
                "0.1.0",
                null,
                getHostName(),
                connId));
        helloOk.setFeatures(new Features(
                registeredMethods,
                List.of("connect.challenge", "agent", "chat", "presence",
                        "tick", "talk.mode", "shutdown", "health", "heartbeat")));
        helloOk.setSnapshot(new Snapshot(
                List.of(), // presence (empty for now)
                null, // health
                new StateVersion(0, 0),
                System.currentTimeMillis() - startedAt,
                null, null));
        helloOk.setPolicy(Policy.defaults());

        // Send hello-ok as response payload
        connection.sendResponse(ResponseFrame.success(requestId, helloOk), objectMapper);

        log.info("ws:connect conn={} client={} mode={} v={} role={} auth={}",
                connId, clientLabel,
                connectParams.getClient().getMode(),
                connectParams.getClient().getVersion(),
                roleRaw, authMethod);
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) {
        String connId = session.getId();
        connections.remove(connId);
        cancelHandshakeTimer(connId);
        log.info("ws:close conn={} code={} reason={}",
                connId, status.getCode(), status.getReason());
    }

    @Override
    public void handleTransportError(WebSocketSession session, Throwable exception) {
        String connId = session.getId();
        log.error("ws:error conn={}: {}", connId, exception.getMessage());
    }

    // --- Broadcast ---

    /**
     * Broadcast an event to all connected clients.
     */
    public void broadcast(String event, Object payload) {
        var frame = EventFrame.of(event, payload);
        connections.values().stream()
                .filter(GatewayConnection::isConnected)
                .forEach(conn -> conn.sendEvent(frame, objectMapper));
    }

    /**
     * Number of active connections.
     */
    public int getConnectionCount() {
        return connections.size();
    }

    /**
     * Number of connected (handshaked) connections.
     */
    public int getConnectedCount() {
        return (int) connections.values().stream()
                .filter(GatewayConnection::isConnected).count();
    }

    // --- Helpers ---

    private List<String> resolveTrustedProxies() {
        OpenClawConfig config = configService.loadConfig();
        OpenClawConfig.GatewayConfig gw = config.getGateway();
        return (gw != null && gw.getTrustedProxies() != null) ? gw.getTrustedProxies() : List.of();
    }

    private void cancelHandshakeTimer(String connId) {
        ScheduledFuture<?> timer = handshakeTimers.remove(connId);
        if (timer != null) {
            timer.cancel(false);
        }
    }

    private void closeQuietly(WebSocketSession session, CloseStatus status) {
        try {
            if (session.isOpen()) {
                session.close(status);
            }
        } catch (Exception e) {
            log.debug("close error: {}", e.getMessage());
        }
    }

    private void closeQuietly(WebSocketSession session, CloseStatus status, String reason) {
        closeQuietly(session, new CloseStatus(status.getCode(), truncateReason(reason)));
    }

    private static String truncateReason(String reason) {
        // WebSocket close reason max 123 bytes
        if (reason == null)
            return null;
        return reason.length() > 120 ? reason.substring(0, 120) + "..." : reason;
    }

    private static String getHostName() {
        try {
            return InetAddress.getLocalHost().getHostName();
        } catch (Exception e) {
            return "unknown";
        }
    }
}
