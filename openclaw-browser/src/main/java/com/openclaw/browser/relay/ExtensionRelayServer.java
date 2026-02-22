package com.openclaw.browser.relay;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.Unpooled;
import io.netty.channel.*;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.codec.http.*;
import io.netty.handler.codec.http.websocketx.*;
import io.netty.util.CharsetUtil;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Extension Relay Server — Netty-based HTTP+WebSocket proxy.
 * Protocol-compatible with TypeScript's extension-relay.ts Chrome Extension.
 *
 * <p>Provides:
 * <ul>
 *   <li>{@code /extension} WebSocket — for Chrome Extension connection</li>
 *   <li>{@code /cdp} WebSocket — for CDP clients (Playwright, CdpClient)</li>
 *   <li>{@code /json/*} HTTP — standard Chrome DevTools Protocol HTTP endpoints</li>
 * </ul>
 */
@Slf4j
public class ExtensionRelayServer {

    private static final ObjectMapper mapper = new ObjectMapper();

    @Getter private final String host;
    @Getter private final int port;
    @Getter private final String baseUrl;
    @Getter private final String cdpWsUrl;
    @Getter private final String relayAuthToken;

    private Channel serverChannel;
    private EventLoopGroup bossGroup;
    private EventLoopGroup workerGroup;

    // State
    private volatile ChannelHandlerContext extensionCtx;
    private final Set<ChannelHandlerContext> cdpClients = ConcurrentHashMap.newKeySet();
    private final ConcurrentMap<String, ExtensionRelayTypes.ConnectedTarget> connectedTargets
            = new ConcurrentHashMap<>();
    private final ConcurrentMap<Integer, CompletableFuture<Object>> pendingExtension
            = new ConcurrentHashMap<>();
    private final AtomicInteger nextExtensionId = new AtomicInteger(1);
    private ScheduledFuture<?> pingTask;

    public ExtensionRelayServer(String host, int port) {
        this.host = host;
        this.port = port;
        this.baseUrl = "http://" + host + ":" + port;
        this.cdpWsUrl = "ws://" + host + ":" + port + "/cdp";

        byte[] tokenBytes = new byte[32];
        new SecureRandom().nextBytes(tokenBytes);
        this.relayAuthToken = Base64.getUrlEncoder().withoutPadding().encodeToString(tokenBytes);
    }

    /**
     * Start the relay server.
     */
    public void start() throws InterruptedException {
        bossGroup = new NioEventLoopGroup(1);
        workerGroup = new NioEventLoopGroup(2);

        ServerBootstrap b = new ServerBootstrap();
        b.group(bossGroup, workerGroup)
                .channel(NioServerSocketChannel.class)
                .childHandler(new ChannelInitializer<SocketChannel>() {
                    @Override
                    protected void initChannel(SocketChannel ch) {
                        ch.pipeline().addLast(
                                new HttpServerCodec(),
                                new HttpObjectAggregator(65536),
                                new RelayHandler());
                    }
                });

        serverChannel = b.bind(host, port).sync().channel();
        log.info("Extension Relay started on {}:{}", host, port);

        // Start ping timer
        pingTask = workerGroup.scheduleAtFixedRate(this::sendPing, 5, 5, TimeUnit.SECONDS);
    }

    /**
     * Stop the relay server and close all connections.
     */
    public void stop() {
        if (pingTask != null) pingTask.cancel(false);

        if (extensionCtx != null) {
            try { extensionCtx.close(); } catch (Exception e) { /* ignore */ }
        }
        for (ChannelHandlerContext ctx : cdpClients) {
            try { ctx.close(); } catch (Exception e) { /* ignore */ }
        }

        pendingExtension.values().forEach(f ->
                f.completeExceptionally(new Exception("server stopping")));
        pendingExtension.clear();

        if (serverChannel != null) {
            try { serverChannel.close().sync(); } catch (Exception e) { /* ignore */ }
        }
        if (bossGroup != null) bossGroup.shutdownGracefully();
        if (workerGroup != null) workerGroup.shutdownGracefully();

        log.info("Extension Relay stopped");
    }

    public boolean isExtensionConnected() {
        return extensionCtx != null && extensionCtx.channel().isActive();
    }

    // ==================== Relay Handler ====================

    private class RelayHandler extends SimpleChannelInboundHandler<Object> {

        private WebSocketServerHandshaker handshaker;
        private String wsPath; // "/extension" or "/cdp"

        @Override
        protected void channelRead0(ChannelHandlerContext ctx, Object msg) throws Exception {
            if (msg instanceof FullHttpRequest request) {
                handleHttpRequest(ctx, request);
            } else if (msg instanceof WebSocketFrame frame) {
                handleWebSocketFrame(ctx, frame);
            }
        }

        private void handleHttpRequest(ChannelHandlerContext ctx, FullHttpRequest req)
                throws Exception {
            String uri = req.uri();
            String path = URI.create(uri).getPath();

            // WebSocket upgrade
            if (req.headers().contains(HttpHeaderNames.UPGRADE, HttpHeaderValues.WEBSOCKET, true)) {
                handleWebSocketUpgrade(ctx, req, path);
                return;
            }

            // HTTP endpoints
            if ("/".equals(path)) {
                if (req.method() == HttpMethod.HEAD) {
                    sendResponse(ctx, HttpResponseStatus.OK, "");
                } else {
                    sendResponse(ctx, HttpResponseStatus.OK, "OK");
                }
                return;
            }

            if ("/extension/status".equals(path)) {
                sendJsonResponse(ctx, HttpResponseStatus.OK,
                        "{\"connected\":" + isExtensionConnected() + "}");
                return;
            }

            // Auth check for /json/* endpoints
            if (path.startsWith("/json")) {
                String token = req.headers().get(ExtensionRelayTypes.RELAY_AUTH_HEADER);
                if (token == null || !token.equals(relayAuthToken)) {
                    sendResponse(ctx, HttpResponseStatus.UNAUTHORIZED, "Unauthorized");
                    return;
                }
            }

            if ("/json/version".equals(path) || "/json/version/".equals(path)) {
                ObjectNode payload = mapper.createObjectNode();
                payload.put("Browser", "OpenClaw/extension-relay");
                payload.put("Protocol-Version", "1.3");
                if (isExtensionConnected()) {
                    payload.put("webSocketDebuggerUrl", cdpWsUrl);
                }
                sendJsonResponse(ctx, HttpResponseStatus.OK, mapper.writeValueAsString(payload));
                return;
            }

            if (Set.of("/json", "/json/", "/json/list", "/json/list/").contains(path)) {
                List<Map<String, Object>> list = new ArrayList<>();
                for (ExtensionRelayTypes.ConnectedTarget t : connectedTargets.values()) {
                    Map<String, Object> entry = new LinkedHashMap<>();
                    entry.put("id", t.getTargetId());
                    entry.put("type", t.getTargetInfo().getType() != null
                            ? t.getTargetInfo().getType() : "page");
                    entry.put("title", t.getTargetInfo().getTitle() != null
                            ? t.getTargetInfo().getTitle() : "");
                    entry.put("description", t.getTargetInfo().getTitle() != null
                            ? t.getTargetInfo().getTitle() : "");
                    entry.put("url", t.getTargetInfo().getUrl() != null
                            ? t.getTargetInfo().getUrl() : "");
                    entry.put("webSocketDebuggerUrl", cdpWsUrl);
                    list.add(entry);
                }
                sendJsonResponse(ctx, HttpResponseStatus.OK, mapper.writeValueAsString(list));
                return;
            }

            // /json/activate/{targetId}
            if (path.matches("/json/activate/.+")) {
                String targetId = path.substring("/json/activate/".length());
                forwardToExtensionAsync("Target.activateTarget",
                        mapper.createObjectNode().put("targetId", targetId), null);
                sendResponse(ctx, HttpResponseStatus.OK, "OK");
                return;
            }

            // /json/close/{targetId}
            if (path.matches("/json/close/.+")) {
                String targetId = path.substring("/json/close/".length());
                forwardToExtensionAsync("Target.closeTarget",
                        mapper.createObjectNode().put("targetId", targetId), null);
                sendResponse(ctx, HttpResponseStatus.OK, "OK");
                return;
            }

            sendResponse(ctx, HttpResponseStatus.NOT_FOUND, "not found");
        }

        private void handleWebSocketUpgrade(ChannelHandlerContext ctx, FullHttpRequest req,
                                             String path) {
            // Loopback check
            InetSocketAddress remote = (InetSocketAddress) ctx.channel().remoteAddress();
            if (!isLoopback(remote.getAddress().getHostAddress())) {
                sendResponse(ctx, HttpResponseStatus.FORBIDDEN, "Forbidden");
                return;
            }

            // Origin check — only allow chrome-extension:// origins
            String origin = req.headers().get(HttpHeaderNames.ORIGIN);
            if (origin != null && !origin.startsWith("chrome-extension://")) {
                sendResponse(ctx, HttpResponseStatus.FORBIDDEN, "Forbidden: invalid origin");
                return;
            }

            if ("/extension".equals(path)) {
                if (isExtensionConnected()) {
                    sendResponse(ctx, HttpResponseStatus.CONFLICT, "Extension already connected");
                    return;
                }
                wsPath = "/extension";
            } else if ("/cdp".equals(path)) {
                String token = req.headers().get(ExtensionRelayTypes.RELAY_AUTH_HEADER);
                if (token == null || !token.equals(relayAuthToken)) {
                    sendResponse(ctx, HttpResponseStatus.UNAUTHORIZED, "Unauthorized");
                    return;
                }
                if (!isExtensionConnected()) {
                    sendResponse(ctx, HttpResponseStatus.SERVICE_UNAVAILABLE,
                            "Extension not connected");
                    return;
                }
                wsPath = "/cdp";
            } else {
                sendResponse(ctx, HttpResponseStatus.NOT_FOUND, "Not Found");
                return;
            }

            WebSocketServerHandshakerFactory wsFactory = new WebSocketServerHandshakerFactory(
                    "ws://" + req.headers().get(HttpHeaderNames.HOST) + path, null, true);
            handshaker = wsFactory.newHandshaker(req);
            if (handshaker == null) {
                WebSocketServerHandshakerFactory.sendUnsupportedVersionResponse(ctx.channel());
            } else {
                handshaker.handshake(ctx.channel(), req);
                if ("/extension".equals(wsPath)) {
                    extensionCtx = ctx;
                    log.info("Chrome Extension connected via relay");
                } else {
                    cdpClients.add(ctx);
                }
            }
        }

        private void handleWebSocketFrame(ChannelHandlerContext ctx, WebSocketFrame frame)
                throws Exception {
            if (frame instanceof CloseWebSocketFrame) {
                if ("/extension".equals(wsPath)) {
                    onExtensionDisconnected();
                } else {
                    cdpClients.remove(ctx);
                }
                handshaker.close(ctx.channel(), (CloseWebSocketFrame) frame.retain());
                return;
            }
            if (frame instanceof PingWebSocketFrame) {
                ctx.writeAndFlush(new PongWebSocketFrame(frame.content().retain()));
                return;
            }
            if (!(frame instanceof TextWebSocketFrame textFrame)) return;

            String text = textFrame.text();
            if ("/extension".equals(wsPath)) {
                handleExtensionMessage(text);
            } else {
                handleCdpClientMessage(ctx, text);
            }
        }

        @Override
        public void channelInactive(ChannelHandlerContext ctx) {
            if ("/extension".equals(wsPath) && extensionCtx == ctx) {
                onExtensionDisconnected();
            } else {
                cdpClients.remove(ctx);
            }
        }

        @Override
        public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
            log.debug("Relay handler error: {}", cause.getMessage());
            ctx.close();
        }
    }

    // ==================== Extension message handling ====================

    private void handleExtensionMessage(String text) {
        try {
            JsonNode msg = mapper.readTree(text);

            // Response to a forwarded command
            if (msg.has("id") && msg.get("id").isInt()) {
                int id = msg.get("id").asInt();
                CompletableFuture<Object> future = pendingExtension.remove(id);
                if (future == null) return;

                if (msg.has("error") && msg.get("error").isTextual()
                        && !msg.get("error").asText().isBlank()) {
                    future.completeExceptionally(new Exception(msg.get("error").asText()));
                } else {
                    future.complete(msg.has("result") ? msg.get("result") : null);
                }
                return;
            }

            // Event or pong
            if (msg.has("method")) {
                String method = msg.get("method").asText("");
                if ("pong".equals(method)) return;
                if (!"forwardCDPEvent".equals(method)) return;

                JsonNode eventParams = msg.get("params");
                if (eventParams == null) return;
                String cdpMethod = eventParams.has("method")
                        ? eventParams.get("method").asText("") : "";
                Object cdpParams = eventParams.has("params") ? eventParams.get("params") : null;
                String sessionId = eventParams.has("sessionId")
                        ? eventParams.get("sessionId").asText(null) : null;

                // Target management
                if ("Target.attachedToTarget".equals(cdpMethod)) {
                    handleAttachedToTarget(cdpParams, sessionId);
                    return;
                }
                if ("Target.detachedFromTarget".equals(cdpMethod)) {
                    handleDetachedFromTarget(cdpParams);
                    broadcastCdpEvent(cdpMethod, cdpParams, sessionId);
                    return;
                }
                if ("Target.targetInfoChanged".equals(cdpMethod)) {
                    handleTargetInfoChanged(cdpParams);
                }

                broadcastCdpEvent(cdpMethod, cdpParams, sessionId);
            }
        } catch (Exception e) {
            log.debug("Extension message parse error: {}", e.getMessage());
        }
    }

    private void handleAttachedToTarget(Object params, String sessionId) {
        try {
            JsonNode p = mapper.valueToTree(params);
            String targetType = p.has("targetInfo") && p.get("targetInfo").has("type")
                    ? p.get("targetInfo").get("type").asText("page") : "page";
            if (!"page".equals(targetType)) return;

            String sid = p.has("sessionId") ? p.get("sessionId").asText("") : "";
            String targetId = p.has("targetInfo") && p.get("targetInfo").has("targetId")
                    ? p.get("targetInfo").get("targetId").asText("") : "";
            if (sid.isEmpty() || targetId.isEmpty()) return;

            ExtensionRelayTypes.TargetInfo info = mapper.treeToValue(
                    p.get("targetInfo"), ExtensionRelayTypes.TargetInfo.class);

            ExtensionRelayTypes.ConnectedTarget prev = connectedTargets.get(sid);
            boolean changed = prev != null && prev.getTargetId() != null
                    && !prev.getTargetId().equals(targetId);

            connectedTargets.put(sid, ExtensionRelayTypes.ConnectedTarget.builder()
                    .sessionId(sid).targetId(targetId).targetInfo(info).build());

            if (changed && prev.getTargetId() != null) {
                broadcastCdpEvent("Target.detachedFromTarget",
                        mapper.createObjectNode()
                                .put("sessionId", sid)
                                .put("targetId", prev.getTargetId()),
                        sid);
            }
            if (prev == null || changed) {
                broadcastCdpEvent("Target.attachedToTarget", params, sessionId);
            }
        } catch (Exception e) {
            log.debug("handleAttachedToTarget error: {}", e.getMessage());
        }
    }

    private void handleDetachedFromTarget(Object params) {
        try {
            JsonNode p = mapper.valueToTree(params);
            String sid = p.has("sessionId") ? p.get("sessionId").asText("") : "";
            if (!sid.isEmpty()) {
                connectedTargets.remove(sid);
            }
        } catch (Exception e) {
            // ignore
        }
    }

    private void handleTargetInfoChanged(Object params) {
        try {
            JsonNode p = mapper.valueToTree(params);
            if (!p.has("targetInfo")) return;
            JsonNode ti = p.get("targetInfo");
            String targetId = ti.has("targetId") ? ti.get("targetId").asText("") : "";
            String type = ti.has("type") ? ti.get("type").asText("page") : "page";
            if (targetId.isEmpty() || !"page".equals(type)) return;

            ExtensionRelayTypes.TargetInfo info = mapper.treeToValue(ti,
                    ExtensionRelayTypes.TargetInfo.class);
            for (Map.Entry<String, ExtensionRelayTypes.ConnectedTarget> entry
                    : connectedTargets.entrySet()) {
                if (entry.getValue().getTargetId().equals(targetId)) {
                    connectedTargets.put(entry.getKey(),
                            ExtensionRelayTypes.ConnectedTarget.builder()
                                    .sessionId(entry.getKey())
                                    .targetId(targetId)
                                    .targetInfo(info)
                                    .build());
                }
            }
        } catch (Exception e) {
            // ignore
        }
    }

    // ==================== CDP client handling ====================

    private void handleCdpClientMessage(ChannelHandlerContext ctx, String text) {
        try {
            JsonNode cmd = mapper.readTree(text);
            if (!cmd.has("id") || !cmd.has("method")) return;

            int id = cmd.get("id").asInt();
            String method = cmd.get("method").asText("");
            Object params = cmd.has("params") ? cmd.get("params") : null;
            String sessionId = cmd.has("sessionId") ? cmd.get("sessionId").asText(null) : null;

            Object result = routeCdpCommand(method, params, sessionId);

            // Handle auto-attach replay
            if ("Target.setAutoAttach".equals(method) && sessionId == null) {
                replayTargetEvents(ctx, "autoAttach");
            }
            if ("Target.setDiscoverTargets".equals(method)) {
                JsonNode p = cmd.has("params") ? cmd.get("params") : null;
                if (p != null && p.has("discover") && p.get("discover").asBoolean()) {
                    replayTargetEvents(ctx, "discover");
                }
            }
            if ("Target.attachToTarget".equals(method)) {
                replayAttachToTarget(ctx, cmd.has("params") ? cmd.get("params") : null);
            }

            sendCdpResponse(ctx, id, sessionId, result, null);

        } catch (Exception e) {
            log.debug("CDP client message error: {}", e.getMessage());
        }
    }

    private Object routeCdpCommand(String method, Object params, String sessionId)
            throws Exception {
        return switch (method) {
            case "Browser.getVersion" -> {
                ObjectNode r = mapper.createObjectNode();
                r.put("protocolVersion", "1.3");
                r.put("product", "Chrome/OpenClaw-Extension-Relay");
                r.put("revision", "0");
                r.put("userAgent", "OpenClaw-Extension-Relay");
                r.put("jsVersion", "V8");
                yield r;
            }
            case "Browser.setDownloadBehavior" -> mapper.createObjectNode();
            case "Target.setAutoAttach", "Target.setDiscoverTargets" -> mapper.createObjectNode();
            case "Target.getTargets" -> {
                List<Map<String, Object>> infos = new ArrayList<>();
                for (ExtensionRelayTypes.ConnectedTarget t : connectedTargets.values()) {
                    Map<String, Object> info = new LinkedHashMap<>();
                    info.put("targetId", t.getTargetId());
                    info.put("type", t.getTargetInfo().getType() != null
                            ? t.getTargetInfo().getType() : "page");
                    info.put("title", t.getTargetInfo().getTitle());
                    info.put("url", t.getTargetInfo().getUrl());
                    info.put("attached", true);
                    infos.add(info);
                }
                ObjectNode r = mapper.createObjectNode();
                r.set("targetInfos", mapper.valueToTree(infos));
                yield r;
            }
            case "Target.getTargetInfo" -> {
                String targetId = getTargetIdFromParams(params);
                ExtensionRelayTypes.ConnectedTarget found = null;
                if (targetId != null) {
                    for (ExtensionRelayTypes.ConnectedTarget t : connectedTargets.values()) {
                        if (targetId.equals(t.getTargetId())) {
                            found = t;
                            break;
                        }
                    }
                }
                if (found == null && sessionId != null) {
                    found = connectedTargets.get(sessionId);
                }
                if (found == null) {
                    found = connectedTargets.values().stream().findFirst().orElse(null);
                }
                ObjectNode r = mapper.createObjectNode();
                if (found != null) r.set("targetInfo", mapper.valueToTree(found.getTargetInfo()));
                yield r;
            }
            case "Target.attachToTarget" -> {
                String targetId = getTargetIdFromParams(params);
                if (targetId == null) throw new Exception("targetId required");
                for (ExtensionRelayTypes.ConnectedTarget t : connectedTargets.values()) {
                    if (targetId.equals(t.getTargetId())) {
                        ObjectNode r = mapper.createObjectNode();
                        r.put("sessionId", t.getSessionId());
                        yield r;
                    }
                }
                throw new Exception("target not found");
            }
            default -> {
                int id = nextExtensionId.getAndIncrement();
                yield sendToExtension(id, method, params, sessionId);
            }
        };
    }

    // ==================== Extension communication ====================

    private Object sendToExtension(int id, String cdpMethod, Object cdpParams,
                                   String sessionId) throws Exception {
        ChannelHandlerContext ext = extensionCtx;
        if (ext == null || !ext.channel().isActive()) {
            throw new Exception("Chrome extension not connected");
        }

        ExtensionRelayTypes.ExtensionForwardCommandMessage msg =
                ExtensionRelayTypes.ExtensionForwardCommandMessage.create(
                        id, cdpMethod, cdpParams, sessionId);

        CompletableFuture<Object> future = new CompletableFuture<>();
        pendingExtension.put(id, future);

        ext.writeAndFlush(new TextWebSocketFrame(mapper.writeValueAsString(msg)));

        try {
            return future.get(30, TimeUnit.SECONDS);
        } catch (TimeoutException e) {
            pendingExtension.remove(id);
            throw new Exception("extension request timeout: " + cdpMethod);
        }
    }

    private void forwardToExtensionAsync(String cdpMethod, Object cdpParams, String sessionId) {
        try {
            int id = nextExtensionId.getAndIncrement();
            sendToExtension(id, cdpMethod, cdpParams, sessionId);
        } catch (Exception e) {
            // ignore
        }
    }

    // ==================== Broadcasting ====================

    private void broadcastCdpEvent(String method, Object params, String sessionId) {
        try {
            ObjectNode evt = mapper.createObjectNode();
            evt.put("method", method);
            if (params != null) evt.set("params", mapper.valueToTree(params));
            if (sessionId != null) evt.put("sessionId", sessionId);

            String json = mapper.writeValueAsString(evt);
            TextWebSocketFrame frame = new TextWebSocketFrame(json);

            for (ChannelHandlerContext ctx : cdpClients) {
                if (ctx.channel().isActive()) {
                    ctx.writeAndFlush(frame.retainedDuplicate());
                }
            }
            frame.release();
        } catch (Exception e) {
            log.debug("Broadcast error: {}", e.getMessage());
        }
    }

    private void sendCdpResponse(ChannelHandlerContext ctx, int id, String sessionId,
                                 Object result, String errorMsg) {
        try {
            ObjectNode resp = mapper.createObjectNode();
            resp.put("id", id);
            if (sessionId != null) resp.put("sessionId", sessionId);
            if (errorMsg != null) {
                ObjectNode err = mapper.createObjectNode();
                err.put("message", errorMsg);
                resp.set("error", err);
            } else if (result != null) {
                resp.set("result", mapper.valueToTree(result));
            } else {
                resp.set("result", mapper.createObjectNode());
            }
            ctx.writeAndFlush(new TextWebSocketFrame(mapper.writeValueAsString(resp)));
        } catch (Exception e) {
            log.debug("sendCdpResponse error: {}", e.getMessage());
        }
    }

    private void replayTargetEvents(ChannelHandlerContext ctx, String mode) {
        for (ExtensionRelayTypes.ConnectedTarget t : connectedTargets.values()) {
            try {
                ObjectNode evt = mapper.createObjectNode();
                if ("autoAttach".equals(mode)) {
                    evt.put("method", "Target.attachedToTarget");
                    ObjectNode params = mapper.createObjectNode();
                    params.put("sessionId", t.getSessionId());
                    params.set("targetInfo", mapper.valueToTree(t.getTargetInfo()));
                    params.put("waitingForDebugger", false);
                    evt.set("params", params);
                } else {
                    evt.put("method", "Target.targetCreated");
                    ObjectNode params = mapper.createObjectNode();
                    ObjectNode info = mapper.valueToTree(t.getTargetInfo()).deepCopy();
                    ((ObjectNode) info).put("attached", true);
                    params.set("targetInfo", info);
                    evt.set("params", params);
                }
                ctx.writeAndFlush(new TextWebSocketFrame(mapper.writeValueAsString(evt)));
            } catch (Exception e) {
                // ignore
            }
        }
    }

    private void replayAttachToTarget(ChannelHandlerContext ctx, JsonNode params) {
        if (params == null) return;
        String targetId = params.has("targetId") ? params.get("targetId").asText("") : "";
        if (targetId.isEmpty()) return;
        for (ExtensionRelayTypes.ConnectedTarget t : connectedTargets.values()) {
            if (targetId.equals(t.getTargetId())) {
                try {
                    ObjectNode evt = mapper.createObjectNode();
                    evt.put("method", "Target.attachedToTarget");
                    ObjectNode p = mapper.createObjectNode();
                    p.put("sessionId", t.getSessionId());
                    ObjectNode info = mapper.valueToTree(t.getTargetInfo()).deepCopy();
                    ((ObjectNode) info).put("attached", true);
                    p.set("targetInfo", info);
                    p.put("waitingForDebugger", false);
                    evt.set("params", p);
                    ctx.writeAndFlush(new TextWebSocketFrame(mapper.writeValueAsString(evt)));
                } catch (Exception e) {
                    // ignore
                }
                return;
            }
        }
    }

    // ==================== Lifecycle ====================

    private void onExtensionDisconnected() {
        extensionCtx = null;
        pendingExtension.values().forEach(f ->
                f.completeExceptionally(new Exception("extension disconnected")));
        pendingExtension.clear();
        connectedTargets.clear();

        for (ChannelHandlerContext ctx : cdpClients) {
            try { ctx.close(); } catch (Exception e) { /* ignore */ }
        }
        cdpClients.clear();
        log.info("Chrome Extension disconnected from relay");
    }

    private void sendPing() {
        ChannelHandlerContext ext = extensionCtx;
        if (ext == null || !ext.channel().isActive()) return;
        try {
            ext.writeAndFlush(new TextWebSocketFrame("{\"method\":\"ping\"}"));
        } catch (Exception e) {
            // ignore
        }
    }

    // ==================== Helpers ====================

    private static void sendResponse(ChannelHandlerContext ctx, HttpResponseStatus status,
                                      String body) {
        FullHttpResponse resp = new DefaultFullHttpResponse(
                HttpVersion.HTTP_1_1, status,
                Unpooled.copiedBuffer(body, CharsetUtil.UTF_8));
        resp.headers().set(HttpHeaderNames.CONTENT_TYPE, "text/plain; charset=utf-8");
        resp.headers().set(HttpHeaderNames.CONTENT_LENGTH, resp.content().readableBytes());
        resp.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.CLOSE);
        ctx.writeAndFlush(resp);
    }

    private static void sendJsonResponse(ChannelHandlerContext ctx, HttpResponseStatus status,
                                          String json) {
        FullHttpResponse resp = new DefaultFullHttpResponse(
                HttpVersion.HTTP_1_1, status,
                Unpooled.copiedBuffer(json, CharsetUtil.UTF_8));
        resp.headers().set(HttpHeaderNames.CONTENT_TYPE, "application/json");
        resp.headers().set(HttpHeaderNames.CONTENT_LENGTH, resp.content().readableBytes());
        ctx.writeAndFlush(resp);
    }

    private static boolean isLoopback(String ip) {
        if (ip == null) return false;
        return ip.equals("127.0.0.1") || ip.startsWith("127.")
                || ip.equals("::1") || ip.startsWith("::ffff:127.")
                || ip.equals("0:0:0:0:0:0:0:1");
    }

    private String getTargetIdFromParams(Object params) {
        if (params == null) return null;
        try {
            JsonNode p = mapper.valueToTree(params);
            return p.has("targetId") ? p.get("targetId").asText(null) : null;
        } catch (Exception e) {
            return null;
        }
    }
}
