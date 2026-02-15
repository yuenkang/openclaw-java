package com.openclaw.app.browser;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.agent.tools.builtin.browser.PlaywrightSession;
import com.openclaw.common.config.PortDefaults;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.*;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.codec.http.*;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.*;

import static io.netty.handler.codec.http.HttpHeaderNames.CONTENT_TYPE;
import static io.netty.handler.codec.http.HttpResponseStatus.*;
import static io.netty.handler.codec.http.HttpVersion.HTTP_1_1;

/**
 * Standalone Netty HTTP server for browser control, listening on port 18791.
 * Corresponds to TypeScript's browser/server.ts (Express on controlPort).
 *
 * <p>
 * All browser operations are delegated to {@link BrowserProfileService}.
 * </p>
 */
@Slf4j
@Component
public class BrowserControlServer {

    private static final ObjectMapper mapper = new ObjectMapper();

    private final BrowserProfileService profileService;
    private EventLoopGroup bossGroup;
    private EventLoopGroup workerGroup;
    private Channel serverChannel;
    private boolean headless = false;

    public BrowserControlServer(BrowserProfileService profileService) {
        this.profileService = profileService;
    }

    @PostConstruct
    public void start() {
        start(PortDefaults.DEFAULT_BROWSER_CONTROL_PORT);
    }

    public void start(int port) {
        if (serverChannel != null) {
            log.debug("Browser control server already running");
            return;
        }
        bossGroup = new NioEventLoopGroup(1);
        workerGroup = new NioEventLoopGroup(4);
        try {
            ServerBootstrap b = new ServerBootstrap();
            b.group(bossGroup, workerGroup)
                    .channel(NioServerSocketChannel.class)
                    .childHandler(new ChannelInitializer<SocketChannel>() {
                        @Override
                        protected void initChannel(SocketChannel ch) {
                            ch.pipeline().addLast(
                                    new HttpServerCodec(),
                                    new HttpObjectAggregator(1048576), // 1MB
                                    new BrowserControlHandler());
                        }
                    });
            serverChannel = b.bind("127.0.0.1", port).sync().channel();
            log.info("Browser control listening on http://127.0.0.1:{}/", port);
        } catch (Exception e) {
            log.error("Browser control server failed to bind 127.0.0.1:{}: {}", port, e.getMessage());
            shutdown();
        }
    }

    @PreDestroy
    public void stop() {
        profileService.stopAll();
        shutdown();
        log.info("Browser control server stopped");
    }

    private void shutdown() {
        if (serverChannel != null) {
            serverChannel.close();
            serverChannel = null;
        }
        if (workerGroup != null)
            workerGroup.shutdownGracefully();
        if (bossGroup != null)
            bossGroup.shutdownGracefully();
    }

    // =========================================================================
    // Netty Handler
    // =========================================================================

    private class BrowserControlHandler extends SimpleChannelInboundHandler<FullHttpRequest> {

        @Override
        protected void channelRead0(ChannelHandlerContext ctx, FullHttpRequest request) {
            String uri = request.uri();
            QueryStringDecoder decoder = new QueryStringDecoder(uri);
            String path = decoder.path();
            HttpMethod method = request.method();
            String profile = firstParam(decoder, "profile");

            try {
                Object result = route(method, path, decoder, request, profile);
                sendJson(ctx, OK, result);
            } catch (BadRequestException e) {
                sendJson(ctx, BAD_REQUEST, Map.of("ok", false, "error", e.getMessage()));
            } catch (MethodNotAllowedException e) {
                sendJson(ctx, METHOD_NOT_ALLOWED, Map.of("ok", false, "error", "method not allowed"));
            } catch (NotFoundException e) {
                sendJson(ctx, NOT_FOUND, Map.of("ok", false, "error", "not found: " + path));
            } catch (Exception e) {
                log.error("Browser control error on {}: {}", path, e.getMessage(), e);
                sendJson(ctx, INTERNAL_SERVER_ERROR, Map.of("ok", false, "error", e.getMessage()));
            }
        }

        @Override
        public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
            log.error("Netty channel error: {}", cause.getMessage(), cause);
            ctx.close();
        }

        // =====================================================================
        // Router
        // =====================================================================

        private Object route(HttpMethod method, String path, QueryStringDecoder decoder,
                FullHttpRequest request, String profile) throws Exception {

            // GET /
            if ("/".equals(path) && method == HttpMethod.GET) {
                return handleStatus(profile);
            }

            // POST /start
            if ("/start".equals(path) && method == HttpMethod.POST) {
                return handleStart(request, profile);
            }

            // POST /stop
            if ("/stop".equals(path) && method == HttpMethod.POST) {
                return handleStop(profile);
            }

            // /profiles/**
            if (path.startsWith("/profiles")) {
                return handleProfiles(method, path, request);
            }

            // /tabs/**
            if (path.startsWith("/tabs")) {
                return handleTabs(method, path, request, profile);
            }

            // POST /navigate
            if ("/navigate".equals(path) && method == HttpMethod.POST) {
                return handleNavigate(request, profile);
            }

            // GET /snapshot
            if ("/snapshot".equals(path) && method == HttpMethod.GET) {
                return handleSnapshot(decoder, profile);
            }

            // POST /screenshot
            if ("/screenshot".equals(path) && method == HttpMethod.POST) {
                return handleScreenshot(request, profile);
            }

            // POST /act
            if ("/act".equals(path) && method == HttpMethod.POST) {
                return handleAct(request, profile);
            }

            // GET /console
            if ("/console".equals(path) && method == HttpMethod.GET) {
                return handleConsole(decoder, profile);
            }

            // POST /pdf
            if ("/pdf".equals(path)) {
                throw new BadRequestException("PDF export not yet implemented");
            }

            // Stubs
            if (path.startsWith("/hooks/")) {
                return Map.of("ok", true);
            }

            throw new NotFoundException();
        }

        // =====================================================================
        // Handlers
        // =====================================================================

        private Object handleStatus(String profile) {
            String resolved = profileService.resolveProfileName(profile);
            Map<String, Object> body = new LinkedHashMap<>();
            body.put("enabled", true);
            body.put("profile", resolved);
            body.put("running", profileService.isRunning(profile));
            body.put("headless", headless);
            return body;
        }

        private Object handleStart(FullHttpRequest request, String profile) {
            Map<String, Object> reqBody = readBody(request);
            PlaywrightSession session = profileService.getOrCreateSession(profile);

            if (session.isRunning()) {
                Map<String, Object> result = new LinkedHashMap<>();
                result.put("ok", true);
                result.put("profile", profileService.resolveProfileName(profile));
                result.put("message", "already running");
                return result;
            }

            String cdpUrl = reqBody != null ? (String) reqBody.get("cdpUrl") : null;
            if (reqBody != null && reqBody.containsKey("headless")) {
                headless = Boolean.TRUE.equals(reqBody.get("headless"));
            }

            if (cdpUrl != null && !cdpUrl.isBlank()) {
                session.connectCDP(cdpUrl);
            } else {
                session.launch(headless);
            }

            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("profile", profileService.resolveProfileName(profile));
            return result;
        }

        private Object handleStop(String profile) {
            boolean stopped = profileService.stopSession(profile);
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("profile", profileService.resolveProfileName(profile));
            result.put("stopped", stopped);
            return result;
        }

        private Object handleProfiles(HttpMethod method, String path, FullHttpRequest request)
                throws Exception {
            // POST /profiles/create
            if (method == HttpMethod.POST && "/profiles/create".equals(path)) {
                Map<String, Object> reqBody = readBody(request);
                String name = reqBody != null ? (String) reqBody.get("name") : null;
                String color = reqBody != null ? (String) reqBody.get("color") : null;
                String cdpUrl = reqBody != null ? (String) reqBody.get("cdpUrl") : null;
                if (name == null || name.isBlank()) {
                    throw new BadRequestException("name is required");
                }
                BrowserProfileService.CreateProfileResult r = profileService.createProfile(name, color, cdpUrl);
                Map<String, Object> result = new LinkedHashMap<>();
                result.put("ok", r.isOk());
                result.put("profile", r.getProfile());
                result.put("cdpPort", r.getCdpPort());
                result.put("cdpUrl", r.getCdpUrl());
                result.put("color", r.getColor());
                return result;
            }

            // DELETE /profiles/{name}
            if (method == HttpMethod.DELETE && path.startsWith("/profiles/")) {
                String name = path.substring("/profiles/".length());
                if (name.isEmpty())
                    throw new BadRequestException("profile name is required");
                BrowserProfileService.DeleteProfileResult r = profileService.deleteProfile(name);
                Map<String, Object> result = new LinkedHashMap<>();
                result.put("ok", r.isOk());
                result.put("profile", r.getProfile());
                result.put("stopped", r.isStopped());
                return result;
            }

            // GET /profiles
            if (method != HttpMethod.GET)
                throw new MethodNotAllowedException();
            List<BrowserProfileService.ProfileStatus> profiles = profileService.listProfiles();
            List<Map<String, Object>> list = profiles.stream().map(p -> {
                Map<String, Object> m = new LinkedHashMap<>();
                m.put("name", p.getName());
                m.put("cdpPort", p.getCdpPort());
                m.put("cdpUrl", p.getCdpUrl());
                m.put("color", p.getColor());
                m.put("running", p.isRunning());
                m.put("isDefault", p.isDefault());
                return m;
            }).toList();
            return Map.of("profiles", list);
        }

        private Object handleTabs(HttpMethod method, String path, FullHttpRequest request,
                String profile) {
            // POST /tabs/open
            if (method == HttpMethod.POST && "/tabs/open".equals(path)) {
                Map<String, Object> reqBody = readBody(request);
                String url = reqBody != null ? (String) reqBody.get("url") : null;
                if (url == null || url.isBlank())
                    throw new BadRequestException("url is required");
                PlaywrightSession session = ensureBrowserStarted(profile);
                PlaywrightSession.TabInfo tab = session.openTab(url);
                return tabToMap(tab);
            }

            // POST /tabs/focus
            if (method == HttpMethod.POST && "/tabs/focus".equals(path)) {
                Map<String, Object> reqBody = readBody(request);
                String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
                if (targetId == null || targetId.isBlank())
                    throw new BadRequestException("targetId is required");
                PlaywrightSession session = profileService.getOrCreateSession(profile);
                session.focusTab(targetId);
                return Map.of("ok", true);
            }

            // DELETE /tabs/{targetId}
            if (method == HttpMethod.DELETE && path.startsWith("/tabs/")) {
                String targetId = path.substring("/tabs/".length());
                PlaywrightSession session = profileService.getOrCreateSession(profile);
                session.closeTab(targetId);
                return Map.of("ok", true);
            }

            // GET /tabs
            if (method != HttpMethod.GET)
                throw new MethodNotAllowedException();
            if (!profileService.isRunning(profile)) {
                return Map.of("running", false, "tabs", List.of());
            }
            PlaywrightSession session = profileService.getOrCreateSession(profile);
            List<PlaywrightSession.TabInfo> tabs = session.listTabs();
            return Map.of("running", true, "tabs", tabs.stream().map(this::tabToMap).toList());
        }

        private Object handleNavigate(FullHttpRequest request, String profile) {
            Map<String, Object> reqBody = readBody(request);
            String url = reqBody != null ? (String) reqBody.get("url") : null;
            String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
            if (url == null || url.isBlank())
                throw new BadRequestException("url is required");
            PlaywrightSession session = ensureBrowserStarted(profile);
            PlaywrightSession.NavigateResult nav = session.navigate(url, targetId);
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("url", nav.getUrl());
            result.put("title", nav.getTitle());
            return result;
        }

        private Object handleSnapshot(QueryStringDecoder decoder, String profile) {
            String targetId = firstParam(decoder, "targetId");
            PlaywrightSession session = ensureBrowserStarted(profile);
            PlaywrightSession.SnapshotResult snap = session.snapshot(targetId);
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("format", "aria");
            result.put("snapshot", snap.getSnapshot());
            result.put("url", snap.getUrl());
            result.put("title", snap.getTitle());
            return result;
        }

        private Object handleScreenshot(FullHttpRequest request, String profile) {
            Map<String, Object> reqBody = readBody(request);
            PlaywrightSession session = ensureBrowserStarted(profile);
            String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
            boolean fullPage = reqBody != null && Boolean.TRUE.equals(reqBody.get("fullPage"));
            PlaywrightSession.ScreenshotResult shot = session.screenshot(targetId, fullPage);
            String base64 = Base64.getEncoder().encodeToString(shot.getBuffer());
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("data", base64);
            result.put("contentType", shot.getContentType());
            result.put("url", shot.getUrl());
            result.put("title", shot.getTitle());
            return result;
        }

        private Object handleAct(FullHttpRequest request, String profile) {
            Map<String, Object> reqBody = readBody(request);
            String kind = reqBody != null ? (String) reqBody.get("kind") : null;
            String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
            if (kind == null || kind.isBlank())
                throw new BadRequestException("kind is required");
            PlaywrightSession session = ensureBrowserStarted(profile);
            return session.act(kind, targetId, reqBody);
        }

        private Object handleConsole(QueryStringDecoder decoder, String profile) {
            String targetId = firstParam(decoder, "targetId");
            if (!profileService.isRunning(profile)) {
                return Map.of("ok", true, "messages", List.of());
            }
            PlaywrightSession session = profileService.getOrCreateSession(profile);
            List<PlaywrightSession.ConsoleEntry> messages = session.getConsoleMessages(targetId);
            List<Map<String, Object>> mapped = messages.stream().map(e -> {
                Map<String, Object> m = new LinkedHashMap<>();
                m.put("type", e.getType());
                m.put("text", e.getText());
                m.put("timestamp", e.getTimestamp());
                return m;
            }).toList();
            return Map.of("ok", true, "messages", mapped);
        }

        // =====================================================================
        // Helpers
        // =====================================================================

        private PlaywrightSession ensureBrowserStarted(String profile) {
            PlaywrightSession session = profileService.getOrCreateSession(profile);
            if (!session.isRunning()) {
                session.launch(headless);
            }
            return session;
        }

        private Map<String, Object> tabToMap(PlaywrightSession.TabInfo tab) {
            Map<String, Object> m = new LinkedHashMap<>();
            m.put("targetId", tab.getTargetId());
            m.put("url", tab.getUrl());
            m.put("title", tab.getTitle());
            return m;
        }

        private Map<String, Object> readBody(FullHttpRequest request) {
            try {
                ByteBuf content = request.content();
                if (content.readableBytes() == 0)
                    return null;
                byte[] bytes = new byte[content.readableBytes()];
                content.readBytes(bytes);
                return mapper.readValue(bytes, new TypeReference<>() {
                });
            } catch (Exception e) {
                return null;
            }
        }

        private void sendJson(ChannelHandlerContext ctx, HttpResponseStatus status, Object body) {
            try {
                byte[] bytes = mapper.writeValueAsBytes(body);
                ByteBuf buf = Unpooled.wrappedBuffer(bytes);
                FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, status, buf);
                response.headers().set(CONTENT_TYPE, "application/json");
                response.headers().setInt(HttpHeaderNames.CONTENT_LENGTH, bytes.length);
                ctx.writeAndFlush(response);
            } catch (Exception e) {
                log.error("Failed to serialize response: {}", e.getMessage());
                ctx.close();
            }
        }
    }

    // =========================================================================
    // Query string helper
    // =========================================================================

    private static String firstParam(QueryStringDecoder decoder, String key) {
        List<String> values = decoder.parameters().get(key);
        if (values == null || values.isEmpty())
            return null;
        return values.get(0);
    }

    // =========================================================================
    // Exception types for routing
    // =========================================================================

    private static class BadRequestException extends RuntimeException {
        BadRequestException(String msg) {
            super(msg);
        }
    }

    private static class MethodNotAllowedException extends RuntimeException {
    }

    private static class NotFoundException extends RuntimeException {
    }
}
