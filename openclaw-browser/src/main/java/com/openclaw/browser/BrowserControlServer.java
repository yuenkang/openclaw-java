package com.openclaw.browser;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.browser.routes.*;
import com.openclaw.browser.server.DualChannelBridge;
import com.openclaw.common.config.PortDefaults;
import io.netty.bootstrap.ServerBootstrap;
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

import java.util.concurrent.ConcurrentHashMap;

import static io.netty.handler.codec.http.HttpHeaderNames.CONTENT_TYPE;
import static io.netty.handler.codec.http.HttpResponseStatus.*;
import static io.netty.handler.codec.http.HttpVersion.HTTP_1_1;

/**
 * Standalone Netty HTTP server for browser control, listening on port 18791.
 * Corresponds to TypeScript's browser/server.ts (Express on controlPort).
 *
 * <p>
 * All browser operations are delegated to {@link BrowserProfileService} via
 * the route handler classes in {@link com.openclaw.browser.routes}.
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

    /** Per-profile dual-channel bridges (profile name → bridge). */
    private final ConcurrentHashMap<String, DualChannelBridge> bridges = new ConcurrentHashMap<>();

    /** Shared route context for all route handlers. */
    private final RouteContext routeContext;

    public BrowserControlServer(BrowserProfileService profileService) {
        this.profileService = profileService;
        this.routeContext = new RouteContext(profileService, bridges);
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
    // Netty Handler — thin router that delegates to Route classes
    // =========================================================================

    private class BrowserControlHandler extends SimpleChannelInboundHandler<FullHttpRequest> {

        @Override
        protected void channelRead0(ChannelHandlerContext ctx, FullHttpRequest request) {
            QueryStringDecoder decoder = new QueryStringDecoder(request.uri());
            String path = decoder.path();
            HttpMethod method = request.method();
            String profile = RouteContext.firstParam(decoder, "profile");

            try {
                Object result = route(method, path, decoder, request, profile);
                sendJson(ctx, OK, result);
            } catch (RouteContext.BadRequestException e) {
                sendJson(ctx, BAD_REQUEST,
                        java.util.Map.of("ok", false, "error", e.getMessage()));
            } catch (RouteContext.MethodNotAllowedException e) {
                sendJson(ctx, METHOD_NOT_ALLOWED,
                        java.util.Map.of("ok", false, "error", "method not allowed"));
            } catch (RouteContext.NotFoundException e) {
                sendJson(ctx, NOT_FOUND,
                        java.util.Map.of("ok", false, "error", "not found: " + path));
            } catch (Exception e) {
                log.error("Browser control error on {}: {}", path, e.getMessage(), e);
                sendJson(ctx, INTERNAL_SERVER_ERROR,
                        java.util.Map.of("ok", false, "error", e.getMessage()));
            }
        }

        @Override
        public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
            log.error("Netty channel error: {}", cause.getMessage(), cause);
            ctx.close();
        }

        // =================================================================
        // Router — dispatches to Route classes
        // =================================================================

        private Object route(HttpMethod method, String path, QueryStringDecoder decoder,
                             FullHttpRequest request, String profile) throws Exception {

            // ----- Basic lifecycle -----
            if ("/".equals(path) && method == HttpMethod.GET)
                return BasicRoutes.handleStatus(routeContext, profile);
            if ("/start".equals(path) && method == HttpMethod.POST)
                return BasicRoutes.handleStart(routeContext, request, profile);
            if ("/stop".equals(path) && method == HttpMethod.POST)
                return BasicRoutes.handleStop(routeContext, profile);
            if (path.startsWith("/profiles"))
                return BasicRoutes.handleProfiles(routeContext, method, path, request);
            if ("/channels".equals(path) && method == HttpMethod.GET)
                return BasicRoutes.handleChannels(routeContext, profile);
            if ("/reset-profile".equals(path) && method == HttpMethod.POST)
                return BasicRoutes.handleResetProfile(routeContext, profile);

            // ----- Tabs -----
            if (path.startsWith("/tabs"))
                return TabRoutes.handle(routeContext, method, path, request, profile);

            // ----- Navigation / Actions -----
            if ("/navigate".equals(path) && method == HttpMethod.POST)
                return ActRoutes.handleNavigate(routeContext, request, profile);
            if ("/act".equals(path) && method == HttpMethod.POST)
                return ActRoutes.handleAct(routeContext, request, profile);
            if ("/console".equals(path) && method == HttpMethod.GET)
                return ActRoutes.handleConsole(routeContext, decoder, profile);

            // ----- Snapshot / Screenshot -----
            if ("/snapshot".equals(path) && method == HttpMethod.GET)
                return SnapshotRoutes.handleSnapshot(routeContext, decoder, profile);
            if ("/screenshot".equals(path) && method == HttpMethod.POST)
                return SnapshotRoutes.handleScreenshot(routeContext, request, profile);
            if ("/screenshot-labels".equals(path) && method == HttpMethod.POST)
                return SnapshotRoutes.handleScreenshotWithLabels(routeContext, request, profile);

            // ----- Cookies -----
            if ("/cookies".equals(path) && method == HttpMethod.GET)
                return StorageRoutes.handleCookies(routeContext, decoder, profile);
            if ("/cookies".equals(path) && method == HttpMethod.POST)
                return StorageRoutes.handleCookiesSet(routeContext, request, profile);
            if ("/cookies".equals(path) && method == HttpMethod.DELETE)
                return StorageRoutes.handleCookiesClear(routeContext, profile);

            // ----- Storage -----
            if ("/storage".equals(path) && method == HttpMethod.GET)
                return StorageRoutes.handleStorageGet(routeContext, decoder, profile);
            if ("/storage".equals(path) && method == HttpMethod.POST)
                return StorageRoutes.handleStorageSet(routeContext, request, profile);
            if ("/storage".equals(path) && method == HttpMethod.DELETE)
                return StorageRoutes.handleStorageClear(routeContext, decoder, profile);

            // ----- Debug / Observe -----
            if ("/errors".equals(path) && method == HttpMethod.GET)
                return HooksRoutes.handleErrors(routeContext, decoder, profile);
            if ("/requests".equals(path) && method == HttpMethod.GET)
                return HooksRoutes.handleRequests(routeContext, decoder, profile);
            if ("/highlight".equals(path) && method == HttpMethod.POST)
                return HooksRoutes.handleHighlight(routeContext, request, profile);

            // ----- PDF -----
            if ("/pdf".equals(path) && method == HttpMethod.POST)
                return HooksRoutes.handlePdf(routeContext, request, profile);

            // ----- Hooks -----
            if ("/hooks/dialog".equals(path) && method == HttpMethod.POST)
                return HooksRoutes.handleHooksDialog(routeContext, request, profile);
            if ("/hooks/file-chooser".equals(path) && method == HttpMethod.POST)
                return HooksRoutes.handleHooksFileChooser(routeContext, request, profile);
            if ("/hooks/arm-upload".equals(path) && method == HttpMethod.POST)
                return HooksRoutes.handleArmUpload(routeContext, request, profile);
            if ("/hooks/arm-dialog".equals(path) && method == HttpMethod.POST)
                return HooksRoutes.handleArmDialog(routeContext, request, profile);

            // ----- Console -----
            if ("/console".equals(path) && method == HttpMethod.GET)
                return HooksRoutes.handleConsole(routeContext, decoder, profile);

            // ----- Trace -----
            if ("/trace/start".equals(path) && method == HttpMethod.POST)
                return HooksRoutes.handleTraceStart(routeContext, request, profile);
            if ("/trace/stop".equals(path) && method == HttpMethod.POST)
                return HooksRoutes.handleTraceStop(routeContext, request, profile);

            // ----- State -----
            if ("/state/offline".equals(path) && method == HttpMethod.POST)
                return StateRoutes.handleSetOffline(routeContext, request, profile);
            if ("/state/headers".equals(path) && method == HttpMethod.POST)
                return StateRoutes.handleSetHeaders(routeContext, request, profile);
            if ("/state/credentials".equals(path) && method == HttpMethod.POST)
                return StateRoutes.handleSetCredentials(routeContext, request, profile);
            if ("/state/geolocation".equals(path) && method == HttpMethod.POST)
                return StateRoutes.handleSetGeolocation(routeContext, request, profile);
            if ("/state/media".equals(path) && method == HttpMethod.POST)
                return StateRoutes.handleSetMedia(routeContext, request, profile);

            // ----- Response Body -----
            if ("/response/body".equals(path) && method == HttpMethod.POST)
                return HooksRoutes.handleResponseBody(routeContext, request, profile);

            // ----- Resize -----
            if ("/resize".equals(path) && method == HttpMethod.POST)
                return StateRoutes.handleResize(routeContext, request, profile);

            throw new RouteContext.NotFoundException();
        }

        // =================================================================
        // JSON response helper
        // =================================================================

        private void sendJson(ChannelHandlerContext ctx, HttpResponseStatus status, Object body) {
            try {
                byte[] bytes = mapper.writeValueAsBytes(body);
                var buf = Unpooled.wrappedBuffer(bytes);
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
}
