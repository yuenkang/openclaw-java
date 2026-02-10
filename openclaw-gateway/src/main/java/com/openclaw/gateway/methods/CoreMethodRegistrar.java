package com.openclaw.gateway.methods;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.gateway.routing.RouteResolver;
import com.openclaw.gateway.session.SessionStore;
import com.openclaw.gateway.websocket.GatewayConnection;
import com.openclaw.gateway.websocket.GatewayMethodRouter;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Registers core Gateway JSON-RPC methods.
 * Corresponds to TypeScript's server-methods/*.ts files.
 */
@Slf4j
@Component
public class CoreMethodRegistrar {

    private final GatewayMethodRouter methodRouter;
    private final SessionStore sessionStore;
    private final ConfigService configService;
    private final RouteResolver routeResolver;
    private final ObjectMapper objectMapper;

    public CoreMethodRegistrar(
            GatewayMethodRouter methodRouter,
            SessionStore sessionStore,
            ConfigService configService,
            RouteResolver routeResolver,
            ObjectMapper objectMapper) {
        this.methodRouter = methodRouter;
        this.sessionStore = sessionStore;
        this.configService = configService;
        this.routeResolver = routeResolver;
        this.objectMapper = objectMapper;
    }

    @PostConstruct
    public void registerMethods() {
        // Status / health
        methodRouter.registerMethod("status", this::handleStatus);

        // Config
        methodRouter.registerMethod("config.get", this::handleConfigGet);

        // Session management
        methodRouter.registerMethod("session.list", this::handleSessionList);
        methodRouter.registerMethod("session.create", this::handleSessionCreate);
        methodRouter.registerMethod("session.cancel", this::handleSessionCancel);

        // Route resolution
        methodRouter.registerMethod("route.resolve", this::handleRouteResolve);

        log.info("Registered {} core Gateway methods", methodRouter.getRegisteredMethods().size());
    }

    // --- Method handlers ---

    private CompletableFuture<Object> handleStatus(JsonNode params, GatewayConnection conn) {
        Map<String, Object> status = new LinkedHashMap<>();
        status.put("status", "ok");
        status.put("version", "0.1.0");
        status.put("sessions", sessionStore.size());
        status.put("uptime", System.currentTimeMillis());
        return CompletableFuture.completedFuture(status);
    }

    private CompletableFuture<Object> handleConfigGet(JsonNode params, GatewayConnection conn) {
        OpenClawConfig config = configService.loadConfig();
        // Redact sensitive fields
        if (config.getGateway() != null) {
            config.getGateway().setPassword(null);
            config.getGateway().setToken(null);
        }
        return CompletableFuture.completedFuture(config);
    }

    private CompletableFuture<Object> handleSessionList(JsonNode params, GatewayConnection conn) {
        return CompletableFuture.completedFuture(sessionStore.listSessions());
    }

    private CompletableFuture<Object> handleSessionCreate(JsonNode params, GatewayConnection conn) {
        String sessionKey = params.has("sessionKey") ? params.get("sessionKey").asText() : null;
        String cwd = params.has("cwd") ? params.get("cwd").asText() : System.getProperty("user.dir");

        if (sessionKey == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("sessionKey is required"));
        }

        // Check if session already exists
        var existing = sessionStore.findBySessionKey(sessionKey);
        if (existing.isPresent()) {
            return CompletableFuture.completedFuture(existing.get());
        }

        var session = sessionStore.createSession(sessionKey, cwd);
        return CompletableFuture.completedFuture(session);
    }

    private CompletableFuture<Object> handleSessionCancel(JsonNode params, GatewayConnection conn) {
        String sessionId = params.has("sessionId") ? params.get("sessionId").asText() : null;
        if (sessionId == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("sessionId is required"));
        }

        boolean cancelled = sessionStore.cancelRun(sessionId);
        return CompletableFuture.completedFuture(Map.of("cancelled", cancelled));
    }

    private CompletableFuture<Object> handleRouteResolve(JsonNode params, GatewayConnection conn) {
        RouteResolver.RouteContext ctx = RouteResolver.RouteContext.builder()
                .channelId(params.has("channelId") ? params.get("channelId").asText() : null)
                .accountId(params.has("accountId") ? params.get("accountId").asText() : null)
                .peerId(params.has("peerId") ? params.get("peerId").asText() : null)
                .guildId(params.has("guildId") ? params.get("guildId").asText() : null)
                .build();

        var route = routeResolver.resolve(ctx);
        return CompletableFuture.completedFuture(route.orElse(null));
    }
}
