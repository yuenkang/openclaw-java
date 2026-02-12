package com.openclaw.gateway.methods;

import com.fasterxml.jackson.databind.JsonNode;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.common.model.AcpSession;
import com.openclaw.gateway.routing.RouteResolver;
import com.openclaw.gateway.session.SessionStore;
import com.openclaw.gateway.session.SessionTranscriptStore;
import com.openclaw.gateway.websocket.GatewayConnection;
import com.openclaw.gateway.websocket.GatewayMethodRouter;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Registers core Gateway method handlers.
 * Corresponds to TypeScript's server-methods/*.ts files.
 *
 * <p>
 * Note: auth.pair is replaced by the connect handshake protocol.
 * Authentication is now handled in GatewayWebSocketHandler.handleConnect().
 */
@Slf4j
@Component
public class CoreMethodRegistrar {

    private static final long START_TIME = System.currentTimeMillis();

    private final GatewayMethodRouter methodRouter;
    private final SessionStore sessionStore;
    private final SessionTranscriptStore transcriptStore;
    private final ConfigService configService;
    private final RouteResolver routeResolver;
    private final ModelCatalog modelCatalog;

    public CoreMethodRegistrar(
            GatewayMethodRouter methodRouter,
            SessionStore sessionStore,
            SessionTranscriptStore transcriptStore,
            ConfigService configService,
            RouteResolver routeResolver,
            ModelCatalog modelCatalog) {
        this.methodRouter = methodRouter;
        this.sessionStore = sessionStore;
        this.transcriptStore = transcriptStore;
        this.configService = configService;
        this.routeResolver = routeResolver;
        this.modelCatalog = modelCatalog;
    }

    @PostConstruct
    public void registerMethods() {
        // Status / health
        methodRouter.registerMethod("status", this::handleStatus);
        methodRouter.registerMethod("health", this::handleStatus);

        // Models
        methodRouter.registerMethod("models.list", this::handleModelsList);

        // Config
        methodRouter.registerMethod("config.get", this::handleConfigGet);

        // Session management
        methodRouter.registerMethod("sessions.list", this::handleSessionList);
        methodRouter.registerMethod("session.create", this::handleSessionCreate);
        methodRouter.registerMethod("session.get", this::handleSessionGet);
        methodRouter.registerMethod("session.cancel", this::handleSessionCancel);
        methodRouter.registerMethod("session.patch", this::handleSessionPatch);
        methodRouter.registerMethod("session.delete", this::handleSessionDelete);
        methodRouter.registerMethod("session.reset", this::handleSessionReset);

        // Agent management
        methodRouter.registerMethod("agent.list", this::handleAgentList);
        methodRouter.registerMethod("agent.identity.get", this::handleAgentIdentityGet);

        // Route resolution
        methodRouter.registerMethod("route.resolve", this::handleRouteResolve);

        log.info("Registered {} core Gateway methods", methodRouter.getRegisteredMethods().size());
    }

    // --- Status / Health ---

    private CompletableFuture<Object> handleStatus(JsonNode params, GatewayConnection conn) {
        Runtime rt = Runtime.getRuntime();
        Map<String, Object> status = new LinkedHashMap<>();
        status.put("status", "ok");
        status.put("version", "0.1.0");
        status.put("uptimeMs", System.currentTimeMillis() - START_TIME);
        status.put("sessions", sessionStore.size());
        status.put("providers", modelCatalog.listProviders().size());

        Map<String, Object> runtime = new LinkedHashMap<>();
        runtime.put("javaVersion", System.getProperty("java.version"));
        runtime.put("os", System.getProperty("os.name") + " " + System.getProperty("os.arch"));
        runtime.put("maxMemoryMb", rt.maxMemory() / (1024 * 1024));
        runtime.put("usedMemoryMb", (rt.totalMemory() - rt.freeMemory()) / (1024 * 1024));
        runtime.put("availableProcessors", rt.availableProcessors());
        status.put("runtime", runtime);

        return CompletableFuture.completedFuture(status);
    }

    // --- Models ---

    private CompletableFuture<Object> handleModelsList(JsonNode params, GatewayConnection conn) {
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("models", modelCatalog.listModels());
        result.put("providers", modelCatalog.listProviders());
        result.put("aliases", modelCatalog.listAliases());
        return CompletableFuture.completedFuture(result);
    }

    // --- Config ---

    private CompletableFuture<Object> handleConfigGet(JsonNode params, GatewayConnection conn) {
        OpenClawConfig config = configService.loadConfig();
        // Redact sensitive auth fields
        if (config.getGateway() != null && config.getGateway().getAuth() != null) {
            config.getGateway().getAuth().setToken(null);
            config.getGateway().getAuth().setPassword(null);
        }
        return CompletableFuture.completedFuture(config);
    }

    // --- Session Methods ---

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

    /**
     * session.get — Retrieve a session by sessionId or sessionKey.
     */
    private CompletableFuture<Object> handleSessionGet(JsonNode params, GatewayConnection conn) {
        String sessionId = getTextParam(params, "sessionId", null);
        String sessionKey = getTextParam(params, "sessionKey", null);

        if (sessionId == null && sessionKey == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("sessionId or sessionKey is required"));
        }

        Optional<AcpSession> session;
        if (sessionId != null) {
            session = sessionStore.getSession(sessionId);
        } else {
            session = sessionStore.findBySessionKey(sessionKey);
        }

        if (session.isEmpty()) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("Session not found"));
        }

        return CompletableFuture.completedFuture(session.get());
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

    /**
     * session.patch — Update session metadata (model, label, agentId,
     * thinkingLevel, etc.).
     */
    private CompletableFuture<Object> handleSessionPatch(JsonNode params, GatewayConnection conn) {
        String sessionId = getTextParam(params, "sessionId", null);
        String sessionKey = getTextParam(params, "sessionKey", null);

        if (sessionId == null && sessionKey == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("sessionId or sessionKey is required"));
        }

        // Find session
        Optional<AcpSession> sessionOpt;
        if (sessionId != null) {
            sessionOpt = sessionStore.getSession(sessionId);
        } else {
            sessionOpt = sessionStore.findBySessionKey(sessionKey);
        }
        if (sessionOpt.isEmpty()) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("Session not found"));
        }

        AcpSession session = sessionOpt.get();
        String sid = session.getSessionId();

        // Apply patches
        boolean updated = sessionStore.updateSession(sid, s -> {
            if (params.has("model") && !params.get("model").isNull()) {
                s.setModel(params.get("model").asText());
            }
            if (params.has("label") && !params.get("label").isNull()) {
                s.setLabel(params.get("label").asText());
            }
            if (params.has("agentId") && !params.get("agentId").isNull()) {
                s.setAgentId(params.get("agentId").asText());
            }
            if (params.has("thinkingLevel") && !params.get("thinkingLevel").isNull()) {
                s.setThinkingLevel(params.get("thinkingLevel").asText());
            }
            if (params.has("cwd") && !params.get("cwd").isNull()) {
                s.setCwd(params.get("cwd").asText());
            }
        });

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", updated);
        result.put("session", sessionStore.getSession(sid).orElse(null));
        return CompletableFuture.completedFuture(result);
    }

    /**
     * session.delete — Delete a session and optionally its transcript.
     */
    private CompletableFuture<Object> handleSessionDelete(JsonNode params, GatewayConnection conn) {
        String sessionId = getTextParam(params, "sessionId", null);
        String sessionKey = getTextParam(params, "sessionKey", null);

        if (sessionId == null && sessionKey == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("sessionId or sessionKey is required"));
        }

        // Find session
        Optional<AcpSession> sessionOpt;
        if (sessionId != null) {
            sessionOpt = sessionStore.getSession(sessionId);
        } else {
            sessionOpt = sessionStore.findBySessionKey(sessionKey);
        }

        if (sessionOpt.isEmpty()) {
            return CompletableFuture.completedFuture(Map.of("ok", true, "deleted", false));
        }

        String sid = sessionOpt.get().getSessionId();

        // Cancel any active run first
        sessionStore.cancelRun(sid);

        // Delete transcript
        boolean transcriptDeleted = transcriptStore.deleteTranscript(sid);

        // Remove from store
        boolean removed = sessionStore.removeSession(sid);

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("deleted", removed);
        result.put("transcriptDeleted", transcriptDeleted);
        return CompletableFuture.completedFuture(result);
    }

    /**
     * session.reset — Reset session history (clear transcript, keep session
     * config).
     */
    private CompletableFuture<Object> handleSessionReset(JsonNode params, GatewayConnection conn) {
        String sessionId = getTextParam(params, "sessionId", null);
        String sessionKey = getTextParam(params, "sessionKey", null);

        if (sessionId == null && sessionKey == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("sessionId or sessionKey is required"));
        }

        Optional<AcpSession> sessionOpt;
        if (sessionId != null) {
            sessionOpt = sessionStore.getSession(sessionId);
        } else {
            sessionOpt = sessionStore.findBySessionKey(sessionKey);
        }

        if (sessionOpt.isEmpty()) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("Session not found"));
        }

        String sid = sessionOpt.get().getSessionId();

        // Reset token counters
        sessionStore.updateSession(sid, s -> {
            s.setInputTokens(0);
            s.setOutputTokens(0);
            s.setTotalTokens(0);
        });

        // Reset transcript
        try {
            transcriptStore.resetTranscript(sid);
        } catch (Exception e) {
            log.warn("Failed to reset transcript for session {}: {}", sid, e.getMessage());
        }

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("session", sessionStore.getSession(sid).orElse(null));
        return CompletableFuture.completedFuture(result);
    }

    // --- Agent Methods ---

    /**
     * agent.list — List all configured agents.
     */
    private CompletableFuture<Object> handleAgentList(JsonNode params, GatewayConnection conn) {
        OpenClawConfig config = configService.loadConfig();
        var agents = config.getAgents();
        if (agents == null || agents.getList() == null) {
            return CompletableFuture.completedFuture(Map.of("agents", Collections.emptyList()));
        }

        List<Map<String, Object>> agentList = new ArrayList<>();
        for (var agent : agents.getList()) {
            Map<String, Object> entry = new LinkedHashMap<>();
            entry.put("id", agent.getId());
            entry.put("name", agent.getName());
            entry.put("model", agent.getModel());
            if (agent.getDescription() != null) {
                entry.put("description", agent.getDescription());
            }
            agentList.add(entry);
        }

        return CompletableFuture.completedFuture(Map.of("agents", agentList));
    }

    /**
     * agent.identity.get — Get agent identity information.
     */
    private CompletableFuture<Object> handleAgentIdentityGet(JsonNode params, GatewayConnection conn) {
        String agentId = getTextParam(params, "agentId", "default");

        OpenClawConfig config = configService.loadConfig();
        var agents = config.getAgents();
        if (agents == null || agents.getList() == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("No agents configured"));
        }

        var agent = agents.getList().stream()
                .filter(a -> agentId.equals(a.getId()))
                .findFirst();

        if (agent.isEmpty()) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("Agent not found: " + agentId));
        }

        var a = agent.get();
        Map<String, Object> identity = new LinkedHashMap<>();
        identity.put("agentId", a.getId());
        identity.put("name", a.getName());
        identity.put("model", a.getModel());
        if (a.getDescription() != null) {
            identity.put("description", a.getDescription());
        }

        return CompletableFuture.completedFuture(identity);
    }

    // --- Route ---

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

    // --- Helpers ---

    private static String getTextParam(JsonNode params, String field, String defaultValue) {
        if (params != null && params.has(field) && !params.get(field).isNull()) {
            String value = params.get(field).asText("").trim();
            return value.isEmpty() ? defaultValue : value;
        }
        return defaultValue;
    }
}
