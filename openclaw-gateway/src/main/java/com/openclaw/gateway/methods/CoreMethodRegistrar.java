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
        methodRouter.registerMethod("sessions.preview", this::handleSessionsPreview);
        methodRouter.registerMethod("sessions.resolve", this::handleSessionsResolve);
        methodRouter.registerMethod("sessions.compact", this::handleSessionsCompact);
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

    /**
     * sessions.preview — Return recent transcript preview items for given session
     * keys.
     * Corresponds to TypeScript's sessions.preview handler.
     */
    private CompletableFuture<Object> handleSessionsPreview(JsonNode params, GatewayConnection conn) {
        // Extract keys array
        List<String> keys = new ArrayList<>();
        if (params.has("keys") && params.get("keys").isArray()) {
            for (JsonNode keyNode : params.get("keys")) {
                String k = keyNode.asText("").trim();
                if (!k.isEmpty())
                    keys.add(k);
            }
        }
        int limit = params.has("limit") ? Math.max(1, params.get("limit").asInt(12)) : 12;
        int maxChars = params.has("maxChars") ? Math.max(20, params.get("maxChars").asInt(240)) : 240;

        List<Map<String, Object>> previews = new ArrayList<>();
        for (String key : keys) {
            Map<String, Object> preview = new LinkedHashMap<>();
            preview.put("key", key);

            // Find session by sessionKey
            var sessionOpt = sessionStore.findBySessionKey(key);
            if (sessionOpt.isEmpty()) {
                preview.put("status", "missing");
                preview.put("items", Collections.emptyList());
            } else {
                var session = sessionOpt.get();
                var items = transcriptStore.readPreview(session.getSessionId(), limit, maxChars);
                preview.put("status", items.isEmpty() ? "empty" : "ok");
                preview.put("items", items);
            }
            previews.add(preview);
        }

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ts", System.currentTimeMillis());
        result.put("previews", previews);
        return CompletableFuture.completedFuture(result);
    }

    /**
     * sessions.resolve — Resolve a session key from various parameters.
     * Simplified version of TypeScript's sessions.resolve handler.
     */
    private CompletableFuture<Object> handleSessionsResolve(JsonNode params, GatewayConnection conn) {
        String agentId = getTextParam(params, "agentId", "default");
        String channelId = getTextParam(params, "channelId", null);
        String peerId = getTextParam(params, "peerId", null);

        // Build a session key from components
        StringBuilder keyBuilder = new StringBuilder();
        if (agentId != null && !"default".equals(agentId)) {
            keyBuilder.append(agentId).append(":");
        }
        if (channelId != null) {
            keyBuilder.append(channelId);
            if (peerId != null) {
                keyBuilder.append(":").append(peerId);
            }
        } else {
            keyBuilder.append("main");
        }

        String resolvedKey = keyBuilder.toString();
        return CompletableFuture.completedFuture(Map.of("ok", true, "key", resolvedKey));
    }

    /**
     * sessions.compact — Compact a session transcript to keep only recent lines.
     * Corresponds to TypeScript's sessions.compact handler.
     */
    private CompletableFuture<Object> handleSessionsCompact(JsonNode params, GatewayConnection conn) {
        String sessionId = getTextParam(params, "sessionId", null);
        String sessionKey = getTextParam(params, "key", null);
        int maxLines = params.has("maxLines") ? Math.max(1, params.get("maxLines").asInt(400)) : 400;

        // Resolve session
        Optional<AcpSession> sessionOpt;
        if (sessionId != null) {
            sessionOpt = sessionStore.getSession(sessionId);
        } else if (sessionKey != null) {
            sessionOpt = sessionStore.findBySessionKey(sessionKey);
        } else {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("key or sessionId is required"));
        }

        if (sessionOpt.isEmpty()) {
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("compacted", false);
            result.put("reason", "session not found");
            return CompletableFuture.completedFuture(result);
        }

        String sid = sessionOpt.get().getSessionId();
        try {
            var compactResult = transcriptStore.compact(sid, maxLines);

            // Reset token counters after compaction
            if (compactResult.isCompacted()) {
                sessionStore.updateSession(sid, s -> {
                    s.setInputTokens(0);
                    s.setOutputTokens(0);
                    s.setTotalTokens(0);
                });
            }

            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("compacted", compactResult.isCompacted());
            result.put("kept", compactResult.getKeptLines());
            if (compactResult.getArchived() != null) {
                result.put("archived", compactResult.getArchived());
            }
            return CompletableFuture.completedFuture(result);
        } catch (Exception e) {
            log.warn("Failed to compact transcript for session {}: {}", sid, e.getMessage());
            return CompletableFuture.failedFuture(e);
        }
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

    // --- Helpers ---

    private static String getTextParam(JsonNode params, String field, String defaultValue) {
        if (params != null && params.has(field) && !params.get(field).isNull()) {
            String value = params.get(field).asText("").trim();
            return value.isEmpty() ? defaultValue : value;
        }
        return defaultValue;
    }
}
