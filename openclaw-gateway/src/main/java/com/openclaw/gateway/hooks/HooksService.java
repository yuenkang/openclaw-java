package com.openclaw.gateway.hooks;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.common.config.ConfigService;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * Inbound webhook handler: validates token, parses payload, dispatches
 * wake / agent actions based on configured hook mappings.
 * <p>
 * Corresponds to TypeScript's {@code hooks.ts} (234 lines) +
 * {@code hooks-mapping.ts} (439 lines).
 */
@Slf4j
public class HooksService {

    private static final String DEFAULT_HOOKS_PATH = "/hooks";
    private static final int DEFAULT_MAX_BODY_BYTES = 256 * 1024;

    private final ConfigService configService;
    private final ObjectMapper objectMapper;
    private final HookActionHandler actionHandler;

    public HooksService(ConfigService configService, ObjectMapper objectMapper,
            HookActionHandler actionHandler) {
        this.configService = configService;
        this.objectMapper = objectMapper;
        this.actionHandler = actionHandler;
    }

    // ─── Config resolution ──────────────────────────────────────────────

    /**
     * Resolve and validate hooks config from the main config.
     *
     * @return resolved config or null if hooks are not enabled
     */
    public HooksConfigResolved resolveConfig() {
        var cfg = configService.loadConfig();
        var pluginsCfg = cfg.getPlugins();
        var hooksEntry = pluginsCfg != null && pluginsCfg.getEntries() != null
                ? pluginsCfg.getEntries().get("hooks")
                : null;
        @SuppressWarnings("unchecked")
        Map<?, ?> hooks = hooksEntry != null && hooksEntry.getConfig() != null
                ? (Map<?, ?>) hooksEntry.getConfig()
                : null;
        if (hooks == null || !Boolean.TRUE.equals(hooks.get("enabled"))) {
            return null;
        }

        String token = hooks.get("token") instanceof String t ? t.trim() : "";
        if (token.isEmpty()) {
            throw new IllegalStateException("hooks.enabled requires hooks.token");
        }

        String rawPath = hooks.get("path") instanceof String p && !p.isBlank()
                ? p.trim()
                : DEFAULT_HOOKS_PATH;
        String basePath = rawPath.startsWith("/") ? rawPath : "/" + rawPath;
        if (basePath.length() > 1) {
            basePath = basePath.replaceAll("/+$", "");
        }
        if ("/".equals(basePath)) {
            throw new IllegalStateException("hooks.path may not be '/'");
        }

        int maxBody = hooks.get("maxBodyBytes") instanceof Number n && n.intValue() > 0
                ? n.intValue()
                : DEFAULT_MAX_BODY_BYTES;

        return new HooksConfigResolved(basePath, token, maxBody);
    }

    // ─── Token extraction ───────────────────────────────────────────────

    /**
     * Extract the authentication token from the request.
     */
    public static String extractHookToken(HttpServletRequest request) {
        String auth = request.getHeader("Authorization");
        if (auth != null && auth.toLowerCase().startsWith("bearer ")) {
            String t = auth.substring(7).trim();
            if (!t.isEmpty())
                return t;
        }
        String headerToken = request.getHeader("X-OpenClaw-Token");
        if (headerToken != null && !headerToken.isBlank()) {
            return headerToken.trim();
        }
        return null;
    }

    // ─── Request handling ───────────────────────────────────────────────

    /**
     * Handle an inbound hook HTTP request.
     */
    public void handleRequest(HttpServletRequest request, HttpServletResponse response)
            throws IOException {
        var config = resolveConfig();
        if (config == null) {
            response.sendError(404, "Hooks not enabled");
            return;
        }

        // Verify token
        String token = extractHookToken(request);
        if (token == null || !token.equals(config.token())) {
            response.sendError(401, "Invalid or missing token");
            return;
        }

        // Read JSON body
        byte[] body = request.getInputStream().readNBytes(config.maxBodyBytes() + 1);
        if (body.length > config.maxBodyBytes()) {
            response.sendError(413, "Payload too large");
            return;
        }

        Map<String, Object> payload;
        try {
            String raw = new String(body, StandardCharsets.UTF_8).trim();
            if (raw.isEmpty()) {
                payload = Map.of();
            } else {
                @SuppressWarnings("unchecked")
                var parsed = (Map<String, Object>) objectMapper.readValue(raw, Map.class);
                payload = parsed;
            }
        } catch (Exception e) {
            response.sendError(400, "Invalid JSON: " + e.getMessage());
            return;
        }

        // Determine action from path suffix
        String requestPath = request.getRequestURI();
        String actionPath = requestPath.substring(config.basePath().length());
        if (actionPath.startsWith("/"))
            actionPath = actionPath.substring(1);

        HookAction action = resolveAction(actionPath, payload);
        if (action == null) {
            response.sendError(400, "Unknown hook action: " + actionPath);
            return;
        }

        // Dispatch
        try {
            actionHandler.handle(action);
            response.setContentType("application/json");
            response.setStatus(200);
            response.getWriter().write("{\"ok\":true}");
        } catch (Exception e) {
            log.error("Hook action failed: {}", e.getMessage(), e);
            response.sendError(500, "Hook action failed: " + e.getMessage());
        }
    }

    // ─── Action resolution ──────────────────────────────────────────────

    private HookAction resolveAction(String path, Map<String, Object> payload) {
        return switch (path) {
            case "wake" -> resolveWakeAction(payload);
            case "agent" -> resolveAgentAction(payload);
            default -> null;
        };
    }

    private HookAction resolveWakeAction(Map<String, Object> payload) {
        String text = payload.get("text") instanceof String t ? t.trim() : "";
        if (text.isEmpty())
            return null;
        String mode = "next-heartbeat".equals(payload.get("mode"))
                ? "next-heartbeat"
                : "now";
        return new HookAction("wake", text, mode, null, null, null, true);
    }

    private HookAction resolveAgentAction(Map<String, Object> payload) {
        String message = payload.get("message") instanceof String m ? m.trim() : "";
        if (message.isEmpty())
            return null;
        String name = payload.get("name") instanceof String n && !n.isBlank()
                ? n.trim()
                : "Hook";
        String wakeMode = "next-heartbeat".equals(payload.get("wakeMode"))
                ? "next-heartbeat"
                : "now";
        String sessionKey = payload.get("sessionKey") instanceof String sk && !sk.isBlank()
                ? sk.trim()
                : "hook:" + UUID.randomUUID();
        String model = payload.get("model") instanceof String md && !md.isBlank()
                ? md.trim()
                : null;
        boolean deliver = !Boolean.FALSE.equals(payload.get("deliver"));

        return new HookAction("agent", message, wakeMode, sessionKey, name, model, deliver);
    }

    // ─── Types ──────────────────────────────────────────────────────────

    public record HooksConfigResolved(String basePath, String token, int maxBodyBytes) {
    }

    public record HookAction(
            String kind,
            String textOrMessage,
            String mode,
            String sessionKey,
            String name,
            String model,
            boolean deliver) {
    }

    @FunctionalInterface
    public interface HookActionHandler {
        void handle(HookAction action) throws Exception;
    }
}
