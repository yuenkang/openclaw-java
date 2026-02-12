package com.openclaw.gateway.methods;

import com.fasterxml.jackson.databind.JsonNode;
import com.openclaw.gateway.websocket.GatewayConnection;
import com.openclaw.gateway.websocket.GatewayMethodRouter;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Registers wizard.* RPC method handlers.
 * Corresponds to TypeScript's server-methods/wizard.ts.
 *
 * Methods: wizard.start, wizard.next, wizard.cancel, wizard.status
 *
 * Wizard sessions are held in memory. Each session has a
 * state machine: "running" → ("completed" | "cancelled" | "error").
 */
@Slf4j
public class WizardMethodRegistrar {

    private final GatewayMethodRouter router;
    private final Map<String, WizardSession> sessions = new ConcurrentHashMap<>();

    public WizardMethodRegistrar(GatewayMethodRouter router) {
        this.router = router;
    }

    public void registerMethods() {
        router.registerMethod("wizard.start", this::handleStart);
        router.registerMethod("wizard.next", this::handleNext);
        router.registerMethod("wizard.cancel", this::handleCancel);
        router.registerMethod("wizard.status", this::handleStatus);
        log.info("Registered 4 wizard methods");
    }

    // ==================== Handlers ====================

    private CompletableFuture<Object> handleStart(JsonNode params, GatewayConnection conn) {
        // Check if a wizard is already running
        for (var entry : sessions.entrySet()) {
            if ("running".equals(entry.getValue().status)) {
                return CompletableFuture.failedFuture(
                        new IllegalStateException("wizard already running"));
            }
        }

        String sessionId = UUID.randomUUID().toString();
        String mode = params.path("mode").asText("default");
        String workspace = params.path("workspace").isTextual()
                ? params.get("workspace").asText()
                : null;

        WizardSession session = new WizardSession();
        session.sessionId = sessionId;
        session.status = "running";
        session.mode = mode;
        session.workspace = workspace;
        session.createdAtMs = System.currentTimeMillis();

        sessions.put(sessionId, session);

        // Return initial step (placeholder — real wizard runner integration is Phase
        // 4+)
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("sessionId", sessionId);
        result.put("status", "running");
        result.put("done", false);
        result.put("step", Map.of(
                "stepId", "init",
                "type", "info",
                "title", "Wizard started",
                "description", "Wizard session '" + mode + "' initialized"));

        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleNext(JsonNode params, GatewayConnection conn) {
        String sessionId = params.path("sessionId").asText(null);
        if (sessionId == null)
            return failedResult("sessionId required");

        WizardSession session = sessions.get(sessionId);
        if (session == null)
            return failedResult("wizard not found");
        if (!"running".equals(session.status))
            return failedResult("wizard not running");

        // Process answer if provided
        JsonNode answer = params.path("answer");
        if (!answer.isMissingNode() && !answer.isNull()) {
            session.lastAnswer = answer.toString();
        }

        // Mark as completed (simplified — real multi-step wizard is Phase 4+)
        session.status = "completed";
        sessions.remove(sessionId);

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("status", "completed");
        result.put("done", true);
        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleCancel(JsonNode params, GatewayConnection conn) {
        String sessionId = params.path("sessionId").asText(null);
        if (sessionId == null)
            return failedResult("sessionId required");

        WizardSession session = sessions.get(sessionId);
        if (session == null)
            return failedResult("wizard not found");

        session.status = "cancelled";
        sessions.remove(sessionId);

        return CompletableFuture.completedFuture(Map.of(
                "status", "cancelled"));
    }

    private CompletableFuture<Object> handleStatus(JsonNode params, GatewayConnection conn) {
        String sessionId = params.path("sessionId").asText(null);
        if (sessionId == null)
            return failedResult("sessionId required");

        WizardSession session = sessions.get(sessionId);
        if (session == null)
            return failedResult("wizard not found");

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("status", session.status);
        if (session.error != null)
            result.put("error", session.error);

        if (!"running".equals(session.status)) {
            sessions.remove(sessionId);
        }

        return CompletableFuture.completedFuture(result);
    }

    // ==================== Helper ====================

    private CompletableFuture<Object> failedResult(String message) {
        return CompletableFuture.failedFuture(new IllegalArgumentException(message));
    }

    // ==================== Internal Session ====================

    private static class WizardSession {
        String sessionId;
        String status; // "running", "completed", "cancelled", "error"
        String mode;
        String workspace;
        String error;
        String lastAnswer;
        long createdAtMs;
    }
}
