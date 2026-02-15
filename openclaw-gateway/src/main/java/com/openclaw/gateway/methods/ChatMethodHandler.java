package com.openclaw.gateway.methods;

import com.fasterxml.jackson.databind.JsonNode;
import com.openclaw.common.model.AcpSession;
import com.openclaw.gateway.session.SessionStore;
import com.openclaw.gateway.session.SessionTranscriptStore;
import com.openclaw.gateway.websocket.EventBroadcaster;
import com.openclaw.gateway.websocket.GatewayConnection;
import com.openclaw.gateway.websocket.GatewayMethodRouter;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Handles chat-related RPC methods: chat.send, chat.abort, chat.history,
 * chat.inject.
 * Corresponds to TypeScript's server-methods/chat.ts.
 *
 * <p>
 * chat.send triggers an Agent run that streams events back to the client
 * via WebSocket events (chat.delta, chat.tool.start, chat.tool.end, chat.done).
 * </p>
 */
@Slf4j
@Component
public class ChatMethodHandler {

    private final GatewayMethodRouter methodRouter;
    private final SessionStore sessionStore;
    private final SessionTranscriptStore transcriptStore;
    private final EventBroadcaster eventBroadcaster;
    private final ChatAgentBridge chatAgentBridge;

    /** Track which connection initiated which run, for targeted event delivery. */
    private final Map<String, String> runToConnectionId = new ConcurrentHashMap<>();

    public ChatMethodHandler(
            GatewayMethodRouter methodRouter,
            SessionStore sessionStore,
            SessionTranscriptStore transcriptStore,
            EventBroadcaster eventBroadcaster,
            ChatAgentBridge chatAgentBridge) {
        this.methodRouter = methodRouter;
        this.sessionStore = sessionStore;
        this.transcriptStore = transcriptStore;
        this.eventBroadcaster = eventBroadcaster;
        this.chatAgentBridge = chatAgentBridge;
    }

    @PostConstruct
    public void registerMethods() {
        methodRouter.registerMethod("chat.send", this::handleChatSend);
        methodRouter.registerMethod("chat.abort", this::handleChatAbort);
        methodRouter.registerMethod("chat.history", this::handleChatHistory);
        methodRouter.registerMethod("chat.inject", this::handleChatInject);
        log.info("Registered chat methods: chat.send, chat.abort, chat.history, chat.inject");
    }

    // ---- chat.send ----

    private CompletableFuture<Object> handleChatSend(JsonNode params, GatewayConnection conn) {
        // TS TUI sends sessionKey (not key) and idempotencyKey as runId
        String sessionKey = getTextParam(params, "sessionKey", "main");
        String message = getTextParam(params, "message", null);
        String clientRunId = getTextParam(params, "idempotencyKey", null);

        if (message == null || message.isBlank()) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("message is required"));
        }

        String connectionId = conn.getConnectionId();
        // Use client-provided runId for event correlation
        String runId = clientRunId != null ? clientRunId : UUID.randomUUID().toString();

        // Find or create session
        AcpSession session = sessionStore.findBySessionKey(sessionKey)
                .orElseGet(() -> sessionStore.createSession(sessionKey,
                        System.getProperty("user.dir")));

        // Check if session already has an active run
        if (session.getActiveRunId() != null) {
            return CompletableFuture.failedFuture(
                    new IllegalStateException("Session " + sessionKey + " already has an active run"));
        }

        String sessionId = session.getSessionId();

        // Start the run
        sessionStore.startRun(sessionId, runId);
        runToConnectionId.put(runId, connectionId);

        // Persist user message to JSONL
        try {
            transcriptStore.appendMessage(sessionId, "user", message);
        } catch (Exception e) {
            log.warn("Failed to persist user message: {}", e.getMessage());
        }

        // Load existing messages for context
        List<Map<String, String>> chatMessages = loadChatMessages(sessionId);
        // Add the new user message
        chatMessages.add(Map.of("role", "user", "content", message));

        // Create streaming listener
        ChatAgentBridge.ChatEventListener listener = createStreamingListener(runId, sessionKey, connectionId);

        // Build chat request via bridge
        String modelId = session.getModel() != null ? session.getModel() : "default";

        ChatAgentBridge.ChatRunRequest request = ChatAgentBridge.ChatRunRequest.builder()
                .sessionKey(sessionKey)
                .modelId(modelId)
                .messages(chatMessages)
                .cwd(session.getCwd())
                .maxTokens(4096)
                .temperature(0.7)
                .listener(listener)
                .build();

        // Execute async — don't block the WebSocket thread
        chatAgentBridge.runChat(request).thenAccept(result -> {
            try {
                // Persist assistant response
                if (result.success() && result.finalMessage() != null) {
                    transcriptStore.appendMessage(sessionId, "assistant", result.finalMessage());
                }

                // Update session metadata and token counts
                session.setUpdatedAt(System.currentTimeMillis());
                session.setInputTokens(session.getInputTokens() + result.inputTokens());
                session.setOutputTokens(session.getOutputTokens() + result.outputTokens());
                session.setTotalTokens(session.getInputTokens() + session.getOutputTokens());
            } catch (Exception e) {
                log.error("Failed to finalize chat run {}: {}", runId, e.getMessage());
            } finally {
                sessionStore.endRun(sessionId);
                runToConnectionId.remove(runId);
            }
        }).exceptionally(throwable -> {
            log.error("Chat run {} failed: {}", runId, throwable.getMessage());
            listener.onError(throwable.getMessage());
            sessionStore.endRun(sessionId);
            runToConnectionId.remove(runId);
            return null;
        });

        // Immediately respond with runId and status (matches TS format)
        Map<String, Object> response = new LinkedHashMap<>();
        response.put("runId", runId);
        response.put("status", "started");
        return CompletableFuture.completedFuture(response);
    }

    // ---- chat.abort ----

    private CompletableFuture<Object> handleChatAbort(JsonNode params, GatewayConnection conn) {
        // TS TUI sends sessionKey (not key)
        String sessionKey = getTextParam(params, "sessionKey", null);
        String runId = getTextParam(params, "runId", null);

        if (sessionKey == null && runId == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("sessionKey or runId is required"));
        }

        boolean aborted = false;
        if (sessionKey != null) {
            var session = sessionStore.findBySessionKey(sessionKey);
            if (session.isPresent()) {
                aborted = sessionStore.cancelRun(session.get().getSessionId());
            }
        } else {
            var sessionId = sessionStore.findSessionByRunId(runId);
            if (sessionId.isPresent()) {
                aborted = sessionStore.cancelRun(sessionId.get());
            }
        }

        return CompletableFuture.completedFuture(Map.of("ok", true, "aborted", aborted));
    }

    // ---- chat.history ----

    private CompletableFuture<Object> handleChatHistory(JsonNode params, GatewayConnection conn) {
        // TS TUI sends sessionKey (not key)
        String sessionKey = getTextParam(params, "sessionKey", "main");
        int limit = params.has("limit") ? params.get("limit").asInt(50) : 50;

        var session = sessionStore.findBySessionKey(sessionKey);
        if (session.isEmpty()) {
            return CompletableFuture.completedFuture(Map.of(
                    "sessionKey", sessionKey,
                    "messages", Collections.emptyList()));
        }

        var messages = limit > 0
                ? transcriptStore.readLastMessages(session.get().getSessionId(), limit)
                : transcriptStore.readMessages(session.get().getSessionId());

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("sessionKey", sessionKey);
        result.put("sessionId", session.get().getSessionId());
        result.put("messages", messages);
        result.put("count", messages.size());
        return CompletableFuture.completedFuture(result);
    }

    // ---- chat.inject ----

    private CompletableFuture<Object> handleChatInject(JsonNode params, GatewayConnection conn) {
        // TS TUI sends sessionKey (not key)
        String sessionKey = getTextParam(params, "sessionKey", null);
        String role = getTextParam(params, "role", "system");
        String content = getTextParam(params, "content", null);

        if (sessionKey == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("sessionKey is required"));
        }
        if (content == null || content.isBlank()) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("content is required"));
        }

        // Validate role
        if (!Set.of("system", "user", "assistant", "tool").contains(role)) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("role must be one of: system, user, assistant, tool"));
        }

        var session = sessionStore.findBySessionKey(sessionKey);
        if (session.isEmpty()) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("Session not found: " + sessionKey));
        }

        String sessionId = session.get().getSessionId();
        String label = getTextParam(params, "label", null);
        String messageContent = label != null ? "[" + label + "]\n\n" + content : content;

        try {
            transcriptStore.appendMessage(sessionId, role, messageContent);
        } catch (Exception e) {
            return CompletableFuture.failedFuture(
                    new RuntimeException("Failed to inject message: " + e.getMessage()));
        }

        String messageId = UUID.randomUUID().toString().substring(0, 8);

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("messageId", messageId);
        result.put("sessionKey", sessionKey);
        result.put("role", role);
        return CompletableFuture.completedFuture(result);
    }

    // ---- Helpers ----

    /**
     * Creates a streaming listener that pushes events to the originating WebSocket
     * client.
     */
    private ChatAgentBridge.ChatEventListener createStreamingListener(
            String runId, String sessionKey, String connectionId) {
        return new ChatAgentBridge.ChatEventListener() {
            private int seq = 0;
            // TS gateway sends cumulative text buffer in each delta, not incremental
            private final StringBuilder buffer = new StringBuilder();

            @Override
            public void onDelta(String text) {
                // Accumulate text to send full buffer each time (matches TS emitChatDelta)
                buffer.append(text);
                String fullText = buffer.toString();

                Map<String, Object> payload = new LinkedHashMap<>();
                payload.put("runId", runId);
                payload.put("sessionKey", sessionKey);
                payload.put("seq", seq++);
                payload.put("state", "delta");
                Map<String, Object> message = new LinkedHashMap<>();
                message.put("role", "assistant");
                message.put("content", List.of(Map.of("type", "text", "text", fullText)));
                message.put("timestamp", System.currentTimeMillis());
                payload.put("message", message);
                eventBroadcaster.sendToConnection(connectionId, "chat", payload);
            }

            @Override
            public void onToolStart(String toolName, String toolId) {
                // Tool events use "agent" event with stream="tool"
                Map<String, Object> data = new LinkedHashMap<>();
                data.put("phase", "start");
                data.put("name", toolName);
                data.put("toolCallId", toolId);

                Map<String, Object> payload = new LinkedHashMap<>();
                payload.put("runId", runId);
                payload.put("stream", "tool");
                payload.put("data", data);
                eventBroadcaster.sendToConnection(connectionId, "agent", payload);
            }

            @Override
            public void onToolEnd(String toolName, String toolId, String result, boolean success) {
                // Tool result events use "agent" event with stream="tool"
                Map<String, Object> data = new LinkedHashMap<>();
                data.put("phase", "result");
                data.put("name", toolName);
                data.put("toolCallId", toolId);
                data.put("result",
                        Map.of("content", List.of(Map.of("type", "text", "text", result != null ? result : ""))));
                data.put("isError", !success);

                Map<String, Object> payload = new LinkedHashMap<>();
                payload.put("runId", runId);
                payload.put("stream", "tool");
                payload.put("data", data);
                eventBroadcaster.sendToConnection(connectionId, "agent", payload);
            }

            @Override
            public void onComplete(String finalMessage) {
                // TS TUI expects "chat" event with state="final" and structured message
                Map<String, Object> payload = new LinkedHashMap<>();
                payload.put("runId", runId);
                payload.put("sessionKey", sessionKey);
                payload.put("seq", seq++);
                payload.put("state", "final");
                Map<String, Object> message = new LinkedHashMap<>();
                message.put("role", "assistant");
                message.put("content",
                        List.of(Map.of("type", "text", "text", finalMessage != null ? finalMessage : "")));
                message.put("stopReason", "end_turn");
                payload.put("message", message);
                eventBroadcaster.sendToConnection(connectionId, "chat", payload);
            }

            @Override
            public void onError(String error) {
                // TS TUI expects "chat" event with state="error"
                Map<String, Object> payload = new LinkedHashMap<>();
                payload.put("runId", runId);
                payload.put("sessionKey", sessionKey);
                payload.put("seq", seq++);
                payload.put("state", "error");
                payload.put("errorMessage", error);
                eventBroadcaster.sendToConnection(connectionId, "chat", payload);
            }
        };
    }

    /**
     * Load existing chat messages from transcript for context continuity.
     */
    private List<Map<String, String>> loadChatMessages(String sessionId) {
        var rawMessages = transcriptStore.readMessages(sessionId);
        List<Map<String, String>> messages = new ArrayList<>();
        for (var msg : rawMessages) {
            String role = String.valueOf(msg.getOrDefault("role", "user"));
            String content = String.valueOf(msg.getOrDefault("content", ""));
            messages.add(Map.of("role", role, "content", content));
        }
        return messages;
    }

    private static String getTextParam(JsonNode params, String field, String defaultValue) {
        if (params != null && params.has(field) && !params.get(field).isNull()) {
            String value = params.get(field).asText("").trim();
            return value.isEmpty() ? defaultValue : value;
        }
        return defaultValue;
    }
}
