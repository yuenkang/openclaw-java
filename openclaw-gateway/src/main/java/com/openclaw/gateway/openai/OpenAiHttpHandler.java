package com.openclaw.gateway.openai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.common.config.ConfigService;
import com.openclaw.gateway.methods.ChatAgentBridge;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.*;

/**
 * Serves OpenAI-compatible `/v1/chat/completions` endpoint.
 * <p>
 * Corresponds to TypeScript's {@code openai-http.ts} (427 lines).
 * Supports both streaming (SSE) and non-streaming modes.
 */
@Slf4j
public class OpenAiHttpHandler {

    private static final int MAX_BODY_BYTES = 4 * 1024 * 1024;

    private final ConfigService configService;
    private final ObjectMapper objectMapper;
    private final ChatAgentBridge chatAgentBridge;

    public OpenAiHttpHandler(ConfigService configService,
            ObjectMapper objectMapper,
            ChatAgentBridge chatAgentBridge) {
        this.configService = configService;
        this.objectMapper = objectMapper;
        this.chatAgentBridge = chatAgentBridge;
    }

    /**
     * Handle a POST /v1/chat/completions request.
     */
    public void handleChatCompletions(HttpServletRequest request, HttpServletResponse response)
            throws IOException {
        // Validate method
        if (!"POST".equalsIgnoreCase(request.getMethod())) {
            sendError(response, 405, "Method not allowed");
            return;
        }

        // Read body
        byte[] body = request.getInputStream().readNBytes(MAX_BODY_BYTES + 1);
        if (body.length > MAX_BODY_BYTES) {
            sendError(response, 413, "Request body too large");
            return;
        }

        JsonNode requestBody;
        try {
            requestBody = objectMapper.readTree(new String(body, StandardCharsets.UTF_8));
        } catch (Exception e) {
            sendError(response, 400, "Invalid JSON: " + e.getMessage());
            return;
        }

        if (!requestBody.isObject()) {
            sendError(response, 400, "Request body must be a JSON object");
            return;
        }

        // Extract parameters
        String model = requestBody.path("model").asText("default");
        boolean stream = requestBody.path("stream").asBoolean(false);
        JsonNode messagesNode = requestBody.path("messages");

        if (!messagesNode.isArray() || messagesNode.isEmpty()) {
            sendError(response, 400, "messages is required and must be a non-empty array");
            return;
        }

        // Extract the last user message
        String lastUserMessage = null;
        for (int i = messagesNode.size() - 1; i >= 0; i--) {
            JsonNode msg = messagesNode.get(i);
            if ("user".equals(msg.path("role").asText())) {
                lastUserMessage = msg.path("content").asText();
                break;
            }
        }

        if (lastUserMessage == null || lastUserMessage.isBlank()) {
            sendError(response, 400, "No user message found in messages array");
            return;
        }

        String sessionKey = "openai:" + UUID.randomUUID().toString().substring(0, 8);

        // Create run request
        String runId = UUID.randomUUID().toString();
        var runRequest = ChatAgentBridge.ChatRunRequest.builder()
                .sessionKey(sessionKey)
                .messages(List.of(Map.of("role", "user", "content", lastUserMessage)))
                .modelId(model)
                .build();

        if (stream) {
            handleStreaming(response, runRequest, model, runId);
        } else {
            handleNonStreaming(response, runRequest, model, runId);
        }
    }

    private void handleStreaming(HttpServletResponse response,
            ChatAgentBridge.ChatRunRequest runRequest,
            String model, String runId) throws IOException {
        response.setContentType("text/event-stream");
        response.setCharacterEncoding("UTF-8");
        response.setHeader("Cache-Control", "no-cache");
        response.setHeader("Connection", "keep-alive");
        response.setStatus(200);
        response.flushBuffer();

        PrintWriter writer = response.getWriter();
        String id = "chatcmpl-" + runId.substring(0, 8);
        long created = Instant.now().getEpochSecond();

        try {
            var future = chatAgentBridge.runChat(runRequest);
            var result = future.get();
            String text = result.finalMessage() != null ? result.finalMessage() : "";

            // Send content chunks (simulate streaming with a single chunk)
            ObjectNode delta = objectMapper.createObjectNode();
            delta.put("role", "assistant");
            delta.put("content", text);

            ObjectNode choice = objectMapper.createObjectNode();
            choice.put("index", 0);
            choice.set("delta", delta);
            choice.putNull("finish_reason");

            ObjectNode chunk = objectMapper.createObjectNode();
            chunk.put("id", id);
            chunk.put("object", "chat.completion.chunk");
            chunk.put("created", created);
            chunk.put("model", model);
            chunk.set("choices", objectMapper.createArrayNode().add(choice));

            writer.write("data: " + objectMapper.writeValueAsString(chunk) + "\n\n");
            writer.flush();

            // Send final chunk
            ObjectNode finalDelta = objectMapper.createObjectNode();
            ObjectNode finalChoice = objectMapper.createObjectNode();
            finalChoice.put("index", 0);
            finalChoice.set("delta", finalDelta);
            finalChoice.put("finish_reason", "stop");

            ObjectNode finalChunk = objectMapper.createObjectNode();
            finalChunk.put("id", id);
            finalChunk.put("object", "chat.completion.chunk");
            finalChunk.put("created", created);
            finalChunk.put("model", model);
            finalChunk.set("choices", objectMapper.createArrayNode().add(finalChoice));

            writer.write("data: " + objectMapper.writeValueAsString(finalChunk) + "\n\n");
            writer.write("data: [DONE]\n\n");
            writer.flush();

        } catch (Exception e) {
            log.error("Streaming chat completion failed: {}", e.getMessage(), e);
            ObjectNode errorChunk = objectMapper.createObjectNode();
            errorChunk.put("error", e.getMessage());
            writer.write("data: " + objectMapper.writeValueAsString(errorChunk) + "\n\n");
            writer.write("data: [DONE]\n\n");
            writer.flush();
        }
    }

    private void handleNonStreaming(HttpServletResponse response,
            ChatAgentBridge.ChatRunRequest runRequest,
            String model, String runId) throws IOException {
        String id = "chatcmpl-" + runId.substring(0, 8);
        long created = Instant.now().getEpochSecond();

        try {
            var future = chatAgentBridge.runChat(runRequest);
            var result = future.get();
            String text = result.finalMessage() != null ? result.finalMessage() : "";

            // Build response
            ObjectNode message = objectMapper.createObjectNode();
            message.put("role", "assistant");
            message.put("content", text);

            ObjectNode choice = objectMapper.createObjectNode();
            choice.put("index", 0);
            choice.set("message", message);
            choice.put("finish_reason", "stop");

            ObjectNode usage = objectMapper.createObjectNode();
            usage.put("prompt_tokens", 0);
            usage.put("completion_tokens", 0);
            usage.put("total_tokens", 0);

            ObjectNode responseBody = objectMapper.createObjectNode();
            responseBody.put("id", id);
            responseBody.put("object", "chat.completion");
            responseBody.put("created", created);
            responseBody.put("model", model);
            responseBody.set("choices", objectMapper.createArrayNode().add(choice));
            responseBody.set("usage", usage);

            response.setContentType("application/json");
            response.setStatus(200);
            response.getWriter().write(objectMapper.writeValueAsString(responseBody));

        } catch (Exception e) {
            log.error("Chat completion failed: {}", e.getMessage(), e);
            sendError(response, 500, "Internal error: " + e.getMessage());
        }
    }

    private void sendError(HttpServletResponse response, int status, String message)
            throws IOException {
        ObjectNode error = objectMapper.createObjectNode();
        ObjectNode inner = objectMapper.createObjectNode();
        inner.put("message", message);
        inner.put("type", "invalid_request_error");
        error.set("error", inner);

        response.setContentType("application/json");
        response.setStatus(status);
        response.getWriter().write(objectMapper.writeValueAsString(error));
    }
}
