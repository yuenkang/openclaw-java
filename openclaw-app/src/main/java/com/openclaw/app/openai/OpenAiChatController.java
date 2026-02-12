package com.openclaw.app.openai;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.gateway.auth.AuthService;
import com.openclaw.gateway.auth.AuthService.ConnectAuth;
import com.openclaw.gateway.methods.ChatAgentBridge;
import com.openclaw.gateway.methods.ChatAgentBridge.ChatEventListener;
import com.openclaw.gateway.methods.ChatAgentBridge.ChatRunRequest;
import com.openclaw.gateway.openai.OpenAiTypes.*;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * OpenAI-compatible HTTP API controller.
 * Implements POST /v1/chat/completions with both non-streaming and SSE
 * streaming.
 * Mirrors TypeScript's openai-http.ts.
 */
@Slf4j
@RestController
@RequestMapping("/v1")
public class OpenAiChatController {

    private final ChatAgentBridge chatAgentBridge;
    private final AuthService authService;
    private final ObjectMapper objectMapper;

    public OpenAiChatController(
            ChatAgentBridge chatAgentBridge,
            AuthService authService,
            ObjectMapper objectMapper) {
        this.chatAgentBridge = chatAgentBridge;
        this.authService = authService;
        this.objectMapper = objectMapper;
    }

    @PostMapping(value = "/chat/completions", consumes = MediaType.APPLICATION_JSON_VALUE)
    public Object chatCompletions(
            @RequestBody ChatCompletionRequest request,
            HttpServletRequest httpRequest,
            HttpServletResponse httpResponse) {

        // ── Auth ──────────────────────────────────────────────
        String bearerToken = extractBearerToken(httpRequest);
        boolean isLocal = isLocalRequest(httpRequest);
        var authResult = authService.authorize(
                new ConnectAuth(bearerToken, bearerToken),
                httpRequest.getRemoteAddr(),
                isLocal);

        if (!authResult.ok()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                    .body(ErrorResponse.builder()
                            .error(ErrorBody.builder()
                                    .message("Unauthorized")
                                    .type("authentication_error")
                                    .build())
                            .build());
        }

        // ── Parse request ────────────────────────────────────
        String model = request.getModel() != null ? request.getModel() : "openclaw";
        boolean stream = Boolean.TRUE.equals(request.getStream());

        PromptResult prompt = buildAgentPrompt(request.getMessages());
        if (prompt.message().isBlank()) {
            return ResponseEntity.badRequest()
                    .body(ErrorResponse.builder()
                            .error(ErrorBody.builder()
                                    .message("Missing user message in `messages`.")
                                    .type("invalid_request_error")
                                    .build())
                            .build());
        }

        String runId = "chatcmpl-" + UUID.randomUUID();
        String sessionKey = resolveSessionKey(httpRequest, model, request.getUser());

        // Build messages for agent
        List<Map<String, String>> agentMessages = new ArrayList<>();
        agentMessages.add(Map.of("role", "user", "content", prompt.message()));

        ChatRunRequest.Builder reqBuilder = ChatRunRequest.builder()
                .sessionKey(sessionKey)
                .modelId(model)
                .messages(agentMessages);

        if (prompt.extraSystemPrompt() != null) {
            reqBuilder.systemPrompt(prompt.extraSystemPrompt());
        }
        if (request.getMaxTokens() != null) {
            reqBuilder.maxTokens(request.getMaxTokens());
        }
        if (request.getTemperature() != null) {
            reqBuilder.temperature(request.getTemperature());
        }

        if (!stream) {
            return handleNonStreaming(reqBuilder, runId, model);
        } else {
            return handleStreaming(reqBuilder, runId, model);
        }
    }

    // ── Non-streaming ────────────────────────────────────────

    private ResponseEntity<?> handleNonStreaming(
            ChatRunRequest.Builder reqBuilder, String runId, String model) {
        try {
            var result = chatAgentBridge.runChat(reqBuilder.build()).join();

            String content = result.success() && result.finalMessage() != null
                    ? result.finalMessage()
                    : "No response from OpenClaw.";

            ChatCompletionResponse response = ChatCompletionResponse.builder()
                    .id(runId)
                    .object("chat.completion")
                    .created(System.currentTimeMillis() / 1000)
                    .model(model)
                    .choices(List.of(Choice.builder()
                            .index(0)
                            .message(new ChatMessage("assistant", content, null))
                            .finishReason("stop")
                            .build()))
                    .usage(Usage.builder().build())
                    .build();

            return ResponseEntity.ok(response);
        } catch (Exception e) {
            log.error("OpenAI non-streaming error: {}", e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(ErrorResponse.builder()
                            .error(ErrorBody.builder()
                                    .message(e.getMessage())
                                    .type("api_error")
                                    .build())
                            .build());
        }
    }

    // ── Streaming (SSE) ──────────────────────────────────────

    private SseEmitter handleStreaming(
            ChatRunRequest.Builder reqBuilder, String runId, String model) {
        // Timeout: 5 minutes
        SseEmitter emitter = new SseEmitter(300_000L);
        AtomicBoolean wroteRole = new AtomicBoolean(false);
        AtomicBoolean closed = new AtomicBoolean(false);

        ChatEventListener listener = new ChatEventListener() {
            @Override
            public void onDelta(String text) {
                if (closed.get() || text == null || text.isEmpty())
                    return;
                try {
                    if (wroteRole.compareAndSet(false, true)) {
                        sendChunk(emitter, buildChunk(runId, model,
                                DeltaContent.builder().role("assistant").build(), null));
                    }
                    sendChunk(emitter, buildChunk(runId, model,
                            DeltaContent.builder().content(text).build(), null));
                } catch (Exception e) {
                    log.debug("SSE write failed: {}", e.getMessage());
                    closed.set(true);
                }
            }

            @Override
            public void onComplete(String finalMessage) {
                if (closed.compareAndSet(false, true)) {
                    try {
                        // If no deltas were sent, send the full response
                        if (!wroteRole.get()) {
                            wroteRole.set(true);
                            sendChunk(emitter, buildChunk(runId, model,
                                    DeltaContent.builder().role("assistant").build(), null));
                            String content = finalMessage != null ? finalMessage : "No response from OpenClaw.";
                            sendChunk(emitter, buildChunk(runId, model,
                                    DeltaContent.builder().content(content).build(), null));
                        }
                        // Send finish chunk
                        sendChunk(emitter, buildChunk(runId, model,
                                DeltaContent.builder().build(), "stop"));
                        emitter.send(SseEmitter.event().data("[DONE]"));
                        emitter.complete();
                    } catch (Exception e) {
                        log.debug("SSE complete failed: {}", e.getMessage());
                    }
                }
            }

            @Override
            public void onError(String error) {
                if (closed.compareAndSet(false, true)) {
                    try {
                        sendChunk(emitter, buildChunk(runId, model,
                                DeltaContent.builder()
                                        .content("Error: " + error).build(),
                                "stop"));
                        emitter.send(SseEmitter.event().data("[DONE]"));
                        emitter.complete();
                    } catch (Exception e) {
                        log.debug("SSE error write failed: {}", e.getMessage());
                        emitter.completeWithError(e);
                    }
                }
            }
        };

        reqBuilder.listener(listener);

        emitter.onTimeout(() -> closed.set(true));
        emitter.onCompletion(() -> closed.set(true));

        CompletableFuture.runAsync(() -> {
            try {
                chatAgentBridge.runChat(reqBuilder.build()).join();
                // Ensure we close even if listener wasn't called
                if (closed.compareAndSet(false, true)) {
                    emitter.send(SseEmitter.event().data("[DONE]"));
                    emitter.complete();
                }
            } catch (Exception e) {
                if (closed.compareAndSet(false, true)) {
                    log.error("OpenAI streaming error: {}", e.getMessage(), e);
                    emitter.completeWithError(e);
                }
            }
        });

        return emitter;
    }

    // ── Helpers ──────────────────────────────────────────────

    private ChatCompletionChunk buildChunk(String id, String model,
            DeltaContent delta, String finishReason) {
        return ChatCompletionChunk.builder()
                .id(id)
                .object("chat.completion.chunk")
                .created(System.currentTimeMillis() / 1000)
                .model(model)
                .choices(List.of(Choice.builder()
                        .index(0)
                        .delta(delta)
                        .finishReason(finishReason)
                        .build()))
                .build();
    }

    private void sendChunk(SseEmitter emitter, Object data) throws java.io.IOException {
        String json = objectMapper.writeValueAsString(data);
        emitter.send(SseEmitter.event().data(json, MediaType.APPLICATION_JSON));
    }

    private static String extractBearerToken(HttpServletRequest request) {
        String auth = request.getHeader("Authorization");
        if (auth != null && auth.startsWith("Bearer ")) {
            return auth.substring(7).trim();
        }
        return null;
    }

    private static boolean isLocalRequest(HttpServletRequest request) {
        String addr = request.getRemoteAddr();
        return "127.0.0.1".equals(addr) || "0:0:0:0:0:0:0:1".equals(addr)
                || "::1".equals(addr) || "localhost".equals(addr);
    }

    private static String resolveSessionKey(HttpServletRequest request,
            String model, String user) {
        // Use X-Session-Key header, or derive from user+model
        String headerKey = request.getHeader("X-Session-Key");
        if (headerKey != null && !headerKey.isBlank()) {
            return headerKey;
        }
        String userPart = user != null ? user : "openai";
        return "openai:" + userPart + ":" + model;
    }

    /**
     * Build an agent prompt from OpenAI-style messages.
     * Mirrors TS buildAgentPrompt().
     */
    static PromptResult buildAgentPrompt(List<ChatMessage> messages) {
        if (messages == null || messages.isEmpty()) {
            return new PromptResult("", null);
        }

        List<String> systemParts = new ArrayList<>();
        List<ConversationEntry> entries = new ArrayList<>();

        for (ChatMessage msg : messages) {
            if (msg == null || msg.getRole() == null)
                continue;
            String role = msg.getRole().trim();
            String content = extractTextContent(msg.getContent()).trim();
            if (role.isEmpty() || content.isEmpty())
                continue;

            if ("system".equals(role) || "developer".equals(role)) {
                systemParts.add(content);
                continue;
            }

            String normalizedRole = "function".equals(role) ? "tool" : role;
            if (!"user".equals(normalizedRole) && !"assistant".equals(normalizedRole)
                    && !"tool".equals(normalizedRole)) {
                continue;
            }

            String name = msg.getName() != null ? msg.getName().trim() : "";
            String sender = switch (normalizedRole) {
                case "assistant" -> "Assistant";
                case "user" -> "User";
                default -> name.isEmpty() ? "Tool" : "Tool:" + name;
            };

            entries.add(new ConversationEntry(normalizedRole, sender, content));
        }

        // Find the last user/tool message as the current prompt
        String message = "";
        if (!entries.isEmpty()) {
            int currentIndex = -1;
            for (int i = entries.size() - 1; i >= 0; i--) {
                String r = entries.get(i).role();
                if ("user".equals(r) || "tool".equals(r)) {
                    currentIndex = i;
                    break;
                }
            }
            if (currentIndex < 0) {
                currentIndex = entries.size() - 1;
            }

            ConversationEntry current = entries.get(currentIndex);
            if (currentIndex == 0) {
                message = current.content();
            } else {
                // Build history context
                StringBuilder sb = new StringBuilder();
                sb.append("<conversation_history>\n");
                for (int i = 0; i < currentIndex; i++) {
                    ConversationEntry e = entries.get(i);
                    sb.append(e.sender()).append(": ").append(e.content()).append("\n");
                }
                sb.append("</conversation_history>\n\n");
                sb.append(current.content());
                message = sb.toString();
            }
        }

        String extraSystemPrompt = systemParts.isEmpty() ? null : String.join("\n\n", systemParts);
        return new PromptResult(message, extraSystemPrompt);
    }

    private static String extractTextContent(Object content) {
        if (content instanceof String s) {
            return s;
        }
        if (content instanceof List<?> parts) {
            StringBuilder sb = new StringBuilder();
            for (Object part : parts) {
                if (part instanceof Map<?, ?> map) {
                    Object typeObj = map.get("type");
                    String type = typeObj != null ? String.valueOf(typeObj) : "";
                    Object textObj = map.get("text");
                    String text = textObj != null ? String.valueOf(textObj) : "";
                    Object inputTextObj = map.get("input_text");
                    String inputText = inputTextObj != null ? String.valueOf(inputTextObj) : "";
                    if (("text".equals(type) || "input_text".equals(type)) && !text.isEmpty()) {
                        if (!sb.isEmpty())
                            sb.append("\n");
                        sb.append(text);
                    } else if (!inputText.isEmpty()) {
                        if (!sb.isEmpty())
                            sb.append("\n");
                        sb.append(inputText);
                    }
                }
            }
            return sb.toString();
        }
        return "";
    }

    record PromptResult(String message, String extraSystemPrompt) {
    }

    record ConversationEntry(String role, String sender, String content) {
    }
}
