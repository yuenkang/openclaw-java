package com.openclaw.agent.models;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import okhttp3.sse.EventSource;
import okhttp3.sse.EventSourceListener;
import okhttp3.sse.EventSources;

import java.io.IOException;
import java.time.Duration;
import java.util.*;
import java.util.Base64;
import java.util.concurrent.CompletableFuture;

/**
 * OpenAI model provider implementation.
 * Supports OpenAI API and any compatible endpoint (Ollama, vLLM, etc.).
 */
@Slf4j
public class OpenAICompatibleProvider implements ModelProvider {

    private static final String DEFAULT_BASE_URL = "https://api.openai.com/v1";

    private final String providerId;
    private final String apiKey;
    private final String baseUrl;
    private final OkHttpClient httpClient;
    private final ObjectMapper objectMapper;

    public OpenAICompatibleProvider(String apiKey) {
        this("openai", apiKey, DEFAULT_BASE_URL);
    }

    public OpenAICompatibleProvider(String providerId, String apiKey, String baseUrl) {
        this.providerId = providerId;
        this.apiKey = apiKey;
        this.baseUrl = baseUrl;
        this.objectMapper = new ObjectMapper();
        this.httpClient = new OkHttpClient.Builder()
                .connectTimeout(Duration.ofSeconds(30))
                .readTimeout(Duration.ofMinutes(5))
                .writeTimeout(Duration.ofSeconds(30))
                .build();
    }

    @Override
    public String getId() {
        return providerId;
    }

    @Override
    public String getApiBaseUrl() {
        return baseUrl;
    }

    @Override
    public CompletableFuture<ChatResponse> chat(ChatRequest request) {
        CompletableFuture<ChatResponse> future = new CompletableFuture<>();

        try {
            Map<String, Object> body = buildRequestBody(request);
            String jsonBody = objectMapper.writeValueAsString(body);

            Request.Builder httpReqBuilder = new Request.Builder()
                    .url(baseUrl + "/chat/completions")
                    .header("content-type", "application/json")
                    .post(RequestBody.create(jsonBody, MediaType.parse("application/json")));

            if (apiKey != null && !apiKey.isEmpty()) {
                httpReqBuilder.header("Authorization", "Bearer " + apiKey);
            }

            httpClient.newCall(httpReqBuilder.build()).enqueue(new Callback() {
                @Override
                public void onFailure(Call call, IOException e) {
                    future.completeExceptionally(e);
                }

                @Override
                public void onResponse(Call call, Response response) throws IOException {
                    try (ResponseBody responseBody = response.body()) {
                        if (!response.isSuccessful()) {
                            String errorBody = responseBody != null ? responseBody.string() : "Unknown error";
                            future.completeExceptionally(
                                    new RuntimeException("OpenAI API error " + response.code() + ": " + errorBody));
                            return;
                        }

                        String json = responseBody != null ? responseBody.string() : "{}";
                        ChatResponse chatResponse = parseResponse(json);
                        future.complete(chatResponse);
                    }
                }
            });

        } catch (JsonProcessingException e) {
            future.completeExceptionally(e);
        }

        return future;
    }

    @Override
    public CompletableFuture<ChatResponse> chatStream(
            ChatRequest request, StreamListener listener) {
        CompletableFuture<ChatResponse> future = new CompletableFuture<>();

        try {
            Map<String, Object> body = buildRequestBody(request);
            body.put("stream", true);
            body.put("stream_options", Map.of("include_usage", true));
            String jsonBody = objectMapper.writeValueAsString(body);

            Request.Builder httpReqBuilder = new Request.Builder()
                    .url(baseUrl + "/chat/completions")
                    .header("content-type", "application/json")
                    .post(RequestBody.create(jsonBody, MediaType.parse("application/json")));

            if (apiKey != null && !apiKey.isEmpty()) {
                httpReqBuilder.header("Authorization", "Bearer " + apiKey);
            }

            // Mutable state for accumulating the streaming response
            ChatResponse.ChatResponseBuilder responseBuilder = ChatResponse.builder();
            StringBuilder textContent = new StringBuilder();
            StringBuilder reasoningContent = new StringBuilder();
            // Track tool calls: index → (id, name, arguments_builder)
            Map<Integer, String[]> toolIds = new LinkedHashMap<>();
            Map<Integer, StringBuilder> toolArgs = new LinkedHashMap<>();

            EventSource.Factory factory = EventSources.createFactory(httpClient);
            factory.newEventSource(httpReqBuilder.build(), new EventSourceListener() {
                @Override
                public void onEvent(EventSource source, String id, String type, String data) {
                    try {
                        if ("[DONE]".equals(data)) {
                            // Build final response
                            responseBuilder.message(ChatMessage.builder()
                                    .role("assistant")
                                    .content(textContent.toString())
                                    .reasoningContent(
                                            reasoningContent.length() > 0 ? reasoningContent.toString() : null)
                                    .build());
                            List<ToolUse> toolUses = buildToolUses();
                            if (!toolUses.isEmpty()) {
                                responseBuilder.toolUses(toolUses);
                            }
                            future.complete(responseBuilder.build());
                            return;
                        }

                        JsonNode chunk = objectMapper.readTree(data);
                        responseBuilder.id(chunk.path("id").asText());
                        responseBuilder.model(chunk.path("model").asText());

                        // Usage (sent in final chunk with stream_options.include_usage)
                        JsonNode usageNode = chunk.path("usage");
                        if (!usageNode.isMissingNode() && !usageNode.isNull()) {
                            Usage usage = Usage.builder()
                                    .inputTokens(usageNode.path("prompt_tokens").asInt())
                                    .outputTokens(usageNode.path("completion_tokens").asInt())
                                    .build();
                            responseBuilder.usage(usage);
                            listener.onUsage(usage);
                        }

                        JsonNode choices = chunk.path("choices");
                        if (!choices.isArray() || choices.isEmpty())
                            return;

                        JsonNode choice = choices.get(0);
                        JsonNode delta = choice.path("delta");
                        String finishReason = choice.path("finish_reason").asText(null);
                        if (finishReason != null) {
                            responseBuilder.stopReason(finishReason);
                        }

                        // Reasoning/thinking content delta (from thinking models)
                        String reasoning = delta.path("reasoning_content").asText(null);
                        if (reasoning != null && !reasoning.isEmpty()) {
                            reasoningContent.append(reasoning);
                        }

                        // Text delta
                        String content = delta.path("content").asText(null);
                        if (content != null && !content.isEmpty()) {
                            textContent.append(content);
                            listener.onText(content);
                        }

                        // Tool call deltas
                        JsonNode toolCallsNode = delta.path("tool_calls");
                        if (toolCallsNode.isArray()) {
                            for (JsonNode tc : toolCallsNode) {
                                int idx = tc.path("index").asInt(0);
                                JsonNode fn = tc.path("function");

                                // First chunk for this tool call has id and name
                                String tcId = tc.path("id").asText(null);
                                if (tcId != null) {
                                    toolIds.put(idx, new String[] { tcId, fn.path("name").asText("") });
                                    toolArgs.putIfAbsent(idx, new StringBuilder());
                                }

                                // Accumulate arguments
                                String args = fn.path("arguments").asText(null);
                                if (args != null) {
                                    toolArgs.computeIfAbsent(idx, k -> new StringBuilder()).append(args);
                                }
                            }
                        }
                    } catch (Exception e) {
                        log.error("Error parsing OpenAI SSE chunk: {}", e.getMessage(), e);
                    }
                }

                private List<ToolUse> buildToolUses() {
                    List<ToolUse> result = new ArrayList<>();
                    for (Map.Entry<Integer, String[]> entry : toolIds.entrySet()) {
                        int idx = entry.getKey();
                        String[] idAndName = entry.getValue();
                        StringBuilder argsBuilder = toolArgs.getOrDefault(idx, new StringBuilder());
                        try {
                            @SuppressWarnings("unchecked")
                            Map<String, Object> input = argsBuilder.length() > 0
                                    ? objectMapper.readValue(argsBuilder.toString(), Map.class)
                                    : Map.of();
                            ToolUse tu = ToolUse.builder()
                                    .id(idAndName[0])
                                    .name(idAndName[1])
                                    .input(input)
                                    .build();
                            result.add(tu);
                            listener.onToolUse(tu);
                        } catch (Exception e) {
                            log.error("Error parsing tool args for {}: {}", idAndName[1], e.getMessage());
                        }
                    }
                    return result;
                }

                @Override
                public void onFailure(EventSource source, Throwable t, Response response) {
                    if (!future.isDone()) {
                        String errMsg = t != null ? t.getMessage() : "Unknown SSE failure";
                        if (response != null && response.body() != null) {
                            try {
                                errMsg += " - " + response.body().string();
                            } catch (IOException ignored) {
                            }
                        }
                        future.completeExceptionally(new RuntimeException("OpenAI stream failed: " + errMsg));
                    }
                }

                @Override
                public void onClosed(EventSource source) {
                    if (!future.isDone()) {
                        responseBuilder.message(ChatMessage.builder()
                                .role("assistant")
                                .content(textContent.toString())
                                .reasoningContent(reasoningContent.length() > 0 ? reasoningContent.toString() : null)
                                .build());
                        List<ToolUse> toolUses = buildToolUses();
                        if (!toolUses.isEmpty()) {
                            responseBuilder.toolUses(toolUses);
                        }
                        future.complete(responseBuilder.build());
                    }
                }
            });

        } catch (JsonProcessingException e) {
            future.completeExceptionally(e);
        }

        return future;
    }

    @Override
    public List<ModelInfo> listModels() {
        // Use a short-timeout client for model listing to avoid blocking
        // the caller for 30s+ when the API is slow or unreachable.
        OkHttpClient listClient = httpClient.newBuilder()
                .connectTimeout(Duration.ofSeconds(5))
                .readTimeout(Duration.ofSeconds(5))
                .build();
        try {
            Request.Builder reqBuilder = new Request.Builder()
                    .url(baseUrl + "/models")
                    .get();

            if (apiKey != null && !apiKey.isEmpty()) {
                reqBuilder.header("Authorization", "Bearer " + apiKey);
            }

            try (Response response = listClient.newCall(reqBuilder.build()).execute()) {
                if (response.isSuccessful() && response.body() != null) {
                    JsonNode root = objectMapper.readTree(response.body().string());
                    JsonNode data = root.path("data");
                    List<ModelInfo> models = new ArrayList<>();
                    if (data.isArray()) {
                        for (JsonNode m : data) {
                            models.add(ModelInfo.builder()
                                    .id(m.path("id").asText())
                                    .name(m.path("id").asText())
                                    .contextWindow(128000)
                                    .maxTokens(4096)
                                    .build());
                        }
                    }
                    return models;
                }
            }
        } catch (Exception e) {
            log.warn("Failed to list models from {}: {}", baseUrl, e.getMessage());
        }
        return List.of();
    }

    private Map<String, Object> buildRequestBody(ChatRequest request) {
        Map<String, Object> body = new LinkedHashMap<>();

        String model = request.getModel();
        if (model.contains("/")) {
            model = model.substring(model.indexOf("/") + 1);
        }
        body.put("model", model);
        body.put("max_tokens", request.getMaxTokens() > 0 ? request.getMaxTokens() : 4096);

        if (request.getTemperature() > 0) {
            body.put("temperature", request.getTemperature());
        }

        // Messages (OpenAI format: system message is a regular message)
        List<Map<String, Object>> messages = new ArrayList<>();
        if (request.getSystemPrompt() != null && !request.getSystemPrompt().isEmpty()) {
            messages.add(Map.of("role", "system", "content", request.getSystemPrompt()));
        }
        for (ChatMessage msg : request.getMessages()) {
            if ("tool".equals(msg.getRole())) {
                // Tool result messages require tool_call_id
                Map<String, Object> m = new LinkedHashMap<>();
                m.put("role", "tool");
                m.put("tool_call_id", msg.getToolUseId());
                // Support multimodal tool results (e.g. screenshot images)
                if (msg.getContentParts() != null && !msg.getContentParts().isEmpty()) {
                    List<Map<String, Object>> parts = new ArrayList<>();
                    for (ContentPart part : msg.getContentParts()) {
                        if ("text".equals(part.getType())) {
                            parts.add(Map.of("type", "text", "text",
                                    part.getText() != null ? part.getText() : ""));
                        } else if ("image_url".equals(part.getType()) && part.getImageUrl() != null) {
                            parts.add(Map.of("type", "image_url",
                                    "image_url", Map.of("url", part.getImageUrl().getUrl())));
                        }
                    }
                    m.put("content", parts);
                } else {
                    m.put("content", msg.getContent() != null ? msg.getContent() : "");
                }
                messages.add(m);
            } else if ("assistant".equals(msg.getRole()) && msg.getToolUses() != null && !msg.getToolUses().isEmpty()) {
                // Assistant messages with tool calls
                Map<String, Object> m = new LinkedHashMap<>();
                m.put("role", "assistant");
                if (msg.getContent() != null && !msg.getContent().isEmpty()) {
                    m.put("content", msg.getContent());
                }
                List<Map<String, Object>> toolCalls = new ArrayList<>();
                for (ToolUse tu : msg.getToolUses()) {
                    String argsJson;
                    try {
                        argsJson = objectMapper.writeValueAsString(
                                tu.getInput() != null ? tu.getInput() : Map.of());
                    } catch (JsonProcessingException e) {
                        argsJson = "{}";
                    }
                    toolCalls.add(Map.of(
                            "id", tu.getId(),
                            "type", "function",
                            "function", Map.of(
                                    "name", tu.getName(),
                                    "arguments", argsJson)));
                }
                m.put("tool_calls", toolCalls);
                messages.add(m);
            } else {
                Map<String, Object> m = new LinkedHashMap<>();
                m.put("role", msg.getRole());
                // Support multimodal content (text + images)
                if (msg.getContentParts() != null && !msg.getContentParts().isEmpty()) {
                    List<Map<String, Object>> parts = new ArrayList<>();
                    for (ContentPart part : msg.getContentParts()) {
                        if ("text".equals(part.getType())) {
                            parts.add(Map.of("type", "text", "text",
                                    part.getText() != null ? part.getText() : ""));
                        } else if ("image_url".equals(part.getType()) && part.getImageUrl() != null) {
                            String imageUrl = part.getImageUrl().getUrl();
                            if (imageUrl != null && imageUrl.startsWith("data:")) {
                                // data URI: ensure clean base64 (re-encode to strip whitespace/padding issues)
                                // Format: data:<mime>;base64,<data>
                                int commaIdx = imageUrl.indexOf(',');
                                if (commaIdx > 0) {
                                    String prefix = imageUrl.substring(0, commaIdx + 1);
                                    String rawBase64 = imageUrl.substring(commaIdx + 1)
                                            .replaceAll("\\s+", "");
                                    // Re-encode to guarantee valid base64
                                    try {
                                        byte[] decoded = Base64.getDecoder().decode(rawBase64);
                                        rawBase64 = Base64.getEncoder().encodeToString(decoded);
                                    } catch (IllegalArgumentException ignored) {
                                        // If decode fails, use stripped version as-is
                                    }
                                    parts.add(Map.of("type", "image_url",
                                            "image_url", Map.of("url", prefix + rawBase64)));
                                } else {
                                    parts.add(Map.of("type", "image_url",
                                            "image_url", Map.of("url", imageUrl)));
                                }
                            } else {
                                parts.add(Map.of("type", "image_url",
                                        "image_url", Map.of("url", imageUrl)));
                            }
                        }
                    }
                    m.put("content", parts);
                } else {
                    m.put("content", msg.getContent() != null ? msg.getContent() : "");
                }
                // NOTE: We intentionally do NOT include reasoning_content here.
                // When using thinking models via an OpenAI-compatible proxy,
                // the proxy loses the Anthropic 'signature' during format conversion.
                // Sending reasoning_content back would cause the proxy to construct
                // a thinking block without a valid signature, causing Anthropic to
                // reject with "thinking.signature: Field required".
                // Anthropic API allows omitting thinking blocks from history.
                messages.add(m);
            }
        }
        body.put("messages", messages);

        // Tools (OpenAI function calling format)
        if (request.getTools() != null && !request.getTools().isEmpty()) {
            List<Map<String, Object>> tools = new ArrayList<>();
            for (Map<String, Object> tool : request.getTools()) {
                tools.add(Map.of(
                        "type", "function",
                        "function", Map.of(
                                "name", tool.get("name"),
                                "description", tool.getOrDefault("description", ""),
                                "parameters", tool.getOrDefault("input_schema", Map.of()))));
            }
            body.put("tools", tools);
        }

        return body;
    }

    private ChatResponse parseResponse(String json) throws JsonProcessingException {
        JsonNode root = objectMapper.readTree(json);
        JsonNode choice = root.path("choices").path(0);
        JsonNode message = choice.path("message");

        ChatResponse.ChatResponseBuilder builder = ChatResponse.builder()
                .id(root.path("id").asText())
                .model(root.path("model").asText())
                .stopReason(choice.path("finish_reason").asText());

        // Usage
        JsonNode usageNode = root.path("usage");
        if (!usageNode.isMissingNode()) {
            builder.usage(Usage.builder()
                    .inputTokens(usageNode.path("prompt_tokens").asInt())
                    .outputTokens(usageNode.path("completion_tokens").asInt())
                    .build());
        }

        // Message content
        String reasoningContent = message.has("reasoning_content")
                && !message.path("reasoning_content").isNull()
                        ? message.path("reasoning_content").asText(null)
                        : null;
        builder.message(ChatMessage.builder()
                .role("assistant")
                .content(message.path("content").asText(""))
                .reasoningContent(reasoningContent)
                .build());

        // Tool calls
        JsonNode toolCalls = message.path("tool_calls");
        if (toolCalls.isArray() && !toolCalls.isEmpty()) {
            List<ToolUse> toolUses = new ArrayList<>();
            for (JsonNode tc : toolCalls) {
                JsonNode function = tc.path("function");
                @SuppressWarnings("unchecked")
                Map<String, Object> args = objectMapper.readValue(
                        function.path("arguments").asText("{}"), Map.class);
                toolUses.add(ToolUse.builder()
                        .id(tc.path("id").asText())
                        .name(function.path("name").asText())
                        .input(args)
                        .build());
            }
            builder.toolUses(toolUses);
        }

        return builder.build();
    }
}
