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
import java.util.concurrent.CompletableFuture;

/**
 * Anthropic Claude model provider implementation.
 * Corresponds to TypeScript's Anthropic SDK integration.
 */
@Slf4j
public class AnthropicProvider implements ModelProvider {

    private static final String DEFAULT_BASE_URL = "https://api.anthropic.com/v1";
    private static final String API_VERSION = "2023-06-01";

    private final String apiKey;
    private final String baseUrl;
    private final OkHttpClient httpClient;
    private final ObjectMapper objectMapper;

    public AnthropicProvider(String apiKey) {
        this(apiKey, DEFAULT_BASE_URL);
    }

    public AnthropicProvider(String apiKey, String baseUrl) {
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
        return "anthropic";
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

            Request httpRequest = new Request.Builder()
                    .url(baseUrl + "/messages")
                    .header("x-api-key", apiKey)
                    .header("anthropic-version", API_VERSION)
                    .header("content-type", "application/json")
                    .post(RequestBody.create(jsonBody, MediaType.parse("application/json")))
                    .build();

            httpClient.newCall(httpRequest).enqueue(new Callback() {
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
                                    new RuntimeException("Anthropic API error " + response.code() + ": " + errorBody));
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
            String jsonBody = objectMapper.writeValueAsString(body);

            Request httpRequest = new Request.Builder()
                    .url(baseUrl + "/messages")
                    .header("x-api-key", apiKey)
                    .header("anthropic-version", API_VERSION)
                    .header("content-type", "application/json")
                    .post(RequestBody.create(jsonBody, MediaType.parse("application/json")))
                    .build();

            // Mutable state for accumulating the streaming response
            ChatResponse.ChatResponseBuilder responseBuilder = ChatResponse.builder();
            StringBuilder textContent = new StringBuilder();
            List<ToolUse> toolUses = new ArrayList<>();
            // Track current tool being built
            String[] currentToolId = { null };
            String[] currentToolName = { null };
            StringBuilder currentToolInput = new StringBuilder();

            EventSource.Factory factory = EventSources.createFactory(httpClient);
            factory.newEventSource(httpRequest, new EventSourceListener() {
                @Override
                public void onEvent(EventSource source, String id, String type, String data) {
                    try {
                        if ("[DONE]".equals(data))
                            return;
                        JsonNode event = objectMapper.readTree(data);

                        switch (type != null ? type : "") {
                            case "message_start" -> {
                                JsonNode msg = event.path("message");
                                responseBuilder.id(msg.path("id").asText());
                                responseBuilder.model(msg.path("model").asText());
                                JsonNode usageNode = msg.path("usage");
                                if (!usageNode.isMissingNode()) {
                                    responseBuilder.usage(Usage.builder()
                                            .inputTokens(usageNode.path("input_tokens").asInt())
                                            .build());
                                }
                            }
                            case "content_block_start" -> {
                                JsonNode cb = event.path("content_block");
                                String cbType = cb.path("type").asText();
                                if ("tool_use".equals(cbType)) {
                                    currentToolId[0] = cb.path("id").asText();
                                    currentToolName[0] = cb.path("name").asText();
                                    currentToolInput.setLength(0);
                                }
                            }
                            case "content_block_delta" -> {
                                JsonNode delta = event.path("delta");
                                String deltaType = delta.path("type").asText();
                                if ("text_delta".equals(deltaType)) {
                                    String text = delta.path("text").asText("");
                                    if (!text.isEmpty()) {
                                        textContent.append(text);
                                        listener.onText(text);
                                    }
                                } else if ("input_json_delta".equals(deltaType)) {
                                    String partial = delta.path("partial_json").asText("");
                                    currentToolInput.append(partial);
                                }
                            }
                            case "content_block_stop" -> {
                                if (currentToolId[0] != null) {
                                    @SuppressWarnings("unchecked")
                                    Map<String, Object> input = currentToolInput.length() > 0
                                            ? objectMapper.readValue(currentToolInput.toString(), Map.class)
                                            : Map.of();
                                    ToolUse tu = ToolUse.builder()
                                            .id(currentToolId[0])
                                            .name(currentToolName[0])
                                            .input(input)
                                            .build();
                                    toolUses.add(tu);
                                    listener.onToolUse(tu);
                                    currentToolId[0] = null;
                                    currentToolName[0] = null;
                                    currentToolInput.setLength(0);
                                }
                            }
                            case "message_delta" -> {
                                JsonNode delta = event.path("delta");
                                responseBuilder.stopReason(delta.path("stop_reason").asText());
                                JsonNode usageNode = event.path("usage");
                                if (!usageNode.isMissingNode()) {
                                    responseBuilder.usage(Usage.builder()
                                            .inputTokens(usageNode.path("input_tokens").asInt())
                                            .outputTokens(usageNode.path("output_tokens").asInt())
                                            .build());
                                    listener.onUsage(responseBuilder.build().getUsage());
                                }
                            }
                            case "message_stop" -> {
                                responseBuilder.message(ChatMessage.builder()
                                        .role("assistant")
                                        .content(textContent.toString())
                                        .build());
                                if (!toolUses.isEmpty()) {
                                    responseBuilder.toolUses(new ArrayList<>(toolUses));
                                }
                                future.complete(responseBuilder.build());
                            }
                            case "error" -> {
                                String errMsg = event.path("error").path("message").asText("Unknown streaming error");
                                future.completeExceptionally(new RuntimeException("Anthropic stream error: " + errMsg));
                            }
                        }
                    } catch (Exception e) {
                        log.error("Error parsing Anthropic SSE event: {}", e.getMessage(), e);
                    }
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
                        future.completeExceptionally(new RuntimeException("Anthropic stream failed: " + errMsg));
                    }
                }

                @Override
                public void onClosed(EventSource source) {
                    if (!future.isDone()) {
                        // Stream closed before message_stop — build response from what we have
                        responseBuilder.message(ChatMessage.builder()
                                .role("assistant")
                                .content(textContent.toString())
                                .build());
                        if (!toolUses.isEmpty()) {
                            responseBuilder.toolUses(new ArrayList<>(toolUses));
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
        return List.of(
                ModelInfo.builder().id("claude-sonnet-4-5-20250514").name("Claude Sonnet 4.5").contextWindow(200000)
                        .maxTokens(8192).build(),
                ModelInfo.builder().id("claude-opus-4-6-20250610").name("Claude Opus 4.6").contextWindow(200000)
                        .maxTokens(4096).build(),
                ModelInfo.builder().id("claude-3-5-haiku-20241022").name("Claude 3.5 Haiku").contextWindow(200000)
                        .maxTokens(8192).build());
    }

    private Map<String, Object> buildRequestBody(ChatRequest request) {
        Map<String, Object> body = new LinkedHashMap<>();

        String model = request.getModel();
        if (model.contains("/")) {
            model = model.substring(model.indexOf("/") + 1);
        }
        body.put("model", model);
        body.put("max_tokens", request.getMaxTokens() > 0 ? request.getMaxTokens() : 4096);

        if (request.getSystemPrompt() != null && !request.getSystemPrompt().isEmpty()) {
            body.put("system", request.getSystemPrompt());
        }

        // Convert messages
        List<Map<String, Object>> messages = new ArrayList<>();
        for (ChatMessage msg : request.getMessages()) {
            if ("system".equals(msg.getRole()))
                continue; // handled above

            Map<String, Object> m = new LinkedHashMap<>();

            if ("tool".equals(msg.getRole())) {
                // Tool results must use role "user" with tool_result content blocks
                m.put("role", "user");
                List<Map<String, Object>> content = List.of(Map.of(
                        "type", "tool_result",
                        "tool_use_id", msg.getToolUseId(),
                        "content", msg.getContent() != null ? msg.getContent() : ""));
                m.put("content", content);
            } else if ("assistant".equals(msg.getRole()) && msg.getToolUses() != null && !msg.getToolUses().isEmpty()) {
                // Assistant messages with tool uses: content is array of text + tool_use blocks
                m.put("role", "assistant");
                List<Map<String, Object>> content = new ArrayList<>();
                if (msg.getContent() != null && !msg.getContent().isEmpty()) {
                    content.add(Map.of("type", "text", "text", msg.getContent()));
                }
                for (ToolUse tu : msg.getToolUses()) {
                    content.add(Map.of(
                            "type", "tool_use",
                            "id", tu.getId(),
                            "name", tu.getName(),
                            "input", tu.getInput() != null ? tu.getInput() : Map.of()));
                }
                m.put("content", content);
            } else {
                m.put("role", msg.getRole());
                m.put("content", msg.getContent());
            }
            messages.add(m);
        }
        body.put("messages", messages);

        // Tools
        if (request.getTools() != null && !request.getTools().isEmpty()) {
            body.put("tools", request.getTools());
        }

        return body;
    }

    private ChatResponse parseResponse(String json) throws JsonProcessingException {
        JsonNode root = objectMapper.readTree(json);

        ChatResponse.ChatResponseBuilder builder = ChatResponse.builder()
                .id(root.path("id").asText())
                .model(root.path("model").asText())
                .stopReason(root.path("stop_reason").asText());

        // Parse usage
        JsonNode usageNode = root.path("usage");
        if (!usageNode.isMissingNode()) {
            builder.usage(Usage.builder()
                    .inputTokens(usageNode.path("input_tokens").asInt())
                    .outputTokens(usageNode.path("output_tokens").asInt())
                    .cacheReadTokens(usageNode.path("cache_read_input_tokens").asInt())
                    .cacheWriteTokens(usageNode.path("cache_creation_input_tokens").asInt())
                    .build());
        }

        // Parse content blocks
        JsonNode contentArray = root.path("content");
        StringBuilder textContent = new StringBuilder();
        List<ToolUse> toolUses = new ArrayList<>();

        if (contentArray.isArray()) {
            for (JsonNode block : contentArray) {
                String type = block.path("type").asText();
                if ("text".equals(type)) {
                    textContent.append(block.path("text").asText());
                } else if ("tool_use".equals(type)) {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> input = objectMapper.treeToValue(block.path("input"), Map.class);
                    toolUses.add(ToolUse.builder()
                            .id(block.path("id").asText())
                            .name(block.path("name").asText())
                            .input(input)
                            .build());
                }
            }
        }

        builder.message(ChatMessage.builder()
                .role("assistant")
                .content(textContent.toString())
                .build());

        if (!toolUses.isEmpty()) {
            builder.toolUses(toolUses);
        }

        return builder.build();
    }
}
