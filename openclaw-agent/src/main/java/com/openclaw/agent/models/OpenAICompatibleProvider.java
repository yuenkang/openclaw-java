package com.openclaw.agent.models;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;

import java.io.IOException;
import java.time.Duration;
import java.util.*;
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
    public List<ModelInfo> listModels() {
        try {
            Request.Builder reqBuilder = new Request.Builder()
                    .url(baseUrl + "/models")
                    .get();

            if (apiKey != null && !apiKey.isEmpty()) {
                reqBuilder.header("Authorization", "Bearer " + apiKey);
            }

            try (Response response = httpClient.newCall(reqBuilder.build()).execute()) {
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
            messages.add(Map.of("role", msg.getRole(), "content", msg.getContent()));
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
        builder.message(ChatMessage.builder()
                .role("assistant")
                .content(message.path("content").asText(""))
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
