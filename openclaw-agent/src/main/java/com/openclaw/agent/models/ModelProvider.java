package com.openclaw.agent.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Model provider interface.
 * Corresponds to TypeScript's model provider resolution
 * in models-config.providers.ts.
 */
public interface ModelProvider {

    /** Provider identifier (e.g. "anthropic", "openai", "ollama"). */
    String getId();

    /** API base URL. */
    String getApiBaseUrl();

    /**
     * Send a chat completion request and return the full response.
     */
    CompletableFuture<ChatResponse> chat(ChatRequest request);

    /**
     * List available models on this provider.
     */
    List<ModelInfo> listModels();

    // --- Supporting types ---

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ChatRequest {
        private String model;
        private List<ChatMessage> messages;
        private int maxTokens;
        private double temperature;
        private List<Map<String, Object>> tools;
        private String systemPrompt;
        private boolean stream;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ChatResponse {
        private String id;
        private String model;
        private ChatMessage message;
        private List<ToolUse> toolUses;
        private Usage usage;
        private String stopReason;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ChatMessage {
        /** "system" | "user" | "assistant" | "tool" */
        private String role;
        private String content;
        private String toolUseId;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ToolUse {
        private String id;
        private String name;
        private Map<String, Object> input;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class Usage {
        private int inputTokens;
        private int outputTokens;
        private int cacheReadTokens;
        private int cacheWriteTokens;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ModelInfo {
        private String id;
        private String name;
        private int contextWindow;
        private int maxTokens;
    }
}
