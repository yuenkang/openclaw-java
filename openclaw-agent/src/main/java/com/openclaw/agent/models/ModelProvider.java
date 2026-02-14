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
     * Send a streaming chat completion request.
     * The listener receives real-time events (text deltas, tool calls).
     * Default implementation falls back to non-streaming chat().
     */
    default CompletableFuture<ChatResponse> chatStream(
            ChatRequest request, StreamListener listener) {
        return chat(request).thenApply(response -> {
            // Emit text as a single delta
            if (response.getMessage() != null
                    && response.getMessage().getContent() != null
                    && !response.getMessage().getContent().isEmpty()) {
                listener.onText(response.getMessage().getContent());
            }
            // Emit tool uses
            if (response.getToolUses() != null) {
                for (ToolUse tu : response.getToolUses()) {
                    listener.onToolUse(tu);
                }
            }
            if (response.getUsage() != null) {
                listener.onUsage(response.getUsage());
            }
            return response;
        });
    }

    /**
     * List available models on this provider.
     */
    List<ModelInfo> listModels();

    // --- Streaming listener ---

    /**
     * Listener for streaming LLM events.
     * Receives real-time deltas during a chat completion.
     */
    interface StreamListener {
        /** Called when the LLM produces a text delta. */
        default void onText(String delta) {
        }

        /** Called when the LLM invokes a tool (complete tool_use block). */
        default void onToolUse(ToolUse toolUse) {
        }

        /** Called with token usage information. */
        default void onUsage(Usage usage) {
        }
    }

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
        /**
         * Reasoning/thinking content from thinking models (e.g. Claude with extended
         * thinking).
         */
        private String reasoningContent;
        private String toolUseId;
        /** Tool uses included in assistant messages (for multi-turn tool calling) */
        private List<ToolUse> toolUses;
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
