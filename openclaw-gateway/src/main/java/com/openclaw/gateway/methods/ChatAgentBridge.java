package com.openclaw.gateway.methods;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Bridge interface between the Gateway and Agent modules.
 * Allows ChatMethodHandler to trigger agent runs without a direct dependency
 * on openclaw-agent classes.
 *
 * <p>
 * Implementation lives in openclaw-app (ChatAgentBridgeImpl) which has
 * access to both gateway and agent modules.
 */
public interface ChatAgentBridge {

    /**
     * Run a chat agent with streaming events.
     *
     * @param request the chat run request
     * @return future that completes with the chat result
     */
    CompletableFuture<ChatRunResult> runChat(ChatRunRequest request);

    /**
     * Request to run a chat agent.
     */
    record ChatRunRequest(
            String sessionKey,
            String modelId,
            String systemPrompt,
            List<Map<String, String>> messages,
            String cwd,
            int maxTokens,
            double temperature,
            ChatEventListener listener) {
        public static Builder builder() {
            return new Builder();
        }

        public static class Builder {
            private String sessionKey;
            private String modelId;
            private String systemPrompt;
            private List<Map<String, String>> messages;
            private String cwd;
            private int maxTokens = 4096;
            private double temperature = 0.7;
            private ChatEventListener listener;

            public Builder sessionKey(String val) {
                this.sessionKey = val;
                return this;
            }

            public Builder modelId(String val) {
                this.modelId = val;
                return this;
            }

            public Builder systemPrompt(String val) {
                this.systemPrompt = val;
                return this;
            }

            public Builder messages(List<Map<String, String>> val) {
                this.messages = val;
                return this;
            }

            public Builder cwd(String val) {
                this.cwd = val;
                return this;
            }

            public Builder maxTokens(int val) {
                this.maxTokens = val;
                return this;
            }

            public Builder temperature(double val) {
                this.temperature = val;
                return this;
            }

            public Builder listener(ChatEventListener val) {
                this.listener = val;
                return this;
            }

            public ChatRunRequest build() {
                return new ChatRunRequest(sessionKey, modelId, systemPrompt, messages,
                        cwd, maxTokens, temperature, listener);
            }
        }
    }

    /**
     * Result of a chat agent run.
     */
    record ChatRunResult(
            boolean success,
            String finalMessage,
            String error,
            long inputTokens,
            long outputTokens) {
    }

    /**
     * Listener for streaming chat events.
     */
    interface ChatEventListener {
        default void onDelta(String text) {
        }

        default void onToolStart(String toolName, String toolId) {
        }

        default void onToolEnd(String toolName, String toolId, String result, boolean success) {
        }

        default void onComplete(String finalMessage) {
        }

        default void onError(String error) {
        }
    }
}
