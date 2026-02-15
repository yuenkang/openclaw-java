package com.openclaw.agent.tools;

import com.fasterxml.jackson.databind.JsonNode;
import com.openclaw.agent.models.ModelProvider;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.concurrent.CompletableFuture;

/**
 * Agent tool interface.
 * Corresponds to TypeScript's AgentTool / AnyAgentTool.
 *
 * <p>
 * Every tool has a name, description, parameter schema, and an
 * asynchronous execute method that returns a {@link ToolResult}.
 * </p>
 */
public interface AgentTool {

    /** Unique tool name (e.g. "exec", "browser", "canvas"). */
    String getName();

    /** Human-readable description for the LLM. */
    String getDescription();

    /** JSON Schema describing the tool's input parameters. */
    JsonNode getParameterSchema();

    /** Execute the tool with the given context. */
    CompletableFuture<ToolResult> execute(ToolContext context);

    // --- Supporting types ---

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ToolResult {
        private boolean success;
        private String output;
        private Object data;
        private String error;
        /**
         * Multimodal content parts (text + images). When non-null,
         * takes precedence over plain {@code output} string.
         */
        private java.util.List<ModelProvider.ContentPart> contentParts;

        public static ToolResult ok(String output) {
            return ToolResult.builder().success(true).output(output).build();
        }

        public static ToolResult ok(String output, Object data) {
            return ToolResult.builder().success(true).output(output).data(data).build();
        }

        public static ToolResult fail(String error) {
            return ToolResult.builder().success(false).error(error).build();
        }
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ToolContext {
        private JsonNode parameters;
        private String sessionKey;
        private String cwd;
        private com.openclaw.common.config.OpenClawConfig config;
    }
}
