package com.openclaw.agent.runtime;

import com.fasterxml.jackson.databind.JsonNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolSchemaAdapter;

import java.util.Collections;
import java.util.List;

/**
 * Splits tools into built-in (SDK) and custom (OpenClaw extension) sets.
 * Corresponds to TypeScript pi-embedded-runner/tool-split.ts.
 *
 * <p>
 * All tools are passed as custom tools so that OpenClaw policy filtering,
 * sandbox integration, and extended toolset remain consistent across providers.
 * </p>
 */
public final class ToolSplit {

    private ToolSplit() {
    }

    /**
     * A tool definition for the LLM provider.
     */
    public record ToolDefinition(
            String name,
            String description,
            JsonNode parameters) {
    }

    public record SplitResult(
            /** Always empty — we use customTools exclusively. */
            List<AgentTool> builtInTools,
            /** Tool definitions for the LLM provider. */
            List<ToolDefinition> customTools) {
    }

    /**
     * Convert AgentTool list to ToolDefinition list.
     */
    public static List<ToolDefinition> toToolDefinitions(List<AgentTool> tools) {
        return tools.stream()
                .map(tool -> new ToolDefinition(
                        tool.getName(),
                        tool.getDescription(),
                        ToolSchemaAdapter.normalizeToolParameters(tool.getParameterSchema())))
                .toList();
    }

    /**
     * Split SDK tools: all go into customTools (converted to ToolDefinitions).
     *
     * @param tools          The full tool list
     * @param sandboxEnabled Whether sandbox mode is active
     * @return Split result with empty builtInTools and all tools as customTools
     */
    public static SplitResult splitSdkTools(List<AgentTool> tools, boolean sandboxEnabled) {
        return new SplitResult(Collections.emptyList(), toToolDefinitions(tools));
    }
}
