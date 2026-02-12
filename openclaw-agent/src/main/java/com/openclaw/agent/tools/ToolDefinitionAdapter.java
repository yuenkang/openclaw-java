package com.openclaw.agent.tools;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Adapts AgentTool definitions to LLM provider-specific formats.
 * Corresponds to TypeScript's pi-tool-definition-adapter.ts.
 *
 * <p>
 * Handles:
 * </p>
 * <ul>
 * <li>Anthropic tool_use format</li>
 * <li>OpenAI function calling format</li>
 * <li>Gemini function declarations format</li>
 * </ul>
 */
@Slf4j
public final class ToolDefinitionAdapter {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private ToolDefinitionAdapter() {
    }

    /**
     * Convert tools to Anthropic tool_use format.
     *
     * <pre>
     * { "name": "...", "description": "...", "input_schema": { ... } }
     * </pre>
     */
    public static List<Map<String, Object>> toAnthropicFormat(Collection<AgentTool> tools) {
        List<Map<String, Object>> result = new ArrayList<>();
        for (AgentTool tool : tools) {
            Map<String, Object> def = new LinkedHashMap<>();
            def.put("name", normalizeToolName(tool.getName()));
            def.put("description", tool.getDescription());
            JsonNode schema = ToolSchemaAdapter.normalizeToolParameters(tool.getParameterSchema());
            def.put("input_schema", schema);
            result.add(def);
        }
        return result;
    }

    /**
     * Convert tools to OpenAI function calling format.
     *
     * <pre>
     * { "type": "function", "function": { "name": "...", "description": "...", "parameters": { ... } } }
     * </pre>
     */
    public static List<Map<String, Object>> toOpenAIFormat(Collection<AgentTool> tools) {
        List<Map<String, Object>> result = new ArrayList<>();
        for (AgentTool tool : tools) {
            Map<String, Object> func = new LinkedHashMap<>();
            func.put("name", normalizeToolName(tool.getName()));
            func.put("description", tool.getDescription());
            JsonNode schema = ToolSchemaAdapter.normalizeToolParameters(tool.getParameterSchema());
            func.put("parameters", schema);

            Map<String, Object> wrapper = new LinkedHashMap<>();
            wrapper.put("type", "function");
            wrapper.put("function", func);
            result.add(wrapper);
        }
        return result;
    }

    /**
     * Convert tools to Gemini function declarations format.
     *
     * <pre>
     * { "name": "...", "description": "...", "parameters": { ... } }
     * </pre>
     */
    public static List<Map<String, Object>> toGeminiFormat(Collection<AgentTool> tools) {
        List<Map<String, Object>> result = new ArrayList<>();
        for (AgentTool tool : tools) {
            Map<String, Object> def = new LinkedHashMap<>();
            def.put("name", normalizeToolName(tool.getName()));
            def.put("description", tool.getDescription());
            JsonNode schema = ToolSchemaAdapter.cleanSchemaForGemini(
                    ToolSchemaAdapter.normalizeToolParameters(tool.getParameterSchema()));
            def.put("parameters", schema);
            result.add(def);
        }
        return result;
    }

    /**
     * Convert tools to provider-appropriate format.
     *
     * @param tools    the agent tools
     * @param provider provider name (anthropic, openai, google, etc.)
     * @return formatted tool definitions
     */
    public static List<Map<String, Object>> toProviderFormat(
            Collection<AgentTool> tools, String provider) {
        if (provider == null)
            return toAnthropicFormat(tools);
        return switch (provider.toLowerCase().trim()) {
            case "openai", "openai-codex" -> toOpenAIFormat(tools);
            case "google", "gemini" -> toGeminiFormat(tools);
            default -> toAnthropicFormat(tools);
        };
    }

    /**
     * Normalize a tool name (lowercase, replace spaces/hyphens with underscores).
     */
    public static String normalizeToolName(String name) {
        if (name == null || name.isBlank())
            return "tool";
        return name.trim().toLowerCase().replaceAll("[\\s-]+", "_");
    }

    /**
     * Execute a tool with unified error handling, matching TS adapter behavior.
     *
     * @param tool    the tool to execute
     * @param context execution context
     * @return tool result, never throws (errors wrapped in ToolResult)
     */
    public static AgentTool.ToolResult safeExecute(AgentTool tool, AgentTool.ToolContext context) {
        try {
            return tool.execute(context).join();
        } catch (Exception e) {
            String message = e.getMessage() != null ? e.getMessage() : String.valueOf(e);
            log.error("[tools] {} failed: {}", normalizeToolName(tool.getName()), message);
            if (log.isDebugEnabled() && e.getCause() != null) {
                log.debug("tools: {} failed stack:", normalizeToolName(tool.getName()), e.getCause());
            }
            ObjectNode errorPayload = MAPPER.createObjectNode();
            errorPayload.put("status", "error");
            errorPayload.put("tool", normalizeToolName(tool.getName()));
            errorPayload.put("error", message);
            return AgentTool.ToolResult.fail(errorPayload.toPrettyString());
        }
    }
}
