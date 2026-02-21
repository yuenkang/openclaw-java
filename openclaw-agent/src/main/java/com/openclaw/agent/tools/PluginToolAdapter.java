package com.openclaw.agent.tools;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.plugin.tools.PluginToolResolver.ResolvedPluginTool;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Adapter that wraps a plugin-resolved tool as an AgentTool.
 * Bridges the plugin tool system to the agent tool registry.
 */
public class PluginToolAdapter implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private final ResolvedPluginTool pluginTool;

    public PluginToolAdapter(ResolvedPluginTool pluginTool) {
        this.pluginTool = pluginTool;
    }

    @Override
    public String getName() {
        return pluginTool.getName();
    }

    @Override
    public String getDescription() {
        return pluginTool.getDescription() != null
                ? pluginTool.getDescription()
                : "Plugin tool: " + pluginTool.getName();
    }

    @Override
    public JsonNode getParameterSchema() {
        if (pluginTool.getSchema() != null) {
            return MAPPER.valueToTree(pluginTool.getSchema());
        }
        // Default empty schema
        return MAPPER.valueToTree(Map.of(
                "type", "object",
                "properties", Map.of()));
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                if (pluginTool.getHandler() == null) {
                    return ToolResult.fail("Plugin tool has no handler");
                }
                // Convert JsonNode parameters to Map
                Map<String, Object> params = new HashMap<>();
                if (context.getParameters() != null) {
                    params = MAPPER.convertValue(context.getParameters(),
                            MAPPER.getTypeFactory().constructMapType(
                                    HashMap.class, String.class, Object.class));
                }
                String result = pluginTool.getHandler().apply(params);
                return ToolResult.ok(result != null ? result : "");
            } catch (Exception e) {
                return ToolResult.fail("Plugin tool error: " + e.getMessage());
            }
        });
    }
}
