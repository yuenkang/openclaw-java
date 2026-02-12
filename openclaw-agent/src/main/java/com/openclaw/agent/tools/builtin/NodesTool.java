package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.CompletableFuture;

/**
 * Nodes tool — interact with physical/remote nodes.
 * Corresponds to TypeScript's tools/nodes-tool.ts.
 */
@Slf4j
public class NodesTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final String[] NODE_ACTIONS = {
            "list", "pending", "approve", "reject", "notify",
            "camera_snap", "camera_list", "camera_clip",
            "screen_record", "location_get", "run", "invoke"
    };

    @Override
    public String getName() {
        return "nodes";
    }

    @Override
    public String getDescription() {
        return "Interact with connected nodes: list, approve/reject, " +
                "camera, screen, location, and remote command execution.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode properties = schema.putObject("properties");

        ObjectNode action = properties.putObject("action");
        action.put("type", "string");
        ArrayNode ae = action.putArray("enum");
        for (String a : NODE_ACTIONS)
            ae.add(a);

        addStr(properties, "node", "Target node id");
        addStr(properties, "requestId", "Request id (for approve/reject)");
        addStr(properties, "gatewayUrl", "Gateway URL override");
        addStr(properties, "gatewayToken", "Gateway auth token");
        addStr(properties, "title", "Notification title");
        addStr(properties, "body", "Notification body");
        addStr(properties, "facing", "Camera facing: front, back, both");
        addStr(properties, "cwd", "Working directory for command");
        addStr(properties, "invokeCommand", "Invoke command name");
        addStr(properties, "invokeParamsJson", "JSON params for invoke");

        ObjectNode command = properties.putObject("command");
        command.put("type", "array");
        command.putObject("items").put("type", "string");

        ObjectNode timeoutMs = properties.putObject("timeoutMs");
        timeoutMs.put("type", "number");

        schema.putArray("required").add("action");
        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> doExecute(context));
    }

    private ToolResult doExecute(ToolContext context) {
        try {
            JsonNode params = context.getParameters();
            String action = ToolParamUtils.readStringParam(params, "action");
            if (action == null || action.isBlank())
                return ToolResult.fail("'action' is required");

            String node = ToolParamUtils.readStringParam(params, "node");
            log.info("nodes-tool: action={} node={}", action, node);

            // TODO: Wire up to gateway node.* API
            ObjectNode result = MAPPER.createObjectNode();
            result.put("action", action);
            if (node != null)
                result.put("node", node);
            result.put("status", "dispatched");
            result.put("note", "Nodes tool stub — wire up gateway node API");

            return ToolResult.ok(ToolParamUtils.toJsonString(result), result);
        } catch (Exception e) {
            log.error("nodes-tool error: {}", e.getMessage(), e);
            return ToolResult.fail("Nodes tool error: " + e.getMessage());
        }
    }

    private static void addStr(ObjectNode props, String key, String desc) {
        ObjectNode p = props.putObject(key);
        p.put("type", "string");
        p.put("description", desc);
    }
}
