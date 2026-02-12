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
 * Cron tool — manage scheduled tasks (reminders, recurring jobs).
 * Corresponds to TypeScript's tools/cron-tool.ts.
 */
@Slf4j
public class CronTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final String[] CRON_ACTIONS = { "list", "create", "update", "delete", "pause", "resume" };
    private static final String[] WAKE_MODES = { "message", "cron", "hook" };

    @Override
    public String getName() {
        return "cron";
    }

    @Override
    public String getDescription() {
        return "Manage scheduled tasks and reminders. " +
                "Actions: list, create, update, delete, pause, resume.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode properties = schema.putObject("properties");

        ObjectNode action = properties.putObject("action");
        action.put("type", "string");
        action.put("description", "Cron action");
        ArrayNode ae = action.putArray("enum");
        for (String a : CRON_ACTIONS)
            ae.add(a);

        addStr(properties, "id", "Cron job id");
        addStr(properties, "label", "Human-readable label");
        addStr(properties, "cron", "Cron expression (e.g. '0 9 * * *')");
        addStr(properties, "at", "One-time trigger time (ISO-8601 or relative)");
        addStr(properties, "timezone", "Timezone (default: UTC)");
        addStr(properties, "text", "Message text for the reminder");

        ObjectNode mode = properties.putObject("mode");
        mode.put("type", "string");
        ArrayNode me = mode.putArray("enum");
        for (String m : WAKE_MODES)
            me.add(m);

        addStr(properties, "gatewayUrl", "Gateway URL override");
        addStr(properties, "gatewayToken", "Gateway auth token");

        ObjectNode ctx = properties.putObject("contextMessages");
        ctx.put("type", "number");
        ctx.put("description", "Recent messages for context (0-10)");

        ObjectNode incDis = properties.putObject("includeDisabled");
        incDis.put("type", "boolean");
        incDis.put("description", "Include paused cron jobs in list");

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

            log.info("cron-tool: action={}", action);

            switch (action) {
                case "create": {
                    String cron = ToolParamUtils.readStringParam(params, "cron");
                    String at = ToolParamUtils.readStringParam(params, "at");
                    if ((cron == null || cron.isBlank()) && (at == null || at.isBlank())) {
                        return ToolResult.fail("Either 'cron' or 'at' required for create");
                    }
                    return stubResult(action, params);
                }
                case "update":
                case "delete":
                case "pause":
                case "resume": {
                    String id = ToolParamUtils.readStringParam(params, "id");
                    if (id == null || id.isBlank())
                        return ToolResult.fail("'id' required for " + action);
                    return stubResult(action, params);
                }
                case "list":
                    return stubResult(action, params);
                default:
                    return ToolResult.fail("Unknown cron action: " + action);
            }
        } catch (Exception e) {
            log.error("cron-tool error: {}", e.getMessage(), e);
            return ToolResult.fail("Cron tool error: " + e.getMessage());
        }
    }

    private ToolResult stubResult(String action, JsonNode params) {
        ObjectNode result = MAPPER.createObjectNode();
        result.put("action", action);
        String id = ToolParamUtils.readStringParam(params, "id");
        if (id != null)
            result.put("id", id);
        String label = ToolParamUtils.readStringParam(params, "label");
        if (label != null)
            result.put("label", label);
        result.put("status", "list".equals(action) ? "ok" : action + "d");
        result.put("note", "Cron stub — wire up gateway cron API");
        return ToolResult.ok(ToolParamUtils.toJsonString(result), result);
    }

    private static void addStr(ObjectNode props, String key, String desc) {
        ObjectNode p = props.putObject(key);
        p.put("type", "string");
        p.put("description", desc);
    }
}
