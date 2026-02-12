package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * List active sessions with optional filters.
 * Corresponds to TypeScript sessions-list-tool.ts.
 */
@Slf4j
public class SessionsListTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final Set<String> VALID_KINDS = Set.of("main", "group", "cron", "hook", "node", "other");

    @Override
    public String getName() {
        return "sessions_list";
    }

    @Override
    public String getDescription() {
        return "List sessions with optional filters and last messages.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode props = schema.putObject("properties");

        ObjectNode kinds = props.putObject("kinds");
        kinds.put("type", "array");
        kinds.putObject("items").put("type", "string");

        ObjectNode limit = props.putObject("limit");
        limit.put("type", "number");
        limit.put("minimum", 1);

        ObjectNode activeMinutes = props.putObject("activeMinutes");
        activeMinutes.put("type", "number");
        activeMinutes.put("minimum", 1);

        ObjectNode messageLimit = props.putObject("messageLimit");
        messageLimit.put("type", "number");
        messageLimit.put("minimum", 0);

        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> doExecute(context));
    }

    private ToolResult doExecute(ToolContext context) {
        try {
            JsonNode params = context.getParameters();
            OpenClawConfig cfg = context.getConfig();
            if (cfg == null) {
                return ToolResult.fail("No config available");
            }

            var sessionAlias = SessionsHelpers.resolveMainSessionAlias(cfg);

            // Parse filter params
            Set<String> allowedKinds = null;
            if (params != null && params.has("kinds") && params.get("kinds").isArray()) {
                Set<String> parsed = new LinkedHashSet<>();
                for (JsonNode k : params.get("kinds")) {
                    String v = k.asText("").trim().toLowerCase();
                    if (VALID_KINDS.contains(v))
                        parsed.add(v);
                }
                if (!parsed.isEmpty())
                    allowedKinds = parsed;
            }

            Integer limit = ToolParamUtils.readIntegerParam(params, "limit");
            if (limit != null)
                limit = Math.max(1, limit);

            Integer activeMinutes = ToolParamUtils.readIntegerParam(params, "activeMinutes");
            if (activeMinutes != null)
                activeMinutes = Math.max(1, activeMinutes);

            // TODO: call gateway sessions.list and process results
            log.debug("sessions_list: kinds={}, limit={}, activeMinutes={}", allowedKinds, limit, activeMinutes);

            ObjectNode result = MAPPER.createObjectNode();
            result.put("count", 0);
            result.putArray("sessions");
            result.put("_stub", true);
            result.put("_note", "Gateway sessions.list integration pending");

            return ToolParamUtils.jsonResult(result);
        } catch (Exception e) {
            log.error("sessions_list failed", e);
            return ToolResult.fail("sessions_list error: " + e.getMessage());
        }
    }
}
