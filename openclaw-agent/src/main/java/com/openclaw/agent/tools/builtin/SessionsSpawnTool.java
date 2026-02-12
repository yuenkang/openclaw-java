package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;

/**
 * Spawn a background sub-agent run in an isolated session.
 * Corresponds to TypeScript sessions-spawn-tool.ts.
 */
@Slf4j
public class SessionsSpawnTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private final String agentSessionKey;
    private final String agentChannel;
    private final boolean sandboxed;

    public SessionsSpawnTool(String agentSessionKey, String agentChannel, boolean sandboxed) {
        this.agentSessionKey = agentSessionKey;
        this.agentChannel = agentChannel;
        this.sandboxed = sandboxed;
    }

    public SessionsSpawnTool() {
        this(null, null, false);
    }

    @Override
    public String getName() {
        return "sessions_spawn";
    }

    @Override
    public String getDescription() {
        return "Spawn a background sub-agent run in an isolated session and announce the result back.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode props = schema.putObject("properties");

        props.putObject("task").put("type", "string");
        props.putObject("label").put("type", "string");
        props.putObject("agentId").put("type", "string");
        props.putObject("model").put("type", "string");
        props.putObject("thinking").put("type", "string");

        ObjectNode runTimeout = props.putObject("runTimeoutSeconds");
        runTimeout.put("type", "number");
        runTimeout.put("minimum", 0);

        ObjectNode timeout = props.putObject("timeoutSeconds");
        timeout.put("type", "number");
        timeout.put("minimum", 0);

        ObjectNode cleanup = props.putObject("cleanup");
        cleanup.put("type", "string");
        var cleanupEnum = cleanup.putArray("enum");
        cleanupEnum.add("delete");
        cleanupEnum.add("keep");

        schema.putArray("required").add("task");
        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> doExecute(context));
    }

    private ToolResult doExecute(ToolContext context) {
        try {
            JsonNode params = context.getParameters();
            String task = ToolParamUtils.readStringParam(params, "task", true);
            String label = ToolParamUtils.readStringParam(params, "label");
            String requestedAgentId = ToolParamUtils.readStringParam(params, "agentId");
            String modelOverride = ToolParamUtils.readStringParam(params, "model");
            String thinkingOverride = ToolParamUtils.readStringParam(params, "thinking");
            String cleanup = ToolParamUtils.readStringParam(params, "cleanup");
            if (cleanup == null || (!"delete".equals(cleanup) && !"keep".equals(cleanup))) {
                cleanup = "keep";
            }

            OpenClawConfig cfg = context.getConfig();
            if (cfg == null) {
                return ToolResult.fail("No config available");
            }

            var sessionAlias = SessionsHelpers.resolveMainSessionAlias(cfg);
            String mainKey = sessionAlias.mainKey();
            String alias = sessionAlias.alias();

            // Reject spawn from sub-agent sessions
            if (agentSessionKey != null && SessionsHelpers.isSubagentSessionKey(agentSessionKey)) {
                ObjectNode err = MAPPER.createObjectNode();
                err.put("status", "forbidden");
                err.put("error", "sessions_spawn is not allowed from sub-agent sessions");
                return ToolParamUtils.jsonResult(err);
            }

            String requesterInternalKey = agentSessionKey != null
                    ? SessionsHelpers.resolveInternalSessionKey(agentSessionKey, alias, mainKey)
                    : alias;

            String requesterAgentId = SessionsHelpers.resolveAgentIdFromSessionKey(requesterInternalKey);
            String targetAgentId = requestedAgentId != null
                    ? requestedAgentId.trim().toLowerCase()
                    : requesterAgentId;

            // Cross-agent permission check
            if (!targetAgentId.equals(requesterAgentId)) {
                log.debug("Cross-agent spawn: {} -> {}", requesterAgentId, targetAgentId);
                // TODO: check subagents.allowAgents config
            }

            int runTimeoutSeconds = ToolParamUtils.readIntParam(params, "runTimeoutSeconds", 0);
            if (runTimeoutSeconds == 0) {
                runTimeoutSeconds = ToolParamUtils.readIntParam(params, "timeoutSeconds", 0);
            }
            runTimeoutSeconds = Math.max(0, runTimeoutSeconds);

            // Generate child session key
            String childSessionKey = "agent:" + targetAgentId + ":subagent:" + UUID.randomUUID();
            String childRunId = UUID.randomUUID().toString();

            // TODO: call gateway agent method to start the child run
            // TODO: register subagent run in SubagentRegistry
            log.debug("sessions_spawn: task='{}', child={}, agent={}, model={}, thinking={}",
                    task.substring(0, Math.min(50, task.length())),
                    childSessionKey, targetAgentId, modelOverride, thinkingOverride);

            ObjectNode result = MAPPER.createObjectNode();
            result.put("status", "accepted");
            result.put("childSessionKey", childSessionKey);
            result.put("runId", childRunId);
            if (modelOverride != null) {
                result.put("modelApplied", false);
            }
            result.put("_stub", true);
            result.put("_note", "Gateway agent dispatch integration pending");

            return ToolParamUtils.jsonResult(result);
        } catch (IllegalArgumentException e) {
            return ToolResult.fail(e.getMessage());
        } catch (Exception e) {
            log.error("sessions_spawn failed", e);
            return ToolResult.fail("sessions_spawn error: " + e.getMessage());
        }
    }
}
