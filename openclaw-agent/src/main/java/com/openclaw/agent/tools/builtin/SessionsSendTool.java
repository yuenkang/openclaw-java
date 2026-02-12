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
 * Send a message into another session.
 * Corresponds to TypeScript sessions-send-tool.ts.
 */
@Slf4j
public class SessionsSendTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private final String agentSessionKey;
    private final String agentChannel;
    private final boolean sandboxed;

    public SessionsSendTool(String agentSessionKey, String agentChannel, boolean sandboxed) {
        this.agentSessionKey = agentSessionKey;
        this.agentChannel = agentChannel;
        this.sandboxed = sandboxed;
    }

    public SessionsSendTool() {
        this(null, null, false);
    }

    @Override
    public String getName() {
        return "sessions_send";
    }

    @Override
    public String getDescription() {
        return "Send a message into another session. Use sessionKey or label to identify the target.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode props = schema.putObject("properties");

        props.putObject("sessionKey").put("type", "string");

        ObjectNode label = props.putObject("label");
        label.put("type", "string");
        label.put("minLength", 1);
        label.put("maxLength", 64);

        ObjectNode agentId = props.putObject("agentId");
        agentId.put("type", "string");
        agentId.put("minLength", 1);
        agentId.put("maxLength", 64);

        props.putObject("message").put("type", "string");

        ObjectNode timeout = props.putObject("timeoutSeconds");
        timeout.put("type", "number");
        timeout.put("minimum", 0);

        schema.putArray("required").add("message");
        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> doExecute(context));
    }

    private ToolResult doExecute(ToolContext context) {
        try {
            JsonNode params = context.getParameters();
            String message = ToolParamUtils.readStringParam(params, "message", true);
            String sessionKeyParam = ToolParamUtils.readStringParam(params, "sessionKey");
            String labelParam = ToolParamUtils.readStringParam(params, "label");

            OpenClawConfig cfg = context.getConfig();
            if (cfg == null) {
                return ToolResult.fail("No config available");
            }

            // Validate: sessionKey or label, not both
            if (sessionKeyParam != null && labelParam != null) {
                ObjectNode err = MAPPER.createObjectNode();
                err.put("runId", UUID.randomUUID().toString());
                err.put("status", "error");
                err.put("error", "Provide either sessionKey or label (not both).");
                return ToolParamUtils.jsonResult(err);
            }

            if (sessionKeyParam == null && labelParam == null) {
                ObjectNode err = MAPPER.createObjectNode();
                err.put("runId", UUID.randomUUID().toString());
                err.put("status", "error");
                err.put("error", "Either sessionKey or label is required");
                return ToolParamUtils.jsonResult(err);
            }

            var sessionAlias = SessionsHelpers.resolveMainSessionAlias(cfg);
            String mainKey = sessionAlias.mainKey();
            String alias = sessionAlias.alias();

            // Resolve session key
            String targetKey = sessionKeyParam;
            if (targetKey == null) {
                // TODO: resolve via gateway sessions.resolve with label
                log.debug("sessions_send: label-based resolution for '{}'", labelParam);
                ObjectNode err = MAPPER.createObjectNode();
                err.put("runId", UUID.randomUUID().toString());
                err.put("status", "error");
                err.put("error", "Label-based session resolution requires gateway integration");
                err.put("_stub", true);
                return ToolParamUtils.jsonResult(err);
            }

            // Resolve session reference
            var resolved = SessionsHelpers.resolveSessionReference(
                    targetKey, alias, mainKey, null, false);
            if (!resolved.isOk()) {
                var errRes = (SessionsHelpers.SessionReferenceResolution.Error) resolved;
                ObjectNode err = MAPPER.createObjectNode();
                err.put("runId", UUID.randomUUID().toString());
                err.put("status", errRes.status());
                err.put("error", errRes.error());
                return ToolParamUtils.jsonResult(err);
            }

            var ok = (SessionsHelpers.SessionReferenceResolution.Ok) resolved;
            String resolvedKey = ok.key();
            String displayKey = ok.displayKey();

            // A2A policy check
            var a2aPolicy = SessionsHelpers.createAgentToAgentPolicy(cfg);
            String requesterAgentId = SessionsHelpers.resolveAgentIdFromSessionKey(agentSessionKey);
            String targetAgentId = SessionsHelpers.resolveAgentIdFromSessionKey(resolvedKey);
            boolean isCrossAgent = !requesterAgentId.equals(targetAgentId);

            if (isCrossAgent) {
                if (!a2aPolicy.isEnabled()) {
                    ObjectNode err = MAPPER.createObjectNode();
                    err.put("runId", UUID.randomUUID().toString());
                    err.put("status", "forbidden");
                    err.put("error", "Agent-to-agent messaging is disabled. Set tools.agentToAgent.enabled=true.");
                    err.put("sessionKey", displayKey);
                    return ToolParamUtils.jsonResult(err);
                }
                if (!a2aPolicy.isAllowed(requesterAgentId, targetAgentId)) {
                    ObjectNode err = MAPPER.createObjectNode();
                    err.put("runId", UUID.randomUUID().toString());
                    err.put("status", "forbidden");
                    err.put("error", "Agent-to-agent messaging denied by tools.agentToAgent.allow.");
                    err.put("sessionKey", displayKey);
                    return ToolParamUtils.jsonResult(err);
                }
            }

            int timeoutSeconds = ToolParamUtils.readIntParam(params, "timeoutSeconds", 30);
            timeoutSeconds = Math.max(0, timeoutSeconds);

            String runId = UUID.randomUUID().toString();

            // TODO: call gateway agent method with message, then agent.wait
            log.debug("sessions_send: target={}, message='{}', timeout={}s",
                    displayKey, message.substring(0, Math.min(50, message.length())), timeoutSeconds);

            ObjectNode result = MAPPER.createObjectNode();
            result.put("runId", runId);
            result.put("status", "accepted");
            result.put("sessionKey", displayKey);
            result.put("_stub", true);
            result.put("_note", "Gateway agent dispatch integration pending");

            return ToolParamUtils.jsonResult(result);
        } catch (IllegalArgumentException e) {
            return ToolResult.fail(e.getMessage());
        } catch (Exception e) {
            log.error("sessions_send failed", e);
            return ToolResult.fail("sessions_send error: " + e.getMessage());
        }
    }
}
