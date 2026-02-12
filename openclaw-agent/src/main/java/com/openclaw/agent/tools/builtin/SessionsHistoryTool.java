package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;

/**
 * Fetch message history for a session.
 * Corresponds to TypeScript sessions-history-tool.ts.
 */
@Slf4j
public class SessionsHistoryTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final int SESSIONS_HISTORY_TEXT_MAX_CHARS = 4000;

    @Override
    public String getName() {
        return "sessions_history";
    }

    @Override
    public String getDescription() {
        return "Fetch message history for a session.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode props = schema.putObject("properties");

        props.putObject("sessionKey").put("type", "string");

        ObjectNode limit = props.putObject("limit");
        limit.put("type", "number");
        limit.put("minimum", 1);

        props.putObject("includeTools").put("type", "boolean");

        schema.putArray("required").add("sessionKey");
        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> doExecute(context));
    }

    private ToolResult doExecute(ToolContext context) {
        try {
            JsonNode params = context.getParameters();
            String sessionKeyParam = ToolParamUtils.readStringParam(params, "sessionKey", true);
            OpenClawConfig cfg = context.getConfig();
            if (cfg == null) {
                return ToolResult.fail("No config available");
            }

            var sessionAlias = SessionsHelpers.resolveMainSessionAlias(cfg);
            String mainKey = sessionAlias.mainKey();
            String alias = sessionAlias.alias();

            // Resolve session reference
            var resolved = SessionsHelpers.resolveSessionReference(
                    sessionKeyParam, alias, mainKey, null, false);
            if (!resolved.isOk()) {
                var err = (SessionsHelpers.SessionReferenceResolution.Error) resolved;
                ObjectNode errResult = MAPPER.createObjectNode();
                errResult.put("status", err.status());
                errResult.put("error", err.error());
                return ToolParamUtils.jsonResult(errResult);
            }

            var ok = (SessionsHelpers.SessionReferenceResolution.Ok) resolved;
            String displayKey = ok.displayKey();

            Integer limit = ToolParamUtils.readIntegerParam(params, "limit");
            if (limit != null)
                limit = Math.max(1, limit);
            boolean includeTools = ToolParamUtils.readBoolParam(params, "includeTools", false);

            // TODO: call gateway chat.history with resolvedKey and limit
            log.debug("sessions_history: sessionKey={}, limit={}, includeTools={}",
                    displayKey, limit, includeTools);

            ObjectNode result = MAPPER.createObjectNode();
            result.put("sessionKey", displayKey);
            result.putArray("messages");
            result.put("truncated", false);
            result.put("droppedMessages", false);
            result.put("contentTruncated", false);
            result.put("bytes", 0);
            result.put("_stub", true);
            result.put("_note", "Gateway chat.history integration pending");

            return ToolParamUtils.jsonResult(result);
        } catch (IllegalArgumentException e) {
            return ToolResult.fail(e.getMessage());
        } catch (Exception e) {
            log.error("sessions_history failed", e);
            return ToolResult.fail("sessions_history error: " + e.getMessage());
        }
    }

    // --- History sanitization helpers (used when gateway integration is complete)
    // ---

    static String truncateHistoryText(String text) {
        if (text.length() <= SESSIONS_HISTORY_TEXT_MAX_CHARS)
            return text;
        return text.substring(0, SESSIONS_HISTORY_TEXT_MAX_CHARS) + "\n…(truncated)…";
    }

    static int jsonUtf8Bytes(Object value) {
        try {
            String json = new ObjectMapper().writeValueAsString(value);
            return json.getBytes(StandardCharsets.UTF_8).length;
        } catch (Exception e) {
            return String.valueOf(value).getBytes(StandardCharsets.UTF_8).length;
        }
    }
}
