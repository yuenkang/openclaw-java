package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.CompletableFuture;

/**
 * Session status tool — shows session info, model, time.
 * Corresponds to TypeScript's tools/session-status-tool.ts (simplified).
 *
 * <p>
 * Actions:
 * </p>
 * <ul>
 * <li>Show session status (sessionKey, model, time)</li>
 * <li>Set per-session model override (future)</li>
 * </ul>
 */
@Slf4j
public class SessionStatusTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final String TOOL_NAME = "session_status";

    private final String agentSessionKey;
    private final OpenClawConfig config;

    private SessionStatusTool(String agentSessionKey, OpenClawConfig config) {
        this.agentSessionKey = agentSessionKey;
        this.config = config;
    }

    public static SessionStatusTool create(String agentSessionKey, OpenClawConfig config) {
        return new SessionStatusTool(agentSessionKey, config);
    }

    @Override
    public String getName() {
        return TOOL_NAME;
    }

    @Override
    public String getDescription() {
        return "Show a /status-equivalent session status card (usage + time + cost when available). "
                + "Use for model-use questions (📊 session_status). "
                + "Optional: set per-session model override (model=default resets overrides).";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode props = MAPPER.createObjectNode();
        ObjectNode sessionKeyProp = MAPPER.createObjectNode();
        sessionKeyProp.put("type", "string");
        sessionKeyProp.put("description", "Session key to inspect (defaults to current session)");
        props.set("sessionKey", sessionKeyProp);
        ObjectNode modelProp = MAPPER.createObjectNode();
        modelProp.put("type", "string");
        modelProp.put("description",
                "Set model override for this session (e.g. 'gpt-4o', 'claude-3.5-sonnet'). Use 'default' to reset.");
        props.set("model", modelProp);
        schema.set("properties", props);
        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                JsonNode params = context.getParameters();
                String requestedKey = ToolParamUtils.readStringParam(params, "sessionKey");
                String sessionKey = requestedKey != null ? requestedKey : agentSessionKey;
                String modelParam = ToolParamUtils.readStringParam(params, "model");

                if (sessionKey == null || sessionKey.isBlank()) {
                    return ToolResult.fail("sessionKey required");
                }

                // Resolve model info
                String defaultModel = resolveDefaultModel();
                String currentModel = defaultModel;
                boolean changedModel = false;

                if (modelParam != null && !modelParam.isBlank()) {
                    if ("default".equalsIgnoreCase(modelParam)) {
                        currentModel = defaultModel;
                        changedModel = true;
                        log.info("Session {} model reset to default: {}", sessionKey, defaultModel);
                    } else {
                        currentModel = modelParam;
                        changedModel = true;
                        log.info("Session {} model overridden to: {}", sessionKey, currentModel);
                    }
                }

                // Build status card
                StringBuilder sb = new StringBuilder();
                sb.append("📊 **Session Status**\n\n");
                sb.append("🔑 Session: `").append(sessionKey).append("`\n");
                sb.append("🤖 Model: `").append(currentModel).append("`");
                if (changedModel) {
                    sb.append(" (changed)");
                }
                sb.append("\n");

                // Time info
                String timezone = resolveTimezone();
                ZonedDateTime now = ZonedDateTime.now(ZoneId.of(timezone));
                String timeStr = now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
                sb.append("🕒 Time: ").append(timeStr).append(" (").append(timezone).append(")\n");

                // Config path
                sb.append("⚙️ Config: loaded\n");

                String statusText = sb.toString();

                ObjectNode details = MAPPER.createObjectNode();
                details.put("ok", true);
                details.put("sessionKey", sessionKey);
                details.put("changedModel", changedModel);
                details.put("model", currentModel);
                details.put("statusText", statusText);

                return ToolResult.ok(statusText, details);

            } catch (Exception e) {
                log.error("[session_status] failed: {}", e.getMessage());
                return ToolResult.fail("session_status error: " + e.getMessage());
            }
        });
    }

    private String resolveDefaultModel() {
        // Try root-level model first
        if (config != null && config.getModel() != null && !config.getModel().isBlank()) {
            return config.getModel();
        }
        // Try agents.defaults.model
        if (config != null && config.getAgents() != null
                && config.getAgents().getDefaults() != null) {
            String model = config.getAgents().getDefaults().getModel();
            if (model != null && !model.isBlank()) {
                return model;
            }
        }
        return "anthropic/claude-sonnet-4-20250514";
    }

    private String resolveTimezone() {
        // Use system default timezone (no userTimezone field in config yet)
        return ZoneId.systemDefault().getId();
    }
}
