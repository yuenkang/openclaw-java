package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.CompletableFuture;

/**
 * TTS tool — convert text to speech and return a media path.
 * Corresponds to TypeScript's tools/tts-tool.ts.
 */
@Slf4j
public class TtsTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Override
    public String getName() {
        return "tts";
    }

    @Override
    public String getDescription() {
        return "Convert text to speech and return a MEDIA: path. " +
                "Use when the user requests audio or TTS is enabled.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode properties = schema.putObject("properties");

        ObjectNode text = properties.putObject("text");
        text.put("type", "string");
        text.put("description", "Text to convert to speech");

        ObjectNode channel = properties.putObject("channel");
        channel.put("type", "string");
        channel.put("description", "Channel id to pick output format");

        schema.putArray("required").add("text");
        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> doExecute(context));
    }

    private ToolResult doExecute(ToolContext context) {
        try {
            JsonNode params = context.getParameters();
            String text = ToolParamUtils.readStringParam(params, "text");
            if (text == null || text.isBlank())
                return ToolResult.fail("'text' is required");

            String channel = ToolParamUtils.readStringParam(params, "channel");
            log.info("tts-tool: textLen={} channel={}", text.length(), channel);

            // TODO: Wire up to TTS provider
            ObjectNode result = MAPPER.createObjectNode();
            result.put("text", text.length() > 100 ? text.substring(0, 100) + "..." : text);
            result.put("textLength", text.length());
            if (channel != null)
                result.put("channel", channel);
            result.put("status", "pending");
            result.put("note", "TTS stub — wire up TTS provider");

            return ToolResult.ok(ToolParamUtils.toJsonString(result), result);
        } catch (Exception e) {
            log.error("tts-tool error: {}", e.getMessage(), e);
            return ToolResult.fail("TTS tool error: " + e.getMessage());
        }
    }
}
