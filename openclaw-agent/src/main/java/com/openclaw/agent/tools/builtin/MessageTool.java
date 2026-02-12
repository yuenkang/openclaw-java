package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.Set;
import java.util.concurrent.CompletableFuture;

/**
 * Message tool — send, delete, and manage messages via channel plugins.
 * Corresponds to TypeScript's tools/message-tool.ts.
 */
@Slf4j
public class MessageTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final Set<String> EXPLICIT_TARGET_ACTIONS = Set.of(
            "send", "sendWithEffect", "sendAttachment", "reply", "thread-reply", "broadcast");

    private static final String[] ALL_ACTIONS = {
            "send", "reply", "thread-reply", "broadcast",
            "edit", "delete", "react",
            "pin", "unpin", "fetch", "fetch-thread-list",
            "poll", "sendWithEffect", "sendAttachment",
            "sticker-send", "sticker-list",
            "thread-create", "thread-join", "thread-archive",
            "event-create", "event-list", "event-delete",
            "presence", "typing",
            "channel-create", "channel-update", "channel-delete",
            "ban", "unban", "kick", "timeout", "untimeout",
            "role-list", "role-add", "role-remove"
    };

    private final String currentChannelId;
    private final String currentChannelProvider;
    private final boolean requireExplicitTarget;

    public MessageTool() {
        this(null, null, false);
    }

    public MessageTool(String currentChannelId, String currentChannelProvider,
            boolean requireExplicitTarget) {
        this.currentChannelId = currentChannelId;
        this.currentChannelProvider = currentChannelProvider;
        this.requireExplicitTarget = requireExplicitTarget;
    }

    @Override
    public String getName() {
        return "message";
    }

    @Override
    public String getDescription() {
        StringBuilder sb = new StringBuilder(
                "Send, delete, and manage messages via channel plugins.");
        if (currentChannelProvider != null) {
            sb.append(" Current channel: ").append(currentChannelProvider).append(".");
        }
        sb.append(" Supports actions: send, reply, react, delete, edit, poll, pin, threads, and more.");
        return sb.toString();
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode properties = schema.putObject("properties");

        ObjectNode action = properties.putObject("action");
        action.put("type", "string");
        action.put("description", "Message action to perform");
        ArrayNode enumArr = action.putArray("enum");
        for (String a : ALL_ACTIONS) {
            enumArr.add(a);
        }

        addStr(properties, "channel", "Channel provider (e.g. discord, telegram)");
        addStr(properties, "target", "Target channel/user id or name");
        addStr(properties, "accountId", "Account id for multi-account setups");
        addStr(properties, "message", "Message text to send");
        addStr(properties, "media", "Media URL or local path");
        addStr(properties, "buffer", "Base64 payload for attachments");
        addStr(properties, "filename", "Filename for attachments");
        addStr(properties, "replyTo", "Message id to reply to");
        addStr(properties, "threadId", "Thread id");
        addStr(properties, "effectId", "Message effect id");
        addStr(properties, "messageId", "Message id for reactions/edits");
        addStr(properties, "emoji", "Emoji for reactions");
        addStr(properties, "pollQuestion", "Poll question");
        addStr(properties, "reason", "Moderation reason");
        addStr(properties, "gatewayUrl", "Gateway URL override");
        addStr(properties, "gatewayToken", "Gateway auth token");
        addStr(properties, "name", "Channel/thread name");

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
            if (action == null || action.isBlank()) {
                return ToolResult.fail("'action' is required");
            }

            if (requireExplicitTarget && EXPLICIT_TARGET_ACTIONS.contains(action)) {
                String target = ToolParamUtils.readStringParam(params, "target");
                String channelId = ToolParamUtils.readStringParam(params, "channelId");
                if ((target == null || target.isBlank()) &&
                        (channelId == null || channelId.isBlank())) {
                    return ToolResult.fail(
                            "Explicit message target required. Provide target/targets.");
                }
            }

            String message = ToolParamUtils.readStringParam(params, "message");
            String channel = ToolParamUtils.readStringParam(params, "channel");
            String target = ToolParamUtils.readStringParam(params, "target");

            if (channel == null && currentChannelProvider != null)
                channel = currentChannelProvider;
            if (target == null && currentChannelId != null)
                target = currentChannelId;

            log.info("message-tool: action={} channel={} target={}", action, channel, target);

            // TODO: Wire up to actual channel plugin / gateway dispatch
            ObjectNode result = MAPPER.createObjectNode();
            result.put("action", action);
            result.put("channel", channel != null ? channel : "");
            result.put("target", target != null ? target : "");
            if (message != null)
                result.put("message", message);
            result.put("status", "dispatched");
            result.put("note", "Message tool stub — wire up channel plugin dispatch");

            return ToolResult.ok(ToolParamUtils.toJsonString(result), result);

        } catch (Exception e) {
            log.error("message-tool error: {}", e.getMessage(), e);
            return ToolResult.fail("Message tool error: " + e.getMessage());
        }
    }

    private static void addStr(ObjectNode props, String key, String desc) {
        ObjectNode p = props.putObject(key);
        p.put("type", "string");
        p.put("description", desc);
    }
}
