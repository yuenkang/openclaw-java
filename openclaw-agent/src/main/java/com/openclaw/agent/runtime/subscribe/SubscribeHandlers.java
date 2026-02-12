package com.openclaw.agent.runtime.subscribe;

import com.openclaw.agent.runtime.subscribe.HandlerTypes.SubscribeContext;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.function.Consumer;

/**
 * Event handler router — dispatches subscribe events to lifecycle, message, and
 * tool handlers.
 * Corresponds to TypeScript pi-embedded-subscribe.handlers.ts.
 */
@Slf4j
public final class SubscribeHandlers {

    private SubscribeHandlers() {
    }

    /**
     * Create an event handler function that routes events to the appropriate
     * handler.
     */
    @SuppressWarnings("unchecked")
    public static Consumer<Map<String, Object>> createEventHandler(SubscribeContext ctx) {
        return evt -> {
            String type = evt.get("type") instanceof String s ? s : "";
            switch (type) {
                case "message_start" -> MessageHandlers.handleMessageStart(ctx, evt);
                case "message_update" -> MessageHandlers.handleMessageUpdate(ctx, evt);
                case "message_end" -> MessageHandlers.handleMessageEnd(ctx, evt);
                case "tool_execution_start" -> {
                    try {
                        ToolHandlers.handleToolExecutionStart(ctx, evt);
                    } catch (Exception e) {
                        log.debug("tool_execution_start handler failed: {}", e.getMessage());
                    }
                }
                case "tool_execution_update" -> ToolHandlers.handleToolExecutionUpdate(ctx, evt);
                case "tool_execution_end" -> ToolHandlers.handleToolExecutionEnd(ctx, evt);
                case "agent_start" -> LifecycleHandlers.handleAgentStart(ctx);
                case "auto_compaction_start" -> LifecycleHandlers.handleAutoCompactionStart(ctx);
                case "auto_compaction_end" -> {
                    boolean willRetry = Boolean.TRUE.equals(evt.get("willRetry"));
                    LifecycleHandlers.handleAutoCompactionEnd(ctx, willRetry);
                }
                case "agent_end" -> LifecycleHandlers.handleAgentEnd(ctx);
                default -> {
                    /* ignore unknown events */ }
            }
        };
    }
}
