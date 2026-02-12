package com.openclaw.agent.runtime.subscribe;

import com.openclaw.agent.runtime.subscribe.HandlerTypes.MessagingToolSend;
import com.openclaw.agent.runtime.subscribe.HandlerTypes.SubscribeContext;
import com.openclaw.agent.runtime.subscribe.HandlerTypes.ToolErrorSummary;
import com.openclaw.agent.runtime.subscribe.HandlerTypes.ToolMeta;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * Tool execution event handlers: tool_execution_start, _update, _end.
 * Corresponds to TypeScript pi-embedded-subscribe.handlers.tools.ts.
 */
@Slf4j
public final class ToolHandlers {

    private ToolHandlers() {
    }

    /**
     * Handle tool execution start — flush block replies, emit tool summary, track
     * messaging tools.
     */
    @SuppressWarnings("unchecked")
    public static void handleToolExecutionStart(SubscribeContext ctx, Map<String, Object> evt) {
        // Flush pending block replies before tool execution
        ctx.getFlushBlockReplyBuffer().run();
        if (ctx.getParams().onBlockReplyFlush != null) {
            ctx.getParams().onBlockReplyFlush.run();
        }

        String rawToolName = String.valueOf(evt.get("toolName"));
        String toolName = normalizeToolName(rawToolName);
        String toolCallId = String.valueOf(evt.get("toolCallId"));
        Object args = evt.get("args");

        // Warn on empty read path
        if ("read".equals(toolName) && args instanceof Map<?, ?> argsMap) {
            Object pathObj = argsMap.get("path");
            if (!(pathObj instanceof String s) || s.isBlank()) {
                log.warn("read tool called without path: toolCallId={}", toolCallId);
            }
        }

        String meta = extendExecMeta(toolName, args, null);
        ctx.getState().getToolMetaById().put(toolCallId, meta != null ? meta : "");
        log.debug("embedded run tool start: runId={} tool={} toolCallId={}",
                ctx.getParams().runId, toolName, toolCallId);

        // Emit agent event
        emitAgentEvent(ctx, "tool", Map.of(
                "phase", "start", "name", toolName, "toolCallId", toolCallId));

        // Emit tool summary
        if (ctx.getParams().onToolResult != null
                && ctx.getShouldEmitToolResult().test("")
                && !ctx.getState().getToolSummaryById().contains(toolCallId)) {
            ctx.getState().getToolSummaryById().add(toolCallId);
            ctx.getEmitToolSummary().accept(toolName, meta);
        }

        // Track messaging tool sends (pending until confirmed)
        if (EmbeddedMessaging.isMessagingTool(toolName) && args instanceof Map<?, ?> argsMap) {
            Map<String, Object> argsRecord = (Map<String, Object>) argsMap;
            if (EmbeddedMessaging.isMessagingToolSendAction(toolName, argsRecord)) {
                MessagingToolSend sendTarget = SubscribeToolUtils.extractMessagingToolSend(toolName, argsRecord);
                if (sendTarget != null) {
                    ctx.getState().getPendingMessagingTargets().put(toolCallId, sendTarget);
                }
                String text = argsRecord.get("content") instanceof String s ? s
                        : (argsRecord.get("message") instanceof String s2 ? s2 : null);
                if (text != null) {
                    ctx.getState().getPendingMessagingTexts().put(toolCallId, text);
                    log.debug("Tracking pending messaging text: tool={} len={}", toolName, text.length());
                }
            }
        }
    }

    /**
     * Handle tool execution update — emit partial result events.
     */
    @SuppressWarnings("unchecked")
    public static void handleToolExecutionUpdate(SubscribeContext ctx, Map<String, Object> evt) {
        String toolName = normalizeToolName(String.valueOf(evt.get("toolName")));
        String toolCallId = String.valueOf(evt.get("toolCallId"));
        Object partial = evt.get("partialResult");
        Object sanitized = SubscribeToolUtils.sanitizeToolResult(partial);
        emitAgentEvent(ctx, "tool", Map.of(
                "phase", "update", "name", toolName, "toolCallId", toolCallId));
    }

    /**
     * Handle tool execution end — commit messaging texts, emit result events, track
     * errors.
     */
    @SuppressWarnings("unchecked")
    public static void handleToolExecutionEnd(SubscribeContext ctx, Map<String, Object> evt) {
        String toolName = normalizeToolName(String.valueOf(evt.get("toolName")));
        String toolCallId = String.valueOf(evt.get("toolCallId"));
        boolean isError = Boolean.TRUE.equals(evt.get("isError"));
        Object result = evt.get("result");
        boolean isToolError = isError || SubscribeToolUtils.isToolResultError(result);
        Object sanitizedResult = SubscribeToolUtils.sanitizeToolResult(result);
        String meta = ctx.getState().getToolMetaById().get(toolCallId);
        ctx.getState().getToolMetas().add(new ToolMeta(toolName, meta));
        ctx.getState().getToolMetaById().remove(toolCallId);
        ctx.getState().getToolSummaryById().remove(toolCallId);

        if (isToolError) {
            String errorMessage = SubscribeToolUtils.extractToolErrorMessage(sanitizedResult);
            ctx.getState().setLastToolError(new ToolErrorSummary(toolName, meta, errorMessage));
        }

        // Commit messaging tool text on success, discard on error
        String pendingText = ctx.getState().getPendingMessagingTexts().get(toolCallId);
        MessagingToolSend pendingTarget = ctx.getState().getPendingMessagingTargets().get(toolCallId);
        if (pendingText != null) {
            ctx.getState().getPendingMessagingTexts().remove(toolCallId);
            if (!isToolError) {
                ctx.getState().getMessagingToolSentTexts().add(pendingText);
                ctx.getState().getMessagingToolSentTextsNormalized().add(
                        SubscribeToolUtils.normalizeTextForComparison(pendingText));
                log.debug("Committed messaging text: tool={} len={}", toolName, pendingText.length());
                ctx.getTrimMessagingToolSent().run();
            }
        }
        if (pendingTarget != null) {
            ctx.getState().getPendingMessagingTargets().remove(toolCallId);
            if (!isToolError) {
                ctx.getState().getMessagingToolSentTargets().add(pendingTarget);
                ctx.getTrimMessagingToolSent().run();
            }
        }

        emitAgentEvent(ctx, "tool", Map.of(
                "phase", "result", "name", toolName, "toolCallId", toolCallId,
                "isError", isToolError));
        log.debug("embedded run tool end: runId={} tool={} toolCallId={}",
                ctx.getParams().runId, toolName, toolCallId);

        // Emit tool output
        if (ctx.getParams().onToolResult != null && ctx.getShouldEmitToolOutput().test("")) {
            String outputText = SubscribeToolUtils.extractToolResultText(sanitizedResult);
            if (outputText != null) {
                ctx.getEmitToolOutput().emit(toolName, meta, outputText);
            }
        }
    }

    // ── Helpers ──────────────────────────────────────────────────────

    private static String normalizeToolName(String name) {
        if (name == null)
            return "";
        return name.trim().toLowerCase();
    }

    private static String extendExecMeta(String toolName, Object args, String meta) {
        String normalized = toolName.trim().toLowerCase();
        if (!"exec".equals(normalized) && !"bash".equals(normalized))
            return meta;
        if (!(args instanceof Map<?, ?> record))
            return meta;
        java.util.List<String> flags = new java.util.ArrayList<>();
        if (Boolean.TRUE.equals(record.get("pty")))
            flags.add("pty");
        if (Boolean.TRUE.equals(record.get("elevated")))
            flags.add("elevated");
        if (flags.isEmpty())
            return meta;
        String suffix = String.join(" · ", flags);
        return meta != null ? meta + " · " + suffix : suffix;
    }

    private static void emitAgentEvent(SubscribeContext ctx, String stream, Map<String, Object> data) {
        if (ctx.getParams().onAgentEvent != null) {
            try {
                ctx.getParams().onAgentEvent.accept(
                        new SubscribeTypes.AgentEventPayload(stream, data));
            } catch (Exception ignored) {
            }
        }
    }
}
