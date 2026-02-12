package com.openclaw.agent.runtime.subscribe;

import com.openclaw.agent.runtime.subscribe.HandlerTypes.SubscribeContext;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * Lifecycle event handlers: agent start/end, compaction start/end.
 * Corresponds to TypeScript pi-embedded-subscribe.handlers.lifecycle.ts.
 */
@Slf4j
public final class LifecycleHandlers {

    private LifecycleHandlers() {
    }

    public static void handleAgentStart(SubscribeContext ctx) {
        log.debug("embedded run agent start: runId={}", ctx.getParams().runId);
        emitAgentEvent(ctx, "lifecycle", Map.of("phase", "start", "startedAt", System.currentTimeMillis()));
    }

    public static void handleAutoCompactionStart(SubscribeContext ctx) {
        ctx.getState().setCompactionInFlight(true);
        ctx.getEnsureCompactionPromise().run();
        log.debug("embedded run compaction start: runId={}", ctx.getParams().runId);
        emitAgentEvent(ctx, "compaction", Map.of("phase", "start"));
    }

    public static void handleAutoCompactionEnd(SubscribeContext ctx, boolean willRetry) {
        ctx.getState().setCompactionInFlight(false);
        if (willRetry) {
            ctx.getNoteCompactionRetry().run();
            ctx.getResetForCompactionRetry().run();
            log.debug("embedded run compaction retry: runId={}", ctx.getParams().runId);
        } else {
            ctx.getMaybeResolveCompactionWait().run();
        }
        emitAgentEvent(ctx, "compaction", Map.of("phase", "end", "willRetry", willRetry));
    }

    public static void handleAgentEnd(SubscribeContext ctx) {
        log.debug("embedded run agent end: runId={}", ctx.getParams().runId);
        emitAgentEvent(ctx, "lifecycle", Map.of("phase", "end", "endedAt", System.currentTimeMillis()));

        // Flush remaining block reply buffer
        if (ctx.getParams().onBlockReply != null) {
            if (ctx.getBlockChunker() != null && ctx.getBlockChunker().hasBuffered()) {
                ctx.getBlockChunker().drain(true, ctx.getEmitBlockChunk());
                ctx.getBlockChunker().reset();
            } else if (!ctx.getState().getBlockBuffer().isEmpty()) {
                ctx.getEmitBlockChunk().accept(ctx.getState().getBlockBuffer());
                ctx.getState().setBlockBuffer("");
            }
        }

        // Reset block state
        ctx.getState().getBlockState().setThinking(false);
        ctx.getState().getBlockState().setFinal(false);
        ctx.getState().getBlockState().setInlineCode(new HandlerTypes.InlineCodeState());

        // Resolve compaction waiters
        if (ctx.getState().getPendingCompactionRetry() > 0) {
            ctx.getResolveCompactionRetry().run();
        } else {
            ctx.getMaybeResolveCompactionWait().run();
        }
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
