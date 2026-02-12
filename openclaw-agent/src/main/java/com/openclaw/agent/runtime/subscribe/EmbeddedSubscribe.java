package com.openclaw.agent.runtime.subscribe;

import com.openclaw.agent.runtime.subscribe.HandlerTypes.*;
import com.openclaw.agent.runtime.subscribe.SubscribeTypes.*;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;

/**
 * Main subscribe session entry point — creates and manages the full subscribe
 * context.
 * Corresponds to TypeScript pi-embedded-subscribe.ts.
 */
@Slf4j
public final class EmbeddedSubscribe {

    private EmbeddedSubscribe() {
    }

    /** The result of subscribing to a session. */
    public record SubscribeResult(
            List<String> assistantTexts,
            List<ToolMeta> toolMetas,
            Runnable unsubscribe,
            java.util.function.Supplier<Boolean> isCompacting,
            java.util.function.Supplier<List<String>> getMessagingToolSentTexts,
            java.util.function.Supplier<List<MessagingToolSend>> getMessagingToolSentTargets,
            java.util.function.Supplier<Boolean> didSendViaMessagingTool,
            java.util.function.Supplier<ToolErrorSummary> getLastToolError,
            java.util.function.Supplier<CompletableFuture<Void>> waitForCompactionRetry) {
    }

    /**
     * Subscribe to an embedded PI session, creating all state and context.
     *
     * @param params            subscription parameters
     * @param sessionSubscriber function that takes our event handler and returns an
     *                          unsubscribe handle
     * @return SubscribeResult with accessors for texts, state, and lifecycle
     */
    public static SubscribeResult subscribeEmbeddedPiSession(
            SubscribeSessionParams params,
            java.util.function.Function<Consumer<Map<String, Object>>, Runnable> sessionSubscriber) {

        String reasoningMode = params.reasoningMode != null ? params.reasoningMode : "off";
        SubscribeState state = new SubscribeState();
        state.setBlockReplyBreak(params.blockReplyBreak != null ? params.blockReplyBreak : BlockReplyBreak.text_end);
        state.setReasoningMode(reasoningMode);
        state.setIncludeReasoning("on".equals(reasoningMode));
        state.setShouldEmitPartialReplies(
                !("on".equals(reasoningMode) && params.onBlockReply == null));
        state.setStreamReasoning(
                "stream".equals(reasoningMode) && params.onReasoningStream != null);

        BlockReplyChunking blockChunking = params.blockReplyChunking;
        BlockChunker blockChunker = blockChunking != null ? new BlockChunker(blockChunking) : null;

        SubscribeContext ctx = new SubscribeContext(params, state, blockChunking);
        ctx.setBlockChunker(blockChunker);

        // ── Operations ──────────────────────────────────────────────

        // Reset assistant message state
        ctx.setResetAssistantMessageState(nextBaseline -> {
            state.setDeltaBuffer("");
            state.setBlockBuffer("");
            if (blockChunker != null)
                blockChunker.reset();
            state.getBlockState().setThinking(false);
            state.getBlockState().setFinal(false);
            state.getBlockState().setInlineCode(new InlineCodeState());
            state.getPartialBlockState().setThinking(false);
            state.getPartialBlockState().setFinal(false);
            state.getPartialBlockState().setInlineCode(new InlineCodeState());
            state.setLastStreamedAssistant(null);
            state.setLastStreamedAssistantCleaned(null);
            state.setEmittedAssistantUpdate(false);
            state.setLastBlockReplyText(null);
            state.setLastStreamedReasoning(null);
            state.setLastReasoningSent(null);
            state.setSuppressBlockChunks(false);
            state.setAssistantMessageIndex(state.getAssistantMessageIndex() + 1);
            state.setLastAssistantTextMessageIndex(-1);
            state.setLastAssistantTextNormalized(null);
            state.setLastAssistantTextTrimmed(null);
            state.setAssistantTextBaseline(nextBaseline);
        });

        // Finalize assistant texts
        ctx.setFinalizeAssistantTexts(args -> {
            String text = args.text();
            boolean addedDuringMessage = args.addedDuringMessage();
            boolean chunkerHasBuffered = args.chunkerHasBuffered();
            List<String> texts = state.getAssistantTexts();
            if (state.isIncludeReasoning() && text != null && !text.isEmpty() && params.onBlockReply == null) {
                if (texts.size() > state.getAssistantTextBaseline()) {
                    int from = state.getAssistantTextBaseline();
                    texts.subList(from, texts.size()).clear();
                    texts.add(text);
                } else {
                    pushAssistantText(state, text);
                }
                state.setSuppressBlockChunks(true);
            } else if (!addedDuringMessage && !chunkerHasBuffered && text != null && !text.isEmpty()) {
                pushAssistantText(state, text);
            }
            state.setAssistantTextBaseline(texts.size());
        });

        // Emit block chunk
        ctx.setEmitBlockChunk(text -> {
            if (state.isSuppressBlockChunks())
                return;
            HandlerTypes.BlockTagState tagState = state.getBlockState();
            String chunk = ctx.getStripBlockTags().strip(text, tagState);
            if (chunk != null)
                chunk = chunk.stripTrailing();
            if (chunk == null || chunk.isEmpty())
                return;
            if (chunk.equals(state.getLastBlockReplyText()))
                return;
            String norm = SubscribeToolUtils.normalizeTextForComparison(chunk);
            if (SubscribeToolUtils.isMessagingToolDuplicateNormalized(
                    norm, state.getMessagingToolSentTextsNormalized())) {
                log.debug("Skipping block reply - already sent via messaging tool: {}...",
                        chunk.substring(0, Math.min(50, chunk.length())));
                return;
            }
            state.setLastBlockReplyText(chunk);
            state.getAssistantTexts().add(chunk);
            if (params.onBlockReply != null) {
                params.onBlockReply.accept(new ReplyPayload(chunk));
            }
        });

        // Strip block tags
        ctx.setStripBlockTags((text, tagState) -> {
            if (text == null || text.isEmpty())
                return text;
            return EmbeddedUtils.stripThinkingTagsFromText(text);
        });

        // Flush block reply buffer
        ctx.setFlushBlockReplyBuffer(() -> {
            if (params.onBlockReply == null)
                return;
            if (blockChunker != null && blockChunker.hasBuffered()) {
                blockChunker.drain(true, ctx.getEmitBlockChunk());
                blockChunker.reset();
            } else if (!state.getBlockBuffer().isEmpty()) {
                ctx.getEmitBlockChunk().accept(state.getBlockBuffer());
                state.setBlockBuffer("");
            }
        });

        // Emit reasoning stream
        ctx.setEmitReasoningStream(text -> {
            if (!state.isStreamReasoning() || params.onReasoningStream == null)
                return;
            String formatted = EmbeddedUtils.formatReasoningMessage(text);
            if (formatted == null || formatted.isEmpty())
                return;
            if (formatted.equals(state.getLastStreamedReasoning()))
                return;
            state.setLastStreamedReasoning(formatted);
            params.onReasoningStream.accept(new ReplyPayload(formatted));
        });

        // Emit tool summary and output
        ctx.setEmitToolSummary((toolName, meta) -> {
            if (params.onToolResult == null)
                return;
            String agg = (toolName != null ? toolName : "") + (meta != null ? " (" + meta + ")" : "");
            if (!agg.isEmpty()) {
                try {
                    params.onToolResult.accept(new ReplyPayload(agg));
                } catch (Exception ignored) {
                }
            }
        });
        ctx.setEmitToolOutput((toolName, meta, output) -> {
            if (params.onToolResult == null || output == null)
                return;
            String agg = (toolName != null ? toolName : "") + (meta != null ? " (" + meta + ")" : "");
            String message = agg + "\n```txt\n" + output.trim() + "\n```";
            try {
                params.onToolResult.accept(new ReplyPayload(message));
            } catch (Exception ignored) {
            }
        });

        // Should emit checks
        ctx.setShouldEmitToolResult(ignored -> params.shouldEmitToolResult != null
                ? params.shouldEmitToolResult.get()
                : "on".equals(params.verboseLevel) || "full".equals(params.verboseLevel));
        ctx.setShouldEmitToolOutput(ignored -> params.shouldEmitToolOutput != null
                ? params.shouldEmitToolOutput.get()
                : "full".equals(params.verboseLevel));

        // Messaging tool trim
        int maxSentTexts = 200;
        int maxSentTargets = 200;
        ctx.setTrimMessagingToolSent(() -> {
            if (state.getMessagingToolSentTexts().size() > maxSentTexts) {
                int overflow = state.getMessagingToolSentTexts().size() - maxSentTexts;
                state.getMessagingToolSentTexts().subList(0, overflow).clear();
                state.getMessagingToolSentTextsNormalized().subList(0, overflow).clear();
            }
            if (state.getMessagingToolSentTargets().size() > maxSentTargets) {
                int overflow = state.getMessagingToolSentTargets().size() - maxSentTargets;
                state.getMessagingToolSentTargets().subList(0, overflow).clear();
            }
        });

        // Compaction promise management
        ctx.setEnsureCompactionPromise(() -> {
            if (state.getCompactionRetryPromise() == null) {
                state.setCompactionRetryPromise(new CompletableFuture<>());
            }
        });
        ctx.setNoteCompactionRetry(() -> {
            state.setPendingCompactionRetry(state.getPendingCompactionRetry() + 1);
            ctx.getEnsureCompactionPromise().run();
        });
        ctx.setResolveCompactionRetry(() -> {
            if (state.getPendingCompactionRetry() <= 0)
                return;
            state.setPendingCompactionRetry(state.getPendingCompactionRetry() - 1);
            if (state.getPendingCompactionRetry() == 0 && !state.isCompactionInFlight()) {
                CompletableFuture<Void> p = state.getCompactionRetryPromise();
                if (p != null)
                    p.complete(null);
                state.setCompactionRetryPromise(null);
            }
        });
        ctx.setMaybeResolveCompactionWait(() -> {
            if (state.getPendingCompactionRetry() == 0 && !state.isCompactionInFlight()) {
                CompletableFuture<Void> p = state.getCompactionRetryPromise();
                if (p != null)
                    p.complete(null);
                state.setCompactionRetryPromise(null);
            }
        });

        // Reset for compaction retry
        ctx.setResetForCompactionRetry(() -> {
            state.getAssistantTexts().clear();
            state.getToolMetas().clear();
            state.getToolMetaById().clear();
            state.getToolSummaryById().clear();
            state.setLastToolError(null);
            state.getMessagingToolSentTexts().clear();
            state.getMessagingToolSentTextsNormalized().clear();
            state.getMessagingToolSentTargets().clear();
            state.getPendingMessagingTexts().clear();
            state.getPendingMessagingTargets().clear();
            ctx.getResetAssistantMessageState().accept(0);
        });

        // Reply directives (simplified — pass through)
        ctx.setConsumeReplyDirectives((text, isFinal) -> text != null ? new ReplyPayload(text) : null);
        ctx.setConsumePartialReplyDirectives((text, isFinal) -> text != null ? new ReplyPayload(text) : null);

        // Subscribe to session events
        Consumer<Map<String, Object>> handler = SubscribeHandlers.createEventHandler(ctx);
        Runnable unsubscribe = sessionSubscriber.apply(handler);

        return new SubscribeResult(
                state.getAssistantTexts(),
                state.getToolMetas(),
                unsubscribe,
                () -> state.isCompactionInFlight() || state.getPendingCompactionRetry() > 0,
                () -> new ArrayList<>(state.getMessagingToolSentTexts()),
                () -> new ArrayList<>(state.getMessagingToolSentTargets()),
                () -> !state.getMessagingToolSentTexts().isEmpty(),
                () -> state.getLastToolError() != null
                        ? new ToolErrorSummary(
                                state.getLastToolError().toolName(),
                                state.getLastToolError().meta(),
                                state.getLastToolError().error())
                        : null,
                () -> {
                    if (state.isCompactionInFlight() || state.getPendingCompactionRetry() > 0) {
                        ctx.getEnsureCompactionPromise().run();
                        return state.getCompactionRetryPromise() != null
                                ? state.getCompactionRetryPromise()
                                : CompletableFuture.completedFuture(null);
                    }
                    return CompletableFuture.completedFuture(null);
                });
    }

    // ── Helpers ──────────────────────────────────────────────────────

    private static void pushAssistantText(SubscribeState state, String text) {
        if (text == null || text.isEmpty())
            return;
        // Simple dedup
        String trimmed = text.stripTrailing();
        if (trimmed.equals(state.getLastAssistantTextTrimmed())
                && state.getLastAssistantTextMessageIndex() == state.getAssistantMessageIndex()) {
            return;
        }
        state.getAssistantTexts().add(text);
        state.setLastAssistantTextMessageIndex(state.getAssistantMessageIndex());
        state.setLastAssistantTextTrimmed(trimmed);
        String norm = SubscribeToolUtils.normalizeTextForComparison(text);
        state.setLastAssistantTextNormalized(norm.isEmpty() ? null : norm);
    }
}
