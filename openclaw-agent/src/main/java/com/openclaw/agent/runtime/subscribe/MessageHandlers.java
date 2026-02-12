package com.openclaw.agent.runtime.subscribe;

import com.openclaw.agent.runtime.subscribe.HandlerTypes.SubscribeContext;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Message event handlers: message_start, message_update, message_end.
 * Corresponds to TypeScript pi-embedded-subscribe.handlers.messages.ts.
 */
@Slf4j
public final class MessageHandlers {

    private MessageHandlers() {
    }

    /**
     * Handle assistant message start — reset state for new message.
     */
    @SuppressWarnings("unchecked")
    public static void handleMessageStart(SubscribeContext ctx, Map<String, Object> evt) {
        Map<String, Object> msg = (Map<String, Object>) evt.get("message");
        if (msg == null || !"assistant".equals(msg.get("role")))
            return;
        ctx.getResetAssistantMessageState().accept(ctx.getState().getAssistantTexts().size());
        if (ctx.getParams().onAssistantMessageStart != null) {
            ctx.getParams().onAssistantMessageStart.run();
        }
    }

    /**
     * Handle assistant message update — process text deltas and emit streaming
     * events.
     */
    @SuppressWarnings("unchecked")
    public static void handleMessageUpdate(SubscribeContext ctx, Map<String, Object> evt) {
        Map<String, Object> msg = (Map<String, Object>) evt.get("message");
        if (msg == null || !"assistant".equals(msg.get("role")))
            return;

        Object assistantEvent = evt.get("assistantMessageEvent");
        if (!(assistantEvent instanceof Map))
            return;
        Map<String, Object> record = (Map<String, Object>) assistantEvent;
        String evtType = record.get("type") instanceof String s ? s : "";
        if (!"text_delta".equals(evtType) && !"text_start".equals(evtType) && !"text_end".equals(evtType))
            return;

        String delta = record.get("delta") instanceof String s ? s : "";
        String content = record.get("content") instanceof String s ? s : "";

        // Append to raw stream
        RawStreamHandler.appendRawStream(Map.of(
                "ts", System.currentTimeMillis(),
                "event", "assistant_text_stream",
                "runId", String.valueOf(ctx.getParams().runId),
                "evtType", evtType,
                "delta", delta,
                "content", content));

        // Determine the new chunk
        String chunk = "";
        if ("text_delta".equals(evtType)) {
            chunk = delta;
        } else {
            if (!delta.isEmpty()) {
                chunk = delta;
            } else if (!content.isEmpty()) {
                String buf = ctx.getState().getDeltaBuffer();
                if (content.startsWith(buf)) {
                    chunk = content.substring(buf.length());
                } else if (buf.startsWith(content)) {
                    chunk = "";
                } else if (!buf.contains(content)) {
                    chunk = content;
                }
            }
        }

        if (!chunk.isEmpty()) {
            ctx.getState().setDeltaBuffer(ctx.getState().getDeltaBuffer() + chunk);
            if (ctx.getBlockChunker() != null) {
                ctx.getBlockChunker().append(chunk);
            } else {
                ctx.getState().setBlockBuffer(ctx.getState().getBlockBuffer() + chunk);
            }
        }

        // Stream reasoning if enabled
        if (ctx.getState().isStreamReasoning()) {
            ctx.getEmitReasoningStream().accept(
                    EmbeddedUtils.extractThinkingFromTaggedStream(ctx.getState().getDeltaBuffer()));
        }

        // Strip block tags and emit partial replies
        HandlerTypes.BlockTagState cleanState = new HandlerTypes.BlockTagState();
        String next = ctx.getStripBlockTags().strip(ctx.getState().getDeltaBuffer(), cleanState);
        if (next != null)
            next = next.trim();
        if (next != null && !next.isEmpty()) {
            String previousCleaned = ctx.getState().getLastStreamedAssistantCleaned();
            if (previousCleaned == null)
                previousCleaned = "";
            boolean shouldEmit;
            String deltaText = "";
            if (previousCleaned.isEmpty() || next.startsWith(previousCleaned)) {
                deltaText = next.substring(previousCleaned.length());
                shouldEmit = !deltaText.isEmpty();
            } else {
                shouldEmit = false;
            }
            ctx.getState().setLastStreamedAssistant(next);
            ctx.getState().setLastStreamedAssistantCleaned(next); // simplified

            if (shouldEmit) {
                emitAgentEvent(ctx, "assistant", Map.of("text", next, "delta", deltaText));
                ctx.getState().setEmittedAssistantUpdate(true);
                if (ctx.getParams().onPartialReply != null && ctx.getState().isShouldEmitPartialReplies()) {
                    try {
                        ctx.getParams().onPartialReply.accept(
                                new SubscribeTypes.ReplyPayload(next));
                    } catch (Exception ignored) {
                    }
                }
            }
        }

        // Block chunker drain
        if (ctx.getParams().onBlockReply != null && ctx.getBlockChunking() != null
                && ctx.getState().getBlockReplyBreak() == SubscribeTypes.BlockReplyBreak.text_end) {
            if (ctx.getBlockChunker() != null) {
                ctx.getBlockChunker().drain(false, ctx.getEmitBlockChunk());
            }
        }

        // text_end flush
        if ("text_end".equals(evtType)
                && ctx.getState().getBlockReplyBreak() == SubscribeTypes.BlockReplyBreak.text_end) {
            if (ctx.getBlockChunker() != null && ctx.getBlockChunker().hasBuffered()) {
                ctx.getBlockChunker().drain(true, ctx.getEmitBlockChunk());
                ctx.getBlockChunker().reset();
            } else if (!ctx.getState().getBlockBuffer().isEmpty()) {
                ctx.getEmitBlockChunk().accept(ctx.getState().getBlockBuffer());
                ctx.getState().setBlockBuffer("");
            }
        }
    }

    /**
     * Handle assistant message end — finalize text, emit block replies, handle
     * reasoning.
     */
    @SuppressWarnings("unchecked")
    public static void handleMessageEnd(SubscribeContext ctx, Map<String, Object> evt) {
        Map<String, Object> msg = (Map<String, Object>) evt.get("message");
        if (msg == null || !"assistant".equals(msg.get("role")))
            return;

        List<Map<String, Object>> content = msg.get("content") instanceof List<?> l
                ? (List<Map<String, Object>>) l
                : null;

        String rawText = EmbeddedUtils.extractAssistantText(content);
        RawStreamHandler.appendRawStream(Map.of(
                "ts", System.currentTimeMillis(),
                "event", "assistant_message_end",
                "runId", String.valueOf(ctx.getParams().runId),
                "rawText", rawText != null ? rawText : ""));

        HandlerTypes.BlockTagState cleanState = new HandlerTypes.BlockTagState();
        String text = ctx.getStripBlockTags().strip(rawText != null ? rawText : "", cleanState);
        String rawThinking = (ctx.getState().isIncludeReasoning() || ctx.getState().isStreamReasoning())
                ? firstNonEmpty(EmbeddedUtils.extractAssistantThinking(content),
                        EmbeddedUtils.extractThinkingFromTaggedText(rawText != null ? rawText : ""))
                : "";
        String formattedReasoning = rawThinking != null && !rawThinking.isEmpty()
                ? EmbeddedUtils.formatReasoningMessage(rawThinking)
                : "";
        String trimmedText = text != null ? text.trim() : "";

        // Emit if no streaming update was sent
        if (!ctx.getState().isEmittedAssistantUpdate() && !trimmedText.isEmpty()) {
            emitAgentEvent(ctx, "assistant", Map.of("text", trimmedText, "delta", trimmedText));
            ctx.getState().setEmittedAssistantUpdate(true);
        }

        // Finalize assistant texts
        boolean addedDuringMessage = ctx.getState().getAssistantTexts().size() > ctx.getState()
                .getAssistantTextBaseline();
        boolean chunkerHasBuffered = ctx.getBlockChunker() != null && ctx.getBlockChunker().hasBuffered();
        ctx.getFinalizeAssistantTexts().accept(
                new HandlerTypes.FinalizeArgs(text, addedDuringMessage, chunkerHasBuffered));

        // Emit reasoning and block replies
        boolean shouldEmitReasoning = ctx.getState().isIncludeReasoning()
                && !formattedReasoning.isEmpty()
                && ctx.getParams().onBlockReply != null
                && !formattedReasoning.equals(ctx.getState().getLastReasoningSent());

        if (shouldEmitReasoning
                && ctx.getState().getBlockReplyBreak() == SubscribeTypes.BlockReplyBreak.message_end
                && !addedDuringMessage) {
            ctx.getState().setLastReasoningSent(formattedReasoning);
            ctx.getParams().onBlockReply.accept(new SubscribeTypes.ReplyPayload(formattedReasoning));
        }

        // Flush block reply buffer on message_end
        if (text != null && !text.isEmpty() && ctx.getParams().onBlockReply != null
                && (ctx.getState().getBlockReplyBreak() == SubscribeTypes.BlockReplyBreak.message_end
                        || (ctx.getBlockChunker() != null ? ctx.getBlockChunker().hasBuffered()
                                : !ctx.getState().getBlockBuffer().isEmpty()))) {
            if (ctx.getBlockChunker() != null && ctx.getBlockChunker().hasBuffered()) {
                ctx.getBlockChunker().drain(true, ctx.getEmitBlockChunk());
                ctx.getBlockChunker().reset();
            } else if (!text.equals(ctx.getState().getLastBlockReplyText())) {
                String norm = SubscribeToolUtils.normalizeTextForComparison(text);
                if (!SubscribeToolUtils.isMessagingToolDuplicateNormalized(
                        norm, ctx.getState().getMessagingToolSentTextsNormalized())) {
                    ctx.getState().setLastBlockReplyText(text);
                    ctx.getParams().onBlockReply.accept(new SubscribeTypes.ReplyPayload(text));
                }
            }
        }

        // Post-answer reasoning
        if (shouldEmitReasoning
                && !(ctx.getState().getBlockReplyBreak() == SubscribeTypes.BlockReplyBreak.message_end
                        && !addedDuringMessage)) {
            ctx.getState().setLastReasoningSent(formattedReasoning);
            ctx.getParams().onBlockReply.accept(new SubscribeTypes.ReplyPayload(formattedReasoning));
        }

        if (ctx.getState().isStreamReasoning() && rawThinking != null && !rawThinking.isEmpty()) {
            ctx.getEmitReasoningStream().accept(rawThinking);
        }

        // Reset message state
        ctx.getState().setDeltaBuffer("");
        ctx.getState().setBlockBuffer("");
        if (ctx.getBlockChunker() != null)
            ctx.getBlockChunker().reset();
        ctx.getState().getBlockState().setThinking(false);
        ctx.getState().getBlockState().setFinal(false);
        ctx.getState().getBlockState().setInlineCode(new HandlerTypes.InlineCodeState());
        ctx.getState().setLastStreamedAssistant(null);
        ctx.getState().setLastStreamedAssistantCleaned(null);
    }

    private static String firstNonEmpty(String... values) {
        for (String v : values)
            if (v != null && !v.isEmpty())
                return v;
        return "";
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
