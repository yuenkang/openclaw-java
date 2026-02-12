package com.openclaw.agent.runtime.subscribe;

import com.openclaw.agent.runtime.subscribe.SubscribeTypes.ReplyPayload;
import lombok.Data;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Subscribe handler state and context types.
 * Corresponds to TypeScript pi-embedded-subscribe.handlers.types.ts.
 */
public final class HandlerTypes {

    private HandlerTypes() {
    }

    /** Messaging tool send info. */
    public record MessagingToolSend(String tool, String provider, String accountId, String to) {
    }

    /** Tool error summary. */
    public record ToolErrorSummary(String toolName, String meta, String error) {
    }

    /** Inline code span state. */
    @Data
    public static class InlineCodeState {
        boolean inCode = false;
        int backtickCount = 0;
    }

    /** Block tag state (thinking/final). */
    @Data
    public static class BlockTagState {
        boolean thinking = false;
        boolean isFinal = false;
        InlineCodeState inlineCode = new InlineCodeState();
    }

    /**
     * Full mutable state for the subscribe session.
     */
    @Data
    public static class SubscribeState {
        // Assistant text accumulation
        final List<String> assistantTexts = new ArrayList<>();
        final List<ToolMeta> toolMetas = new ArrayList<>();
        final Map<String, String> toolMetaById = new ConcurrentHashMap<>();
        final Set<String> toolSummaryById = ConcurrentHashMap.newKeySet();
        ToolErrorSummary lastToolError;

        // Config / mode
        SubscribeTypes.BlockReplyBreak blockReplyBreak = SubscribeTypes.BlockReplyBreak.text_end;
        String reasoningMode = "off";
        boolean includeReasoning = false;
        boolean shouldEmitPartialReplies = true;
        boolean streamReasoning = false;

        // Delta / block buffers
        String deltaBuffer = "";
        String blockBuffer = "";
        final BlockTagState blockState = new BlockTagState();
        final BlockTagState partialBlockState = new BlockTagState();
        String lastStreamedAssistant;
        String lastStreamedAssistantCleaned;
        boolean emittedAssistantUpdate = false;
        String lastStreamedReasoning;
        String lastBlockReplyText;
        int assistantMessageIndex = 0;
        int lastAssistantTextMessageIndex = -1;
        String lastAssistantTextNormalized;
        String lastAssistantTextTrimmed;
        int assistantTextBaseline = 0;
        boolean suppressBlockChunks = false;
        String lastReasoningSent;

        // Compaction
        boolean compactionInFlight = false;
        int pendingCompactionRetry = 0;
        CompletableFuture<Void> compactionRetryPromise;

        // Messaging tool duplicate tracking
        final List<String> messagingToolSentTexts = new ArrayList<>();
        final List<String> messagingToolSentTextsNormalized = new ArrayList<>();
        final List<MessagingToolSend> messagingToolSentTargets = new ArrayList<>();
        final Map<String, String> pendingMessagingTexts = new ConcurrentHashMap<>();
        final Map<String, MessagingToolSend> pendingMessagingTargets = new ConcurrentHashMap<>();
    }

    /** Tool meta entry. */
    public record ToolMeta(String toolName, String meta) {
    }

    /**
     * Subscribe context — holds params, state, chunker, and all callable
     * operations.
     */
    @Data
    public static class SubscribeContext {
        final SubscribeTypes.SubscribeSessionParams params;
        final SubscribeState state;
        final SubscribeTypes.BlockReplyChunking blockChunking;
        BlockChunker blockChunker;

        // Operations (set during construction)
        Runnable resetForCompactionRetry;
        java.util.function.IntConsumer resetAssistantMessageState;
        java.util.function.Consumer<FinalizeArgs> finalizeAssistantTexts;
        Runnable flushBlockReplyBuffer;
        java.util.function.Consumer<String> emitBlockChunk;
        java.util.function.Consumer<String> emitReasoningStream;
        java.util.function.BiConsumer<String, String> emitToolSummary;
        ToolOutputEmitter emitToolOutput;
        BlockTagStripper stripBlockTags;
        Runnable trimMessagingToolSent;
        Runnable ensureCompactionPromise;
        Runnable noteCompactionRetry;
        Runnable resolveCompactionRetry;
        Runnable maybeResolveCompactionWait;
        java.util.function.BiFunction<String, Boolean, ReplyPayload> consumeReplyDirectives;
        java.util.function.BiFunction<String, Boolean, ReplyPayload> consumePartialReplyDirectives;
        java.util.function.Predicate<String> shouldEmitToolResult;
        java.util.function.Predicate<String> shouldEmitToolOutput;
    }

    /** Finalize assistant text args. */
    public record FinalizeArgs(String text, boolean addedDuringMessage, boolean chunkerHasBuffered) {
    }

    /** Block tag stripping function. */
    @FunctionalInterface
    public interface BlockTagStripper {
        String strip(String text, BlockTagState state);
    }

    /** Tool output emitter. */
    @FunctionalInterface
    public interface ToolOutputEmitter {
        void emit(String toolName, String meta, String output);
    }
}
