package com.openclaw.agent.runtime.subscribe;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * Subscribe session parameter types and block reply chunking config.
 * Corresponds to TypeScript pi-embedded-subscribe.types.ts.
 */
public final class SubscribeTypes {

    private SubscribeTypes() {
    }

    /** Tool result format. */
    public enum ToolResultFormat {
        markdown, plain
    }

    /** Block reply break strategy. */
    public enum BlockReplyBreak {
        text_end, message_end
    }

    /** Block reply chunking config (for chunker). */
    public record BlockReplyChunking(
            int minChars,
            int maxChars,
            BreakPreference breakPreference,
            boolean flushOnParagraph) {
        public enum BreakPreference {
            paragraph, newline, sentence
        }
    }

    /** Payload for block-reply and tool-result callbacks. */
    public record ReplyPayload(
            String text,
            List<String> mediaUrls,
            Boolean audioAsVoice,
            String replyToId,
            Boolean replyToTag,
            Boolean replyToCurrent) {
        public ReplyPayload(String text) {
            this(text, null, null, null, null, null);
        }

        public ReplyPayload(String text, List<String> mediaUrls) {
            this(text, mediaUrls, null, null, null, null);
        }
    }

    /** Agent event payload. */
    public record AgentEventPayload(String stream, java.util.Map<String, Object> data) {
    }

    /**
     * Parameters for subscribing to an embedded PI session.
     */
    public static class SubscribeSessionParams {
        public String sessionId;
        public String runId;
        public String verboseLevel; // "off" | "on" | "full"
        public String reasoningMode; // "off" | "on" | "stream"
        public ToolResultFormat toolResultFormat;
        public BlockReplyBreak blockReplyBreak;
        public BlockReplyChunking blockReplyChunking;
        public boolean enforceFinalTag;

        // Callbacks
        public Supplier<Boolean> shouldEmitToolResult;
        public Supplier<Boolean> shouldEmitToolOutput;
        public Consumer<ReplyPayload> onToolResult;
        public Consumer<ReplyPayload> onReasoningStream;
        public Consumer<ReplyPayload> onBlockReply;
        public Runnable onBlockReplyFlush;
        public Consumer<ReplyPayload> onPartialReply;
        public Runnable onAssistantMessageStart;
        public Consumer<AgentEventPayload> onAgentEvent;
    }
}
