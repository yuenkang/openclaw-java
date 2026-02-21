package com.openclaw.autoreply.reply;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Streaming directive accumulator — buffers streamed LLM chunks and
 * extracts inline reply directives (reply tags, media, silent tokens).
 * Mirrors {@code auto-reply/reply/streaming-directives.ts}.
 */
public final class StreamingDirectives {

    private StreamingDirectives() {
    }

    /** Parsed reply directive result from a streaming chunk. */
    public record ReplyDirectiveParseResult(
            String text,
            String mediaUrl,
            String[] mediaUrls,
            String replyToId,
            boolean replyToCurrent,
            boolean replyToTag,
            boolean audioAsVoice,
            boolean isSilent) {
    }

    private static final String SILENT_REPLY_TOKEN = "[[SILENT]]";

    private record PendingReplyState(String explicitId, boolean sawCurrent, boolean hasTag) {
    }

    private record SplitResult(String text, String tail) {
    }

    /**
     * Split trailing incomplete directive (starts with [[ but no closing ]]).
     */
    private static SplitResult splitTrailingDirective(String text) {
        int openIdx = text.lastIndexOf("[[");
        if (openIdx < 0)
            return new SplitResult(text, "");

        int closeIdx = text.indexOf("]]", openIdx + 2);
        if (closeIdx >= 0)
            return new SplitResult(text, "");

        return new SplitResult(
                text.substring(0, openIdx),
                text.substring(openIdx));
    }

    /**
     * Check whether text contains the silent reply token.
     */
    private static boolean isSilentReplyText(String text, String token) {
        return text != null && text.trim().equals(token);
    }

    /**
     * Check if a parsed result has renderable content (text, media, or audio).
     */
    private static boolean hasRenderableContent(ReplyDirectiveParseResult parsed) {
        return (parsed.text() != null && !parsed.text().isEmpty())
                || (parsed.mediaUrl() != null && !parsed.mediaUrl().isEmpty())
                || (parsed.mediaUrls() != null && parsed.mediaUrls().length > 0)
                || parsed.audioAsVoice();
    }

    /**
     * Create a streaming directive accumulator that buffers chunks and extracts
     * inline reply directives.
     */
    public static StreamingAccumulator createStreamingDirectiveAccumulator() {
        return new StreamingAccumulator();
    }

    /**
     * Stateful accumulator for streaming chunks.
     */
    public static class StreamingAccumulator {
        private String pendingTail = "";
        private PendingReplyState pendingReply = new PendingReplyState(null, false, false);

        /**
         * Reset all pending state.
         */
        public void reset() {
            pendingTail = "";
            pendingReply = new PendingReplyState(null, false, false);
        }

        /**
         * Consume a raw streaming chunk.
         *
         * @param raw         the raw chunk text
         * @param isFinal     whether this is the final chunk
         * @param silentToken custom silent token (nullable, defaults to [[SILENT]])
         * @return parsed result or null if no renderable content yet
         */
        public ReplyDirectiveParseResult consume(String raw, boolean isFinal, String silentToken) {
            String combined = pendingTail + (raw != null ? raw : "");
            pendingTail = "";

            if (!isFinal) {
                SplitResult split = splitTrailingDirective(combined);
                combined = split.text();
                pendingTail = split.tail();
            }

            if (combined.isEmpty())
                return null;

            // Parse the chunk (simplified — full media/reply-tag parsing deferred)
            String text = combined;
            String token = silentToken != null ? silentToken : SILENT_REPLY_TOKEN;
            boolean isSilent = isSilentReplyText(text, token);
            if (isSilent)
                text = "";

            // Reply tag detection (simplified)
            boolean hasReplyTag = false;
            String replyToId = null;
            boolean replyToCurrent = false;
            Pattern replyPattern = Pattern.compile("\\[\\[reply:(\\w+)]]");
            Matcher m = replyPattern.matcher(text);
            if (m.find()) {
                hasReplyTag = true;
                replyToId = m.group(1);
                replyToCurrent = "current".equals(replyToId);
                text = m.replaceAll("").trim();
            }

            boolean mergedHasTag = pendingReply.hasTag() || hasReplyTag;
            boolean mergedSawCurrent = pendingReply.sawCurrent() || replyToCurrent;
            String mergedExplicitId = replyToId != null ? replyToId : pendingReply.explicitId();

            ReplyDirectiveParseResult result = new ReplyDirectiveParseResult(
                    text, null, null,
                    mergedExplicitId, mergedSawCurrent, mergedHasTag,
                    false, isSilent);

            if (!hasRenderableContent(result)) {
                if (mergedHasTag) {
                    pendingReply = new PendingReplyState(mergedExplicitId, mergedSawCurrent, true);
                }
                return null;
            }

            pendingReply = new PendingReplyState(null, false, false);
            return result;
        }
    }
}
