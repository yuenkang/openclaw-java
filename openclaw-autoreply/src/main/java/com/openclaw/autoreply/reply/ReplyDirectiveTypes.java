package com.openclaw.autoreply.reply;

import java.util.List;

/**
 * Result types for reply directive parsing.
 * Mirrors types from {@code auto-reply/reply/reply-directives.ts}.
 */
public final class ReplyDirectiveTypes {

    private ReplyDirectiveTypes() {
    }

    /** Parsed result of reply directives from model output. */
    public record ReplyDirectiveParseResult(
            String text,
            List<String> mediaUrls,
            String mediaUrl,
            String replyToId,
            boolean replyToCurrent,
            boolean replyToTag,
            Boolean audioAsVoice,
            boolean isSilent) {
    }
}
