package com.openclaw.agent.autoreply.reply;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Extract [[reply_to:id]] / [[reply_to:current]] tags from model output.
 * Mirrors {@code auto-reply/reply/reply-tags.ts} with inlined directive-tags
 * parsing.
 */
public final class ReplyTags {

    private ReplyTags() {
    }

    /** Pattern matching [[reply_to:...]] directives. */
    private static final Pattern REPLY_TO_RE = Pattern.compile("\\[\\[reply_to\\s*:\\s*([^\\]]+?)\\]\\]",
            Pattern.CASE_INSENSITIVE);

    /** Result of extracting a reply-to tag. */
    public record ExtractResult(String cleaned, String replyToId, boolean replyToCurrent, boolean hasTag) {
    }

    /**
     * Extract reply-to tags from text.
     *
     * @param text             model output text
     * @param currentMessageId the id of the current inbound message
     * @return extraction result with cleaned text and resolved reply id
     */
    public static ExtractResult extractReplyToTag(String text, String currentMessageId) {
        if (text == null || text.isEmpty()) {
            return new ExtractResult("", null, false, false);
        }
        Matcher m = REPLY_TO_RE.matcher(text);
        if (!m.find()) {
            return new ExtractResult(text, null, false, false);
        }

        String rawId = m.group(1).trim();
        boolean isCurrent = "current".equalsIgnoreCase(rawId);
        String replyToId;
        if (isCurrent) {
            replyToId = currentMessageId != null ? currentMessageId.trim() : null;
            if (replyToId != null && replyToId.isEmpty())
                replyToId = null;
        } else {
            replyToId = rawId.isEmpty() ? null : rawId;
        }

        // Remove all occurrences of the reply_to tag
        String cleaned = REPLY_TO_RE.matcher(text).replaceAll("").replaceAll("\\s+", " ").trim();
        return new ExtractResult(cleaned, replyToId, isCurrent, true);
    }
}
