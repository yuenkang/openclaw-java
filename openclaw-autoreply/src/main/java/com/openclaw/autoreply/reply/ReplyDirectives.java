package com.openclaw.autoreply.reply;

import java.util.List;

/**
 * Parse raw model output into reply directives — text, media URLs,
 * reply-to IDs, silent detection.
 * Mirrors {@code auto-reply/reply/reply-directives.ts} (the
 * parseReplyDirectives function).
 */
public final class ReplyDirectives {

    private ReplyDirectives() {
    }

    /**
     * Parse raw model output text into structured reply directives.
     *
     * @param raw              raw model output
     * @param currentMessageId optional current message ID for reply resolution
     * @param silentToken      optional silent token (defaults to standard token)
     * @return parsed result
     */
    public static ReplyDirectiveTypes.ReplyDirectiveParseResult parseReplyDirectives(
            String raw, String currentMessageId, String silentToken) {

        if (raw == null || raw.isEmpty()) {
            return new ReplyDirectiveTypes.ReplyDirectiveParseResult(
                    "", null, null, null, false, false, false, false);
        }

        // Split media from output (simplified — extract media URLs)
        String text = raw;
        List<String> mediaUrls = null;
        String mediaUrl = null;
        boolean audioAsVoice = false;

        // Extract [[media:url]] patterns
        java.util.regex.Matcher mediaMatcher = java.util.regex.Pattern
                .compile("\\[\\[media:(https?://[^\\]]+)\\]\\]")
                .matcher(text);
        java.util.List<String> extractedUrls = new java.util.ArrayList<>();
        while (mediaMatcher.find()) {
            extractedUrls.add(mediaMatcher.group(1));
        }
        if (!extractedUrls.isEmpty()) {
            text = mediaMatcher.replaceAll("").trim();
            mediaUrls = extractedUrls;
            mediaUrl = extractedUrls.get(0);
        }

        // Parse reply tags
        ReplyTags.ExtractResult replyParsed = ReplyTags.extractReplyToTag(text, currentMessageId);
        if (replyParsed.hasTag()) {
            text = replyParsed.cleaned();
        }

        // Silent detection
        String token = silentToken != null ? silentToken
                : com.openclaw.autoreply.ReplyTokens.SILENT_REPLY_TOKEN;
        boolean isSilent = com.openclaw.autoreply.ReplyTokens.isSilentReplyText(text, token);
        if (isSilent) {
            text = "";
        }

        return new ReplyDirectiveTypes.ReplyDirectiveParseResult(
                text,
                mediaUrls,
                mediaUrl,
                replyParsed.replyToId(),
                replyParsed.replyToCurrent(),
                replyParsed.hasTag(),
                audioAsVoice,
                isSilent);
    }

    /**
     * Convenience overload with default silent token and no message ID.
     */
    public static ReplyDirectiveTypes.ReplyDirectiveParseResult parseReplyDirectives(String raw) {
        return parseReplyDirectives(raw, null, null);
    }
}
