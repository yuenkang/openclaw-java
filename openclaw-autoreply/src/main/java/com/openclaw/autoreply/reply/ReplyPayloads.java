package com.openclaw.autoreply.reply;

import com.openclaw.autoreply.AutoReplyTypes;

import java.util.ArrayList;
import java.util.List;
import java.util.function.UnaryOperator;

/**
 * Payload utility functions — tag extraction, threading, duplicate filtering.
 * Mirrors {@code auto-reply/reply/reply-payloads.ts}.
 */
public final class ReplyPayloads {

    private ReplyPayloads() {
    }

    /**
     * Apply reply tags ([[reply:...]] etc.) to a single payload.
     */
    public static AutoReplyTypes.ReplyPayload applyReplyTagsToPayload(
            AutoReplyTypes.ReplyPayload payload, String currentMessageId) {

        if (payload.text() == null) {
            if (!payload.replyToCurrent() || (payload.replyToId() != null && !payload.replyToId().isEmpty())) {
                return payload;
            }
            String id = currentMessageId != null ? currentMessageId.trim() : null;
            return new AutoReplyTypes.ReplyPayload(
                    payload.text(), payload.mediaUrl(), payload.mediaUrls(),
                    id != null && !id.isEmpty() ? id : null,
                    payload.replyToTag(), payload.replyToCurrent(),
                    payload.audioAsVoice(), payload.isError(), payload.channelData());
        }

        boolean shouldParseTags = payload.text().contains("[[");
        if (!shouldParseTags) {
            if (!payload.replyToCurrent() || (payload.replyToId() != null && !payload.replyToId().isEmpty())) {
                return payload;
            }
            String id = currentMessageId != null ? currentMessageId.trim() : null;
            return new AutoReplyTypes.ReplyPayload(
                    payload.text(), payload.mediaUrl(), payload.mediaUrls(),
                    id != null && !id.isEmpty() ? id : null,
                    true, payload.replyToCurrent(),
                    payload.audioAsVoice(), payload.isError(), payload.channelData());
        }

        ReplyTags.ExtractResult tag = ReplyTags.extractReplyToTag(payload.text(), currentMessageId);
        String cleanedText = tag.cleaned() != null && !tag.cleaned().isEmpty() ? tag.cleaned() : null;
        String replyToId = tag.replyToId() != null ? tag.replyToId() : payload.replyToId();
        boolean replyToTag = tag.hasTag() || payload.replyToTag();
        boolean replyToCurrent = tag.replyToCurrent() || payload.replyToCurrent();

        return new AutoReplyTypes.ReplyPayload(
                cleanedText, payload.mediaUrl(), payload.mediaUrls(),
                replyToId, replyToTag, replyToCurrent,
                payload.audioAsVoice(), payload.isError(), payload.channelData());
    }

    /**
     * Check if a payload has renderable content.
     */
    public static boolean isRenderablePayload(AutoReplyTypes.ReplyPayload payload) {
        return (payload.text() != null && !payload.text().isEmpty())
                || (payload.mediaUrl() != null && !payload.mediaUrl().isEmpty())
                || (payload.mediaUrls() != null && !payload.mediaUrls().isEmpty())
                || payload.audioAsVoice()
                || (payload.channelData() != null && !payload.channelData().isEmpty());
    }

    /**
     * Apply reply tags + threading filter + renderable filter to a list of
     * payloads.
     */
    public static List<AutoReplyTypes.ReplyPayload> applyReplyThreading(
            List<AutoReplyTypes.ReplyPayload> payloads,
            String replyToMode,
            String replyToChannel,
            String currentMessageId) {
        UnaryOperator<AutoReplyTypes.ReplyPayload> filter = ReplyThreading
                .createReplyToModeFilterForChannel(replyToMode, replyToChannel);
        List<AutoReplyTypes.ReplyPayload> result = new ArrayList<>();
        for (var payload : payloads) {
            var tagged = applyReplyTagsToPayload(payload, currentMessageId);
            if (isRenderablePayload(tagged)) {
                result.add(filter.apply(tagged));
            }
        }
        return result;
    }

    /**
     * Filter out payloads whose text duplicates a messaging tool send.
     */
    public static List<AutoReplyTypes.ReplyPayload> filterMessagingToolDuplicates(
            List<AutoReplyTypes.ReplyPayload> payloads,
            List<String> sentTexts) {
        if (sentTexts == null || sentTexts.isEmpty())
            return payloads;
        List<AutoReplyTypes.ReplyPayload> result = new ArrayList<>();
        for (var payload : payloads) {
            String text = payload.text() != null ? payload.text() : "";
            boolean isDuplicate = sentTexts.stream().anyMatch(
                    sent -> sent != null && !sent.isEmpty() && text.contains(sent));
            if (!isDuplicate)
                result.add(payload);
        }
        return result;
    }

    /**
     * Normalize an account ID for comparison.
     */
    public static String normalizeAccountId(String value) {
        if (value == null)
            return null;
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed.toLowerCase();
    }

    /**
     * Check whether messaging-tool replies should be suppressed because
     * the tool already sent to the same target.
     */
    public static boolean shouldSuppressMessagingToolReplies(
            String messageProvider,
            List<MessagingToolSend> messagingToolSentTargets,
            String originatingTo,
            String accountId) {
        String provider = messageProvider != null ? messageProvider.trim().toLowerCase() : null;
        if (provider == null || provider.isEmpty())
            return false;

        String originTarget = normalizeTarget(provider, originatingTo);
        if (originTarget == null)
            return false;

        String originAccount = normalizeAccountId(accountId);
        if (messagingToolSentTargets == null || messagingToolSentTargets.isEmpty())
            return false;

        return messagingToolSentTargets.stream().anyMatch(target -> {
            if (target == null || target.provider() == null)
                return false;
            if (!target.provider().trim().toLowerCase().equals(provider))
                return false;
            String targetKey = normalizeTarget(provider, target.to());
            if (targetKey == null)
                return false;
            String targetAccount = normalizeAccountId(target.accountId());
            if (originAccount != null && targetAccount != null && !originAccount.equals(targetAccount))
                return false;
            return targetKey.equals(originTarget);
        });
    }

    /** Simplified target normalization. */
    private static String normalizeTarget(String provider, String to) {
        if (to == null || to.isBlank())
            return null;
        return to.trim().toLowerCase();
    }

    /** Messaging tool send target record. */
    public record MessagingToolSend(String provider, String to, String accountId) {
    }
}
