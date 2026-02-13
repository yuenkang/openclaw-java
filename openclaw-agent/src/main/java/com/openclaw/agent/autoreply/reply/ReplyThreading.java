package com.openclaw.agent.autoreply.reply;

import com.openclaw.agent.autoreply.AutoReplyTypes;

import java.util.function.UnaryOperator;

/**
 * Reply-to mode resolution and thread-tag filtering.
 * Mirrors {@code auto-reply/reply/reply-threading.ts}.
 */
public final class ReplyThreading {

    private ReplyThreading() {
    }

    /** Canonical reply-to modes. */
    public static final String MODE_OFF = "off";
    public static final String MODE_ALL = "all";
    public static final String MODE_FIRST = "first";

    /**
     * Resolve the effective reply-to mode.
     * <p>
     * In the full implementation this would consult a channel dock;
     * here we return "all" as the default.
     */
    public static String resolveReplyToMode(
            Object cfg, String channel, String accountId, String chatType) {
        // Simplified: channel-specific logic deferred to channels subsystem
        return MODE_ALL;
    }

    /**
     * Create a filter that strips or preserves {@code replyToId} based
     * on the resolved mode.
     *
     * @param mode             one of "off", "all", "first"
     * @param allowTagsWhenOff when true and mode is "off", keep replyToId if
     *                         replyToTag is set
     */
    public static UnaryOperator<AutoReplyTypes.ReplyPayload> createReplyToModeFilter(
            String mode, boolean allowTagsWhenOff) {
        final boolean[] hasThreaded = { false };

        return (payload) -> {
            if (payload.replyToId() == null || payload.replyToId().isEmpty()) {
                return payload;
            }
            if (MODE_OFF.equals(mode)) {
                if (allowTagsWhenOff && payload.replyToTag()) {
                    return payload;
                }
                return new AutoReplyTypes.ReplyPayload(
                        payload.text(), payload.mediaUrl(), payload.mediaUrls(),
                        null, payload.replyToTag(), payload.replyToCurrent(),
                        payload.audioAsVoice(), payload.isError(), payload.channelData());
            }
            if (MODE_ALL.equals(mode)) {
                return payload;
            }
            // "first" mode — only allow the first threaded reply
            if (hasThreaded[0]) {
                return new AutoReplyTypes.ReplyPayload(
                        payload.text(), payload.mediaUrl(), payload.mediaUrls(),
                        null, payload.replyToTag(), payload.replyToCurrent(),
                        payload.audioAsVoice(), payload.isError(), payload.channelData());
            }
            hasThreaded[0] = true;
            return payload;
        };
    }

    /**
     * Convenience overload with default {@code allowTagsWhenOff = false}.
     */
    public static UnaryOperator<AutoReplyTypes.ReplyPayload> createReplyToModeFilter(String mode) {
        return createReplyToModeFilter(mode, false);
    }

    /**
     * Create a mode filter that also considers channel-specific behaviour.
     * In the full implementation this would consult the channel dock for
     * {@code allowTagsWhenOff}; here it delegates to the simple filter.
     */
    public static UnaryOperator<AutoReplyTypes.ReplyPayload> createReplyToModeFilterForChannel(
            String mode, String channel) {
        // Simplified — allowTagsWhenOff deferred to channels subsystem
        return createReplyToModeFilter(mode, false);
    }
}
