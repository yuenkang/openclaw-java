package com.openclaw.autoreply.reply;

import com.openclaw.autoreply.AutoReplyTypes;
import com.openclaw.autoreply.ReplyTokens;

/**
 * Normalize a reply payload — strips silent/heartbeat tokens, applies
 * response prefix, and filters empty content.
 * Mirrors {@code auto-reply/reply/normalize-reply.ts}.
 */
public final class NormalizeReply {

    private NormalizeReply() {
    }

    /** Reason a payload was skipped. */
    public static final String REASON_EMPTY = "empty";
    public static final String REASON_SILENT = "silent";
    public static final String REASON_HEARTBEAT = "heartbeat";

    /** Options for normalization. */
    public record NormalizeOptions(
            String responsePrefix,
            boolean stripHeartbeat,
            String silentToken,
            java.util.function.Consumer<String> onSkip,
            Runnable onHeartbeatStrip) {

        public NormalizeOptions {
            if (silentToken == null)
                silentToken = ReplyTokens.SILENT_REPLY_TOKEN;
        }
    }

    /**
     * Normalize a reply payload, returning {@code null} if the payload should
     * be suppressed (empty, silent, heartbeat-only).
     */
    public static AutoReplyTypes.ReplyPayload normalizeReplyPayload(
            AutoReplyTypes.ReplyPayload payload,
            NormalizeOptions opts) {

        boolean hasMedia = (payload.mediaUrl() != null && !payload.mediaUrl().isEmpty())
                || (payload.mediaUrls() != null && !payload.mediaUrls().isEmpty());
        boolean hasChannelData = payload.channelData() != null && !payload.channelData().isEmpty();
        String trimmed = payload.text() != null ? payload.text().trim() : "";

        if (trimmed.isEmpty() && !hasMedia && !hasChannelData) {
            if (opts != null && opts.onSkip() != null)
                opts.onSkip().accept(REASON_EMPTY);
            return null;
        }

        String silentToken = opts != null && opts.silentToken() != null
                ? opts.silentToken()
                : ReplyTokens.SILENT_REPLY_TOKEN;
        String text = payload.text();

        if (text != null && ReplyTokens.isSilentReplyText(text, silentToken)) {
            if (!hasMedia && !hasChannelData) {
                if (opts != null && opts.onSkip() != null)
                    opts.onSkip().accept(REASON_SILENT);
                return null;
            }
            text = "";
        }
        if (text != null && text.trim().isEmpty()) {
            text = "";
        }

        // Heartbeat stripping
        boolean shouldStripHeartbeat = opts == null || opts.stripHeartbeat();
        if (shouldStripHeartbeat && text != null && text.contains(ReplyTokens.HEARTBEAT_TOKEN)) {
            String stripped = text.replace(ReplyTokens.HEARTBEAT_TOKEN, "").trim();
            if (opts != null && opts.onHeartbeatStrip() != null) {
                opts.onHeartbeatStrip().run();
            }
            if (stripped.isEmpty() && !hasMedia && !hasChannelData) {
                if (opts != null && opts.onSkip() != null)
                    opts.onSkip().accept(REASON_HEARTBEAT);
                return null;
            }
            text = stripped;
        }

        if ((text == null || text.trim().isEmpty()) && !hasMedia && !hasChannelData) {
            if (opts != null && opts.onSkip() != null)
                opts.onSkip().accept(REASON_EMPTY);
            return null;
        }

        // Apply response prefix
        String prefix = opts != null ? opts.responsePrefix() : null;
        if (prefix != null && !prefix.isEmpty()
                && text != null && !text.trim().isEmpty()
                && !text.contains(ReplyTokens.HEARTBEAT_TOKEN)
                && !text.startsWith(prefix)) {
            text = prefix + " " + text;
        }

        return new AutoReplyTypes.ReplyPayload(
                text, payload.mediaUrl(), payload.mediaUrls(),
                payload.replyToId(), payload.replyToTag(), payload.replyToCurrent(),
                payload.audioAsVoice(), payload.isError(), payload.channelData());
    }

    /**
     * Convenience overload with default options (strip heartbeat, default silent
     * token).
     */
    public static AutoReplyTypes.ReplyPayload normalizeReplyPayload(
            AutoReplyTypes.ReplyPayload payload) {
        return normalizeReplyPayload(payload,
                new NormalizeOptions(null, true, null, null, null));
    }
}
