package com.openclaw.autoreply;

import java.util.List;
import java.util.Map;

/**
 * Core types for the auto-reply subsystem.
 * Mirrors {@code auto-reply/types.ts}.
 */
public final class AutoReplyTypes {

    private AutoReplyTypes() {
    }

    /** Context for block reply delivery. */
    public record BlockReplyContext(long timeoutMs) {
    }

    /** Context passed to onModelSelected callback with actual model used. */
    public record ModelSelectedContext(String provider, String model, String thinkLevel) {
    }

    /** A single reply payload. */
    public record ReplyPayload(
            String text,
            String mediaUrl,
            List<String> mediaUrls,
            String replyToId,
            boolean replyToTag,
            boolean replyToCurrent,
            boolean audioAsVoice,
            boolean isError,
            Map<String, Object> channelData) {
    }
}
