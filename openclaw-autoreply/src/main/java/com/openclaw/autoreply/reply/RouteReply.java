package com.openclaw.autoreply.reply;

import com.openclaw.autoreply.AutoReplyTypes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;
import java.util.concurrent.CompletableFuture;

/**
 * Provider-agnostic reply router — routes replies to the originating channel.
 * Mirrors {@code auto-reply/reply/route-reply.ts}.
 */
public final class RouteReply {

    private static final Logger log = LoggerFactory.getLogger(RouteReply.class);

    private RouteReply() {
    }

    /** Internal/webchat channel constant. */
    public static final String INTERNAL_MESSAGE_CHANNEL = "webchat";

    /** Set of channels that support generic routing. */
    private static final Set<String> ROUTABLE_CHANNELS = Set.of(
            "telegram", "whatsapp", "slack", "discord", "line", "signal", "matrix", "messenger");

    /* ── types ──────────────────────────────────────────────── */

    /** Parameters for routing a reply to a channel. */
    public record RouteReplyParams(
            AutoReplyTypes.ReplyPayload payload,
            String channel,
            String to,
            String sessionKey,
            String accountId,
            String threadId,
            Object cfg,
            boolean mirror) {
    }

    /** Result of a route-reply operation. */
    public record RouteReplyResult(boolean ok, String messageId, String error) {
        public static RouteReplyResult success() {
            return new RouteReplyResult(true, null, null);
        }

        public static RouteReplyResult success(String messageId) {
            return new RouteReplyResult(true, messageId, null);
        }

        public static RouteReplyResult failure(String error) {
            return new RouteReplyResult(false, null, error);
        }
    }

    /* ── channel helpers ───────────────────────────────────── */

    /**
     * Normalize a channel identifier to its canonical form.
     */
    public static String normalizeMessageChannel(String channel) {
        if (channel == null)
            return null;
        String lower = channel.trim().toLowerCase();
        return lower.isEmpty() ? null : lower;
    }

    /**
     * Check if a channel is routable via routeReply.
     * Webchat/internal channels require special handling.
     */
    public static boolean isRoutableChannel(String channel) {
        if (channel == null || channel.isBlank())
            return false;
        String normalized = normalizeMessageChannel(channel);
        if (normalized == null)
            return false;
        if (INTERNAL_MESSAGE_CHANNEL.equals(normalized))
            return false;
        return ROUTABLE_CHANNELS.contains(normalized);
    }

    /**
     * Check if a channel is the internal/webchat channel.
     */
    public static boolean isInternalMessageChannel(String channel) {
        return INTERNAL_MESSAGE_CHANNEL.equals(normalizeMessageChannel(channel));
    }

    /* ── routing ───────────────────────────────────────────── */

    /**
     * Route a reply payload to the specified channel.
     * <p>
     * Full provider integration deferred — currently returns a stubbed result.
     * The real implementation will load outbound plumbing lazily and deliver
     * via provider-specific APIs.
     */
    public static CompletableFuture<RouteReplyResult> routeReply(RouteReplyParams params) {
        if (params.channel() == null || params.to() == null || params.to().isBlank()) {
            return CompletableFuture.completedFuture(
                    RouteReplyResult.failure("Missing channel or destination"));
        }

        if (isInternalMessageChannel(params.channel())) {
            return CompletableFuture.completedFuture(
                    RouteReplyResult.failure("Webchat routing not supported for queued replies"));
        }

        if (!isRoutableChannel(params.channel())) {
            return CompletableFuture.completedFuture(
                    RouteReplyResult.failure("Unknown channel: " + params.channel()));
        }

        // Stub: full provider integration deferred
        log.debug("routeReply stub: channel={} to={}", params.channel(), params.to());
        return CompletableFuture.completedFuture(RouteReplyResult.success());
    }
}
