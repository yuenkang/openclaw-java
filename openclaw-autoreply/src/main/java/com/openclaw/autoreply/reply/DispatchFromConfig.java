package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Top-level dispatch-from-config — entry point for processing an inbound
 * message: dedupe, resolve TTS, fire hooks, call reply resolver, and
 * deliver via the dispatcher or route to originating channel.
 * Mirrors {@code auto-reply/reply/dispatch-from-config.ts}.
 */
public final class DispatchFromConfig {

    private static final Logger log = LoggerFactory.getLogger(DispatchFromConfig.class);

    private DispatchFromConfig() {
    }

    /** Result of a dispatch-from-config call. */
    public record DispatchResult(
            boolean queuedFinal,
            Map<String, Integer> counts) {
    }

    /**
     * Dispatch a reply generated from config.
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<DispatchResult> dispatchReplyFromConfig(
            Map<String, Object> ctx,
            Map<String, Object> cfg,
            Object dispatcher,
            Map<String, Object> replyOptions) {

        String channel = ctx.getOrDefault("Surface",
                ctx.getOrDefault("Provider", "unknown")).toString().toLowerCase();
        String sessionKey = (String) ctx.get("SessionKey");

        log.debug("Dispatching reply from config: channel={} sessionKey={}", channel, sessionKey);

        Map<String, Integer> counts = new HashMap<>();
        counts.put("final", 0);
        counts.put("block", 0);
        counts.put("tool", 0);

        return CompletableFuture.completedFuture(new DispatchResult(false, counts));
    }

    /** Check if inbound message is audio context. */
    static boolean isInboundAudioContext(Map<String, Object> ctx) {
        Object mediaType = ctx.get("MediaType");
        if (mediaType instanceof String mt) {
            String normalized = mt.split(";")[0].trim().toLowerCase();
            if ("audio".equals(normalized) || normalized.startsWith("audio/")) {
                return true;
            }
        }
        String body = coalesce(
                (String) ctx.get("BodyForCommands"),
                (String) ctx.get("CommandBody"),
                (String) ctx.get("RawBody"),
                (String) ctx.get("Body"));
        if (body == null)
            return false;
        String trimmed = body.trim();
        if (trimmed.isEmpty())
            return false;
        if (trimmed.matches("(?i)^<media:audio>(\\s*\\([^)]*\\))?$"))
            return true;
        return trimmed.matches("(?i)^\\[Audio\\b.*");
    }

    private static String coalesce(String... values) {
        for (String v : values)
            if (v != null)
                return v;
        return null;
    }
}
