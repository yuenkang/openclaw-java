package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;

/**
 * Followup runner — creates a reusable callback that executes queued
 * followup turns via the embedded PI agent with model fallback, routes
 * replies to the originating channel, and persists session usage.
 * Mirrors {@code auto-reply/reply/followup-runner.ts}.
 */
public final class FollowupRunner {

    private static final Logger log = LoggerFactory.getLogger(FollowupRunner.class);

    private FollowupRunner() {
    }

    /** Parameters for creating a followup runner. */
    public record FollowupRunnerParams(
            Map<String, Object> opts,
            Typing.TypingController typing,
            String typingMode,
            Map<String, Object> sessionEntry,
            Map<String, Object> sessionStore,
            String sessionKey,
            String storePath,
            String defaultModel,
            Integer agentCfgContextTokens) {
    }

    /** A queued followup run descriptor. */
    public record FollowupRun(
            Map<String, Object> run,
            String prompt,
            String originatingChannel,
            String originatingTo,
            String originatingAccountId,
            String originatingThreadId,
            String originatingChatType) {
    }

    /**
     * Create a followup runner consumer that processes queued followup turns.
     */
    public static Consumer<FollowupRun> createFollowupRunner(FollowupRunnerParams params) {
        TypingMode.TypingSignaler typingSignals = TypingMode.createTypingSignaler(
                params.typing(), params.typingMode(),
                params.opts() != null && Boolean.TRUE.equals(params.opts().get("isHeartbeat")));

        return (FollowupRun queued) -> {
            try {
                String runId = UUID.randomUUID().toString();
                if (queued.run().get("sessionKey") != null) {
                    log.debug("Registering followup run context: runId={} sessionKey={}",
                            runId, queued.run().get("sessionKey"));
                }

                // Full model-fallback + pi-agent execution deferred
                log.debug("Followup runner: executing queued turn for session {}",
                        queued.run().get("sessionKey"));

                // Persist session usage (deferred)

                // Build, sanitize, and route reply payloads (deferred)

            } catch (Exception e) {
                log.error("Followup agent failed: {}", e.getMessage());
            } finally {
                params.typing().markRunComplete();
            }
        };
    }

    /**
     * Send followup payloads, routing to the originating channel if set.
     */
    @SuppressWarnings("unchecked")
    static CompletableFuture<Void> sendFollowupPayloads(
            List<Map<String, Object>> payloads,
            FollowupRun queued,
            Map<String, Object> opts,
            TypingMode.TypingSignaler typingSignals) {

        String originatingChannel = queued.originatingChannel();
        String originatingTo = queued.originatingTo();
        boolean shouldRouteToOriginating = originatingChannel != null
                && !originatingChannel.isEmpty()
                && originatingTo != null && !originatingTo.isEmpty();

        Consumer<Map<String, Object>> onBlockReply = opts != null
                ? (Consumer<Map<String, Object>>) opts.get("onBlockReply")
                : null;

        if (!shouldRouteToOriginating && onBlockReply == null) {
            log.debug("followup queue: no onBlockReply handler; dropping payloads");
            return CompletableFuture.completedFuture(null);
        }

        for (Map<String, Object> payload : payloads) {
            String text = (String) payload.get("text");
            String mediaUrl = (String) payload.get("mediaUrl");
            List<String> mediaUrls = (List<String>) payload.get("mediaUrls");

            boolean hasContent = (text != null && !text.isEmpty())
                    || mediaUrl != null
                    || (mediaUrls != null && !mediaUrls.isEmpty());
            if (!hasContent)
                continue;

            // Signal typing
            if (text != null) {
                typingSignals.signalTextDelta(text);
            }

            // Route to originating channel or fall back to dispatcher
            if (shouldRouteToOriginating) {
                RouteReply.RouteResult result = RouteReply.routeReply(
                        payload, originatingChannel, originatingTo,
                        (String) queued.run().get("sessionKey"),
                        queued.originatingAccountId(),
                        queued.originatingThreadId(),
                        queued.run());
                if (!result.ok() && onBlockReply != null) {
                    log.debug("followup queue: route-reply failed: {}", result.error());
                    onBlockReply.accept(payload);
                }
            } else if (onBlockReply != null) {
                onBlockReply.accept(payload);
            }
        }
        return CompletableFuture.completedFuture(null);
    }
}
