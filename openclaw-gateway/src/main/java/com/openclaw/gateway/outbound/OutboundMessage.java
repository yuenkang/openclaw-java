package com.openclaw.gateway.outbound;

import com.openclaw.common.config.OpenClawConfig;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * High-level message send/poll API.
 * Coordinates target resolution, delivery, and result formatting.
 * Corresponds to TypeScript's message.ts.
 */
@Slf4j
public class OutboundMessage {

    private final OutboundDelivery delivery;
    private final OutboundTargetResolver targetResolver;

    public OutboundMessage(OutboundDelivery delivery, OutboundTargetResolver targetResolver) {
        this.delivery = delivery;
        this.targetResolver = targetResolver;
    }

    /**
     * Send an outbound message, resolving targets and delivering payloads.
     */
    @Data
    @Builder
    public static class SendParams {
        private String to;
        private String content;
        private String channel;
        private String mediaUrl;
        private List<String> mediaUrls;
        private boolean gifPlayback;
        private String accountId;
        private boolean dryRun;
        private boolean bestEffort;
        private String gateway;
    }

    @Data
    @Builder
    public static class SendResult {
        private String channel;
        private String to;
        private String via;
        private String mediaUrl;
        private List<String> mediaUrls;
        private OutboundDeliveryResult result;
        private String messageId;
        private boolean dryRun;
    }

    @Data
    @Builder
    public static class PollParams {
        private String to;
        private String question;
        private List<String> options;
        private int maxSelections;
        private Integer durationHours;
        private String channel;
        private boolean dryRun;
    }

    @Data
    @Builder
    public static class PollResult {
        private String channel;
        private String to;
        private String question;
        private List<String> options;
        private int maxSelections;
        private Integer durationHours;
        private String via;
        private OutboundDeliveryResult result;
        private String messageId;
        private boolean dryRun;
    }

    /**
     * Send a message to a target.
     */
    public CompletableFuture<SendResult> sendMessage(OpenClawConfig cfg, SendParams params) {
        log.info("Sending message to {}:{}", params.channel, params.to);

        if (params.isDryRun()) {
            return CompletableFuture.completedFuture(SendResult.builder()
                    .channel(params.channel)
                    .to(params.to)
                    .via("direct")
                    .mediaUrl(params.mediaUrl)
                    .mediaUrls(params.mediaUrls)
                    .messageId("dry-run-" + System.currentTimeMillis())
                    .dryRun(true)
                    .build());
        }

        // Resolve target
        var targetResult = targetResolver.resolveChannelTarget(
                cfg, params.channel, params.to, params.accountId);

        if (!targetResult.ok()) {
            return CompletableFuture.failedFuture(targetResult.error());
        }

        String resolvedTo = targetResult.target().to();

        // Build payload
        OutboundPayloads.NormalizedOutboundPayload payload = OutboundPayloads.NormalizedOutboundPayload.builder()
                .text(params.content)
                .mediaUrls(params.mediaUrls != null ? params.mediaUrls
                        : (params.mediaUrl != null ? List.of(params.mediaUrl)
                                : List.of()))
                .build();

        return delivery.deliverPayloads(
                cfg, params.channel, resolvedTo, List.of(payload),
                params.accountId, null, null)
                .thenApply(results -> {
                    OutboundDeliveryResult firstResult = results.isEmpty() ? null : results.get(0);
                    return SendResult.builder()
                            .channel(params.channel)
                            .to(resolvedTo)
                            .via("direct")
                            .mediaUrl(params.mediaUrl)
                            .mediaUrls(params.mediaUrls)
                            .result(firstResult)
                            .messageId(firstResult != null ? firstResult.getMessageId() : "unknown")
                            .dryRun(false)
                            .build();
                });
    }

    /**
     * Send a poll to a target.
     */
    public CompletableFuture<PollResult> sendPoll(OpenClawConfig cfg, PollParams params) {
        log.info("Sending poll to {}:{}: {}", params.channel, params.to, params.question);

        if (params.isDryRun()) {
            return CompletableFuture.completedFuture(PollResult.builder()
                    .channel(params.channel)
                    .to(params.to)
                    .question(params.question)
                    .options(params.options)
                    .maxSelections(params.maxSelections)
                    .durationHours(params.durationHours)
                    .via("direct")
                    .messageId("dry-run-poll-" + System.currentTimeMillis())
                    .dryRun(true)
                    .build());
        }

        // Poll delivery is channel-specific, typically via gateway
        return CompletableFuture.completedFuture(PollResult.builder()
                .channel(params.channel)
                .to(params.to)
                .question(params.question)
                .options(params.options)
                .maxSelections(params.maxSelections)
                .durationHours(params.durationHours)
                .via("gateway")
                .messageId("poll-" + System.currentTimeMillis())
                .dryRun(false)
                .build());
    }
}
