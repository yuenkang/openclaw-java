package com.openclaw.gateway.outbound;

import com.openclaw.common.config.OpenClawConfig;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.CompletableFuture;

/**
 * Outbound send service for executing send/poll actions.
 * Bridges tool invocations to the outbound delivery pipeline.
 * Corresponds to TypeScript's outbound-send-service.ts.
 */
@Slf4j
public class OutboundSendService {

    private final OutboundMessage outboundMessage;

    public OutboundSendService(OutboundMessage outboundMessage) {
        this.outboundMessage = outboundMessage;
    }

    /**
     * Context for outbound message sending.
     */
    @Data
    @Builder
    public static class OutboundSendContext {
        private OpenClawConfig cfg;
        private String channel;
        private String accountId;
        private boolean dryRun;
        private String sessionKey;
        private String agentId;
    }

    /**
     * Result of executing a send action.
     */
    @Data
    @Builder
    public static class SendActionResult {
        /** "plugin" or "core" */
        private String handledBy;
        private Object payload;
        private OutboundMessage.SendResult sendResult;
    }

    /**
     * Result of executing a poll action.
     */
    @Data
    @Builder
    public static class PollActionResult {
        private String handledBy;
        private Object payload;
        private OutboundMessage.PollResult pollResult;
    }

    /**
     * Execute a send action.
     */
    public CompletableFuture<SendActionResult> executeSend(
            OutboundSendContext ctx,
            String to,
            String message,
            String mediaUrl,
            java.util.List<String> mediaUrls,
            boolean bestEffort) {

        OutboundMessage.SendParams params = OutboundMessage.SendParams.builder()
                .to(to)
                .content(message)
                .channel(ctx.getChannel())
                .mediaUrl(mediaUrl)
                .mediaUrls(mediaUrls)
                .accountId(ctx.getAccountId())
                .dryRun(ctx.isDryRun())
                .bestEffort(bestEffort)
                .build();

        return outboundMessage.sendMessage(ctx.getCfg(), params)
                .thenApply(result -> SendActionResult.builder()
                        .handledBy("core")
                        .payload(result)
                        .sendResult(result)
                        .build());
    }

    /**
     * Execute a poll action.
     */
    public CompletableFuture<PollActionResult> executePoll(
            OutboundSendContext ctx,
            String to,
            String question,
            java.util.List<String> options,
            int maxSelections,
            Integer durationHours) {

        OutboundMessage.PollParams params = OutboundMessage.PollParams.builder()
                .to(to)
                .question(question)
                .options(options)
                .maxSelections(maxSelections)
                .durationHours(durationHours)
                .channel(ctx.getChannel())
                .dryRun(ctx.isDryRun())
                .build();

        return outboundMessage.sendPoll(ctx.getCfg(), params)
                .thenApply(result -> PollActionResult.builder()
                        .handledBy("core")
                        .payload(result)
                        .pollResult(result)
                        .build());
    }
}
