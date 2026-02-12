package com.openclaw.agent.tools.builtin;

import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.CompletableFuture;

/**
 * Agent-to-agent (A2A) messaging flow — ping-pong conversation + announce.
 * Corresponds to TypeScript sessions-send-tool.a2a.ts.
 *
 * <p>
 * This runs asynchronously after the initial sessions_send completes.
 * It orchestrates multi-turn ping-pong between requester and target sessions,
 * then announces the final result to the target channel.
 * </p>
 */
@Slf4j
public class SessionsSendA2ATool {

    /**
     * Run the A2A flow: wait for initial reply, run ping-pong turns, then announce.
     * Currently stubbed — requires agent.wait + agent step execution via gateway.
     */
    public static CompletableFuture<Void> runA2AFlow(A2AFlowParams params) {
        return CompletableFuture.runAsync(() -> {
            try {
                doRunA2AFlow(params);
            } catch (Exception e) {
                log.warn("sessions_send A2A flow failed: runId={}", params.waitRunId, e);
            }
        });
    }

    private static void doRunA2AFlow(A2AFlowParams params) {
        String primaryReply = params.roundOneReply;
        String latestReply = params.roundOneReply;

        if (primaryReply == null && params.waitRunId != null) {
            // TODO: call gateway agent.wait to get the initial reply
            log.debug("A2A flow: waiting for initial reply, runId={}", params.waitRunId);
            return;
        }

        if (latestReply == null) {
            log.debug("A2A flow: no reply available, skipping");
            return;
        }

        // Resolve announce target from session key
        SessionsSendHelpers.AnnounceTarget announceTarget = SessionsSendHelpers
                .resolveAnnounceTargetFromKey(params.targetSessionKey);
        if (announceTarget == null) {
            announceTarget = SessionsSendHelpers.resolveAnnounceTargetFromKey(params.displayKey);
        }
        String targetChannel = announceTarget != null ? announceTarget.channel() : "unknown";

        // Ping-pong turns
        if (params.maxPingPongTurns > 0
                && params.requesterSessionKey != null
                && !params.requesterSessionKey.equals(params.targetSessionKey)) {

            for (int turn = 1; turn <= params.maxPingPongTurns; turn++) {
                // TODO: implement actual ping-pong via runAgentStep
                log.debug("A2A ping-pong turn {}/{}: (stubbed)", turn, params.maxPingPongTurns);
                break; // stub: exit after logging
            }
        }

        // Announce step — build context for future gateway integration
        log.debug("A2A announce: target={}, channel={}", params.displayKey, targetChannel);
        // TODO: call runAgentStep for announce, then deliver via gateway send
    }

    /**
     * Parameters for the A2A flow.
     */
    public record A2AFlowParams(
            String targetSessionKey,
            String displayKey,
            String message,
            long announceTimeoutMs,
            int maxPingPongTurns,
            String requesterSessionKey,
            String requesterChannel,
            String roundOneReply,
            String waitRunId) {
    }
}
