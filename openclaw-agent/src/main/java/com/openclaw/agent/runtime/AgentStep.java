package com.openclaw.agent.runtime;

import lombok.extern.slf4j.Slf4j;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;

/**
 * Agent step tracking: run a nested agent step and read the reply.
 * Corresponds to TypeScript agents/tools/agent-step.ts.
 */
@Slf4j
public final class AgentStep {

    /** Lane name for nested agent steps. */
    public static final String AGENT_LANE_NESTED = "agent:nested";
    /** Internal message channel for agent steps. */
    public static final String INTERNAL_MESSAGE_CHANNEL = "internal";

    private AgentStep() {
    }

    /**
     * Read the latest assistant reply from a session's history.
     *
     * @param sessionKey Session key to read from
     * @param limit      Max messages to scan (default 50)
     * @return Latest assistant text or null
     */
    public static CompletableFuture<String> readLatestAssistantReply(
            String sessionKey, int limit) {
        return CompletableFuture.supplyAsync(() -> {
            // TODO: integrate with gateway chat.history + stripToolMessages
            log.debug("readLatestAssistantReply: stub for sessionKey={}, limit={}", sessionKey, limit);
            return null;
        });
    }

    /**
     * Run a single agent step: dispatch to gateway, wait for completion, read
     * reply.
     *
     * @param sessionKey        Session on which to run
     * @param message           The user message for this step
     * @param extraSystemPrompt Additional system prompt text
     * @param timeoutMs         Max time to wait
     * @param channel           Channel override (defaults to internal)
     * @param lane              Lane override (defaults to nested)
     * @return The assistant's reply text, or null if step failed or timed out
     */
    public static CompletableFuture<String> runAgentStep(
            String sessionKey, String message, String extraSystemPrompt,
            long timeoutMs, String channel, String lane) {
        return CompletableFuture.supplyAsync(() -> {
            String stepIdem = UUID.randomUUID().toString();
            String resolvedChannel = (channel != null && !channel.isBlank())
                    ? channel
                    : INTERNAL_MESSAGE_CHANNEL;
            String resolvedLane = (lane != null && !lane.isBlank())
                    ? lane
                    : AGENT_LANE_NESTED;

            // TODO: gateway call — agent dispatch
            log.debug("runAgentStep: stub dispatch sessionKey={}, idem={}, channel={}, lane={}",
                    sessionKey, stepIdem, resolvedChannel, resolvedLane);

            // TODO: gateway call — agent.wait
            long stepWaitMs = Math.min(timeoutMs, 60_000);
            log.debug("runAgentStep: stub wait runId={}, timeoutMs={}", stepIdem, stepWaitMs);

            // TODO: read latest assistant reply
            log.debug("runAgentStep: stub readLatestAssistantReply");
            return null;
        });
    }

    /**
     * Convenience overload with default channel and lane.
     */
    public static CompletableFuture<String> runAgentStep(
            String sessionKey, String message, String extraSystemPrompt, long timeoutMs) {
        return runAgentStep(sessionKey, message, extraSystemPrompt, timeoutMs, null, null);
    }
}
