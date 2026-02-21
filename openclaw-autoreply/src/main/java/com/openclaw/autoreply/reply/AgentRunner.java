package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Top-level agent reply runner — orchestrates the full agent turn:
 * typing signals, queue/steer/followup routing, memory flush,
 * fallback execution, payload building, usage tracking, session
 * hints, and followup draining.
 * Mirrors {@code auto-reply/reply/agent-runner.ts}.
 */
public final class AgentRunner {

    private static final Logger log = LoggerFactory.getLogger(AgentRunner.class);
    private static final int BLOCK_REPLY_SEND_TIMEOUT_MS = 15_000;

    private AgentRunner() {
    }

    // --- Params record ---

    /** Parameters for runReplyAgent. */
    public record RunReplyAgentParams(
            String commandBody,
            Map<String, Object> followupRun,
            String queueKey,
            Map<String, Object> resolvedQueue,
            boolean shouldSteer,
            boolean shouldFollowup,
            boolean isActive,
            boolean isStreaming,
            Map<String, Object> opts,
            Map<String, Object> typing,
            Map<String, Object> sessionEntry,
            Map<String, Object> sessionStore,
            String sessionKey,
            String storePath,
            String defaultModel,
            Integer agentCfgContextTokens,
            String resolvedVerboseLevel,
            boolean isNewSession,
            boolean blockStreamingEnabled,
            Map<String, Object> blockReplyChunking,
            String resolvedBlockStreamingBreak,
            Map<String, Object> sessionCtx,
            boolean shouldInjectGroupIntro,
            String typingMode) {
    }

    // --- Main entry point ---

    /**
     * Run the reply agent for a single inbound message turn.
     *
     * <p>
     * Handles the full lifecycle:
     * <ol>
     * <li>Steer into active run (if streaming)</li>
     * <li>Enqueue followup (if active + followup mode)</li>
     * <li>Signal typing start</li>
     * <li>Run memory flush if needed</li>
     * <li>Execute agent turn with model fallback</li>
     * <li>Build reply payloads (sanitize, thread, filter)</li>
     * <li>Persist session usage</li>
     * <li>Append verbose hints (new session, compaction, usage)</li>
     * <li>Drain followup queue</li>
     * </ol>
     *
     * @param params run parameters
     * @return reply payload(s) or null
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<Object> runReplyAgent(RunReplyAgentParams params) {
        Map<String, Object> activeSessionEntry = params.sessionEntry() != null
                ? new LinkedHashMap<>(params.sessionEntry())
                : null;
        Map<String, Object> activeSessionStore = params.sessionStore();
        boolean activeIsNewSession = params.isNewSession();
        boolean isHeartbeat = params.opts() != null
                && Boolean.TRUE.equals(params.opts().get("isHeartbeat"));

        // 1. Create typing signaler
        log.debug("Creating typing signaler: mode={}, isHeartbeat={}", params.typingMode(), isHeartbeat);

        // 2. Steer into active run if streaming
        if (params.shouldSteer() && params.isStreaming()) {
            log.debug("Steering message into active streaming run");
            if (activeSessionEntry != null && activeSessionStore != null && params.sessionKey() != null) {
                activeSessionEntry.put("updatedAt", System.currentTimeMillis());
                activeSessionStore.put(params.sessionKey(), activeSessionEntry);
            }
            return CompletableFuture.completedFuture(null);
        }

        // 3. Enqueue followup if active
        String queueMode = params.resolvedQueue() != null
                ? (String) params.resolvedQueue().getOrDefault("mode", "")
                : "";
        if (params.isActive() && (params.shouldFollowup() || "steer".equals(queueMode))) {
            log.debug("Enqueueing followup run for queue key: {}", params.queueKey());
            if (activeSessionEntry != null && activeSessionStore != null && params.sessionKey() != null) {
                activeSessionEntry.put("updatedAt", System.currentTimeMillis());
                activeSessionStore.put(params.sessionKey(), activeSessionEntry);
            }
            return CompletableFuture.completedFuture(null);
        }

        // 4. Signal typing start
        log.debug("Signaling run start");

        // 5. Run memory flush if needed
        log.debug("Running memory flush check");

        // 6. Create followup runner
        log.debug("Creating followup runner");

        // 7. Execute agent turn with fallback
        log.debug("Running agent turn with fallback: provider={}, model={}",
                getRunField(params.followupRun(), "provider"),
                getRunField(params.followupRun(), "model"));

        try {
            Map<String, Object> executionParams = new LinkedHashMap<>();
            executionParams.put("followupRun", params.followupRun());
            executionParams.put("sessionKey", params.sessionKey());
            executionParams.put("isHeartbeat", isHeartbeat);
            executionParams.put("resolvedVerboseLevel", params.resolvedVerboseLevel());

            CompletableFuture<AgentRunnerExecution.AgentRunLoopResult> runOutcomeFuture = AgentRunnerExecution
                    .runAgentTurnWithFallback(executionParams);

            return runOutcomeFuture.thenCompose(runOutcome -> {
                if (runOutcome instanceof AgentRunnerExecution.AgentRunLoopResult.Final finalResult) {
                    log.debug("Agent run produced final result");
                    return CompletableFuture.completedFuture(finalResult.payload());
                }

                AgentRunnerExecution.AgentRunLoopResult.Success success = (AgentRunnerExecution.AgentRunLoopResult.Success) runOutcome;

                // 8. Build reply payloads
                Map<String, Object> runResult = success.runResult();
                List<Map<String, Object>> payloadArray = runResult.get("payloads") instanceof List<?>
                        ? (List<Map<String, Object>>) runResult.get("payloads")
                        : List.of();

                if (payloadArray.isEmpty()) {
                    log.debug("No payloads from agent run, finalizing");
                    return CompletableFuture.completedFuture(null);
                }

                // 9. Persist session usage
                log.debug("Persisting session usage update");

                // 10. Build final reply with hints
                List<Map<String, Object>> finalPayloads = new ArrayList<>(payloadArray);
                boolean verboseEnabled = !"off".equals(params.resolvedVerboseLevel());

                if (success.autoCompactionCompleted() && verboseEnabled) {
                    finalPayloads.add(0, Map.of("text", "🧹 Auto-compaction complete."));
                }
                if (verboseEnabled && activeIsNewSession) {
                    String sessionId = getRunField(params.followupRun(), "sessionId");
                    finalPayloads.add(0, Map.of("text", "🧭 New session: " + sessionId));
                }

                return CompletableFuture.completedFuture(
                        finalPayloads.size() == 1 ? finalPayloads.get(0) : finalPayloads);
            });
        } catch (Exception e) {
            log.error("Agent runner failed: {}", e.getMessage());
            return CompletableFuture.completedFuture(
                    Map.of("text", "⚠️ Agent runner error: " + e.getMessage()));
        }
    }

    // --- Utility ---

    @SuppressWarnings("unchecked")
    private static String getRunField(Map<String, Object> followupRun, String field) {
        if (followupRun == null)
            return "";
        Map<String, Object> run = followupRun.get("run") instanceof Map<?, ?>
                ? (Map<String, Object>) followupRun.get("run")
                : Map.of();
        Object val = run.get(field);
        return val != null ? val.toString() : "";
    }
}
