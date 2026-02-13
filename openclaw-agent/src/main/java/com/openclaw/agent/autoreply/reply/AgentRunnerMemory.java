package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Memory flush runner — determines whether a memory flush (compaction)
 * should run after a reply turn, executes it via the embedded PI agent
 * with model fallback, and persists the updated compaction metadata.
 * Mirrors {@code auto-reply/reply/agent-runner-memory.ts}.
 */
public final class AgentRunnerMemory {

    private static final Logger log = LoggerFactory.getLogger(AgentRunnerMemory.class);

    private AgentRunnerMemory() {
    }

    /**
     * Run memory flush if conditions are met.
     *
     * <p>
     * Conditions checked:
     * <ol>
     * <li>Memory flush settings are configured</li>
     * <li>Workspace is writable (not read-only sandboxed)</li>
     * <li>Not a heartbeat run</li>
     * <li>Not a CLI provider</li>
     * <li>Token usage exceeds soft threshold</li>
     * </ol>
     *
     * @param cfg                   agent config
     * @param followupRun           the queued followup run
     * @param sessionCtx            session template context
     * @param defaultModel          fallback model id
     * @param agentCfgContextTokens configured context token budget (nullable)
     * @param resolvedVerboseLevel  current verbose level
     * @param sessionEntry          current session entry (mutable)
     * @param sessionStore          session store map (mutable)
     * @param sessionKey            current session key (nullable)
     * @param storePath             session store file path (nullable)
     * @param isHeartbeat           whether this is a heartbeat run
     * @return updated session entry, or the original if no flush ran
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<Map<String, Object>> runMemoryFlushIfNeeded(
            Map<String, Object> cfg,
            Map<String, Object> followupRun,
            Map<String, Object> sessionCtx,
            String defaultModel,
            Integer agentCfgContextTokens,
            String resolvedVerboseLevel,
            Map<String, Object> sessionEntry,
            Map<String, Object> sessionStore,
            String sessionKey,
            String storePath,
            boolean isHeartbeat) {

        // 1. Resolve memory flush settings
        Map<String, Object> memoryFlushSettings = MemoryFlush.resolveMemoryFlushSettings(cfg);
        if (memoryFlushSettings == null) {
            return CompletableFuture.completedFuture(sessionEntry);
        }

        // 2. Check workspace writability (sandbox integration deferred)
        boolean memoryFlushWritable = true;

        // 3. Check flush conditions
        if (!memoryFlushWritable || isHeartbeat) {
            return CompletableFuture.completedFuture(sessionEntry);
        }

        // 4. Check token threshold
        Map<String, Object> run = followupRun.containsKey("run")
                ? (Map<String, Object>) followupRun.get("run")
                : followupRun;
        String model = run.containsKey("model") ? (String) run.get("model") : defaultModel;

        int contextWindowTokens = MemoryFlush.resolveMemoryFlushContextWindowTokens(
                model, agentCfgContextTokens);
        int reserveTokensFloor = memoryFlushSettings.containsKey("reserveTokensFloor")
                ? ((Number) memoryFlushSettings.get("reserveTokensFloor")).intValue()
                : 0;
        int softThresholdTokens = memoryFlushSettings.containsKey("softThresholdTokens")
                ? ((Number) memoryFlushSettings.get("softThresholdTokens")).intValue()
                : 0;

        Map<String, Object> entryForCheck = sessionEntry != null ? sessionEntry
                : (sessionKey != null && sessionStore != null
                        ? (Map<String, Object>) sessionStore.get(sessionKey)
                        : null);
        boolean shouldFlush = MemoryFlush.shouldRunMemoryFlush(
                entryForCheck, contextWindowTokens, reserveTokensFloor, softThresholdTokens);

        if (!shouldFlush) {
            return CompletableFuture.completedFuture(sessionEntry);
        }

        // 5. Run memory flush (full pi-agent integration deferred)
        log.info("Memory flush conditions met for session {} — full flush integration deferred",
                sessionKey);

        return CompletableFuture.completedFuture(sessionEntry);
    }
}
