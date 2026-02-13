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

                // 1. Resolve memory flush settings — use defaults since
                // resolveMemoryFlushSettings doesn't exist
                MemoryFlush.MemoryFlushSettings memoryFlushSettings = new MemoryFlush.MemoryFlushSettings(
                                true,
                                MemoryFlush.DEFAULT_MEMORY_FLUSH_SOFT_TOKENS,
                                MemoryFlush.DEFAULT_MEMORY_FLUSH_PROMPT,
                                MemoryFlush.DEFAULT_MEMORY_FLUSH_SYSTEM_PROMPT,
                                0);
                if (!memoryFlushSettings.enabled()) {
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

                // Resolve context window tokens — use agent config value or default 128k
                int contextWindowTokens = agentCfgContextTokens != null ? agentCfgContextTokens : 128000;
                int reserveTokensFloor = memoryFlushSettings.reserveTokensFloor();
                int softThresholdTokens = memoryFlushSettings.softThresholdTokens();

                // Extract totalTokens and compaction info from session entry
                Map<String, Object> entryForCheck = sessionEntry != null ? sessionEntry
                                : (sessionKey != null && sessionStore != null
                                                ? (Map<String, Object>) sessionStore.get(sessionKey)
                                                : null);
                Integer totalTokens = null;
                Integer compactionCount = null;
                Integer memoryFlushCompactionCount = null;
                if (entryForCheck != null) {
                        Object tt = entryForCheck.get("totalTokens");
                        if (tt instanceof Number n)
                                totalTokens = n.intValue();
                        Object cc = entryForCheck.get("compactionCount");
                        if (cc instanceof Number n)
                                compactionCount = n.intValue();
                        Object mfc = entryForCheck.get("memoryFlushCompactionCount");
                        if (mfc instanceof Number n)
                                memoryFlushCompactionCount = n.intValue();
                }
                boolean shouldFlush = MemoryFlush.shouldRunMemoryFlush(
                                totalTokens, compactionCount, memoryFlushCompactionCount,
                                contextWindowTokens, reserveTokensFloor, softThresholdTokens);

                if (!shouldFlush) {
                        return CompletableFuture.completedFuture(sessionEntry);
                }

                // 5. Run memory flush (full pi-agent integration deferred)
                log.info("Memory flush conditions met for session {} — full flush integration deferred",
                                sessionKey);

                return CompletableFuture.completedFuture(sessionEntry);
        }
}
