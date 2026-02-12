package com.openclaw.agent.runtime;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import com.openclaw.agent.models.AuthProfileStore;

import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.agent.runtime.EmbeddedRunTypes.*;
import com.openclaw.common.config.OpenClawConfig;

import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

/**
 * High-level agent run orchestrator.
 * Corresponds to TypeScript's pi-embedded-runner/run.ts (runEmbeddedPiAgent).
 *
 * <p>
 * Handles auth profile resolution, failover retry loops, model resolution,
 * usage aggregation, and delegates to {@link AgentRunner} for the actual
 * conversation loop.
 * </p>
 */
@Slf4j
public class RunOrchestrator {

    /** Anthropic magic string that can poison session transcripts. */
    private static final String ANTHROPIC_REFUSAL_MAGIC = "ANTHROPIC_MAGIC_STRING_TRIGGER_REFUSAL";
    private static final String ANTHROPIC_REFUSAL_REPLACEMENT = "ANTHROPIC MAGIC STRING TRIGGER REFUSAL (redacted)";

    private static final int MAX_AUTH_PROFILE_RETRIES = 5;

    private static final Pattern PROVIDER_SEPARATOR = Pattern.compile("[/:]");

    private final AgentRunner agentRunner;
    private final ActiveRunRegistry activeRunRegistry;
    private final ModelProviderRegistry providerRegistry;
    private final AuthProfileStore authProfileStore;
    private final OpenClawConfig config;

    public RunOrchestrator(
            AgentRunner agentRunner,
            ActiveRunRegistry activeRunRegistry,
            ModelProviderRegistry providerRegistry,
            AuthProfileStore authProfileStore,
            OpenClawConfig config) {
        this.agentRunner = agentRunner;
        this.activeRunRegistry = activeRunRegistry;
        this.providerRegistry = providerRegistry;
        this.authProfileStore = authProfileStore;
        this.config = config;
    }

    // ── Run parameters ────────────────────────────────────────────────

    @Data
    @Builder
    public static class RunParams {
        private String sessionId;
        private String sessionKey;
        private String agentId;
        private String userMessage;
        private String model; // "provider/modelId"
        private String systemPrompt;
        private String authProfileId;
        private String thinkLevel;
        private Integer maxTurns;
        @Builder.Default
        private double temperature = 0.7;

        private AgentRunner.AgentEventListener listener;
    }

    // ── Main entry point ──────────────────────────────────────────────

    /**
     * Run an agent session with full orchestration:
     * auth profile resolution, failover retry, history sanitization.
     *
     * @param params run parameters
     * @return the run result
     */
    public RunResult run(RunParams params) {
        long startTime = System.currentTimeMillis();
        String sessionId = params.getSessionId();

        // Parse provider/model
        String[] parsed = parseModelSpec(params.getModel());
        String provider = parsed[0];
        String modelId = parsed[1];

        log.info("run orchestrator: sessionId={} model={}/{} agentId={}",
                sessionId, provider, modelId, params.getAgentId());

        // Register active run
        AtomicBoolean cancelled = new AtomicBoolean(false);
        AtomicBoolean streaming = new AtomicBoolean(false);
        AtomicBoolean compacting = new AtomicBoolean(false);
        AtomicReference<AgentRunner.AgentRunContext> contextRef = new AtomicReference<>();

        ActiveRunRegistry.RunHandle handle = new ActiveRunRegistry.RunHandle() {
            @Override
            public CompletableFuture<Void> queueMessage(String text) {
                // TODO: implement multi-turn queue
                log.debug("queueMessage not yet implemented: sessionId={}", sessionId);
                return CompletableFuture.completedFuture(null);
            }

            @Override
            public boolean isStreaming() {
                return streaming.get();
            }

            @Override
            public boolean isCompacting() {
                return compacting.get();
            }

            @Override
            public void abort() {
                cancelled.set(true);
                AgentRunner.AgentRunContext ctx = contextRef.get();
                if (ctx != null) {
                    ctx.setCancelled(true);
                }
            }
        };

        activeRunRegistry.setActiveRun(sessionId, handle);

        try {
            return executeWithFailover(params, provider, modelId,
                    cancelled, streaming, compacting, contextRef, startTime);
        } finally {
            activeRunRegistry.clearActiveRun(sessionId, handle);
        }
    }

    // ── Failover loop ─────────────────────────────────────────────────

    private RunResult executeWithFailover(
            RunParams params, String provider, String modelId,
            AtomicBoolean cancelled, AtomicBoolean streaming,
            AtomicBoolean compacting, AtomicReference<AgentRunner.AgentRunContext> contextRef,
            long startTime) {

        int attempt = 0;
        String currentAuthProfileId = params.getAuthProfileId();

        while (attempt < MAX_AUTH_PROFILE_RETRIES) {
            attempt++;
            if (cancelled.get()) {
                return buildAbortedResult(params.getSessionId(), provider, modelId, startTime);
            }

            try {
                // Scrub refusal magic from user prompt
                String userMessage = scrubAnthropicRefusalMagic(params.getUserMessage());

                // Build run context
                AgentRunner.AgentRunContext context = AgentRunner.AgentRunContext.builder()
                        .runId(java.util.UUID.randomUUID().toString())
                        .sessionKey(params.getSessionKey())
                        .agentId(params.getAgentId())
                        .modelId(provider + "/" + modelId)
                        .systemPrompt(params.getSystemPrompt())
                        .userMessage(userMessage)
                        .maxTurns(params.getMaxTurns() != null ? params.getMaxTurns() : 25)
                        .temperature(params.getTemperature())
                        .config(config)
                        .compactionEnabled(true)
                        .listener(params.getListener() != null ? params.getListener()
                                : AgentRunner.AgentRunContext.NOOP_LISTENER)
                        .build();

                contextRef.set(context);
                streaming.set(true);

                // Execute the agent loop
                AgentRunner.AgentResult agentResult = agentRunner.run(context);

                streaming.set(false);

                // Build result
                return buildRunResult(agentResult, params.getSessionId(),
                        provider, modelId, startTime);

            } catch (Exception e) {
                streaming.set(false);
                String errorMsg = e.getMessage() != null ? e.getMessage() : e.toString();

                ErrorClassifier.FailoverReason reason = ErrorClassifier.classifyFailoverReason(errorMsg);

                log.warn("run attempt {} failed: sessionId={} reason={} error={}",
                        attempt, params.getSessionId(), reason, errorMsg);

                // Check if this error is eligible for auth profile failover
                if (!ErrorClassifier.isFailoverEligible(reason)
                        || attempt >= MAX_AUTH_PROFILE_RETRIES) {
                    return buildErrorResult(params.getSessionId(), provider, modelId,
                            errorMsg, reason, startTime);
                }

                // Try next auth profile via profile ordering
                if (authProfileStore != null) {
                    List<String> orderedProfiles = authProfileStore.resolveProfileOrder(provider);
                    // Find next profile that isn't the current one
                    String nextProfile = null;
                    for (String key : orderedProfiles) {
                        if (!key.equals(currentAuthProfileId)) {
                            nextProfile = key;
                            break;
                        }
                    }
                    if (nextProfile != null) {
                        currentAuthProfileId = nextProfile;
                        log.info("failover: rotating to auth profile '{}' (attempt {})",
                                nextProfile, attempt + 1);
                        continue;
                    }
                }

                // No more profiles to try
                return buildErrorResult(params.getSessionId(), provider, modelId,
                        errorMsg, reason, startTime);
            }
        }

        return buildErrorResult(params.getSessionId(), provider, modelId,
                "Max auth profile retries exceeded",
                ErrorClassifier.FailoverReason.UNKNOWN, startTime);
    }

    // ── Result builders ───────────────────────────────────────────────

    private RunResult buildRunResult(AgentRunner.AgentResult agentResult,
            String sessionId, String provider, String modelId, long startTime) {

        long durationMs = System.currentTimeMillis() - startTime;

        AgentMeta agentMeta = AgentMeta.builder()
                .sessionId(sessionId)
                .provider(provider)
                .model(modelId)
                .build();

        // Extract usage from agent result events
        for (AgentRunner.AgentEvent event : agentResult.getEvents()) {
            if (event.getUsage() != null) {
                agentMeta.setInputTokens(
                        agentMeta.getInputTokens() + event.getUsage().getInputTokens());
                agentMeta.setOutputTokens(
                        agentMeta.getOutputTokens() + event.getUsage().getOutputTokens());
            }
        }

        RunMeta meta = RunMeta.builder()
                .durationMs(durationMs)
                .agentMeta(agentMeta)
                .stopReason(agentResult.isSuccess() ? "completed" : "error")
                .build();

        List<Payload> payloads = new ArrayList<>();
        if (agentResult.getFinalMessage() != null && !agentResult.getFinalMessage().isBlank()) {
            payloads.add(Payload.builder()
                    .text(agentResult.getFinalMessage())
                    .build());
        }

        return RunResult.builder()
                .payloads(payloads)
                .meta(meta)
                .build();
    }

    private RunResult buildAbortedResult(String sessionId, String provider,
            String modelId, long startTime) {
        return RunResult.builder()
                .meta(RunMeta.builder()
                        .durationMs(System.currentTimeMillis() - startTime)
                        .aborted(true)
                        .agentMeta(AgentMeta.builder()
                                .sessionId(sessionId)
                                .provider(provider)
                                .model(modelId)
                                .build())
                        .build())
                .build();
    }

    private RunResult buildErrorResult(String sessionId, String provider,
            String modelId, String errorMessage,
            ErrorClassifier.FailoverReason reason, long startTime) {

        ErrorKind errorKind = switch (reason) {
            case CONTEXT_OVERFLOW -> ErrorKind.CONTEXT_OVERFLOW;
            default -> null;
        };

        String userMessage = ErrorClassifier.formatAssistantErrorText(errorMessage);

        RunMeta.RunMetaBuilder metaBuilder = RunMeta.builder()
                .durationMs(System.currentTimeMillis() - startTime)
                .agentMeta(AgentMeta.builder()
                        .sessionId(sessionId)
                        .provider(provider)
                        .model(modelId)
                        .build())
                .stopReason("error");

        if (errorKind != null) {
            metaBuilder.error(RunError.builder()
                    .kind(errorKind)
                    .message(errorMessage)
                    .build());
        }

        List<Payload> payloads = new ArrayList<>();
        if (userMessage != null) {
            payloads.add(Payload.builder()
                    .text(userMessage)
                    .isError(true)
                    .build());
        }

        return RunResult.builder()
                .payloads(payloads)
                .meta(metaBuilder.build())
                .build();
    }

    // ── Helpers ───────────────────────────────────────────────────────

    /**
     * Parse a model spec like "anthropic/claude-sonnet-4-20250514" into [provider,
     * modelId].
     */
    static String[] parseModelSpec(String model) {
        if (model == null || model.isBlank()) {
            return new String[] { "anthropic", "claude-sonnet-4-20250514" };
        }
        String[] parts = PROVIDER_SEPARATOR.split(model, 2);
        if (parts.length == 2) {
            return parts;
        }
        // No separator — assume anthropic
        return new String[] { "anthropic", model };
    }

    /**
     * Scrub Anthropic's refusal test token poisoning from prompt text.
     */
    static String scrubAnthropicRefusalMagic(String prompt) {
        if (prompt == null || !prompt.contains(ANTHROPIC_REFUSAL_MAGIC)) {
            return prompt;
        }
        return prompt.replace(ANTHROPIC_REFUSAL_MAGIC, ANTHROPIC_REFUSAL_REPLACEMENT);
    }
}
