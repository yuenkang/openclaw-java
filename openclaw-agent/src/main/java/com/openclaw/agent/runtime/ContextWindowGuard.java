package com.openclaw.agent.runtime;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

/**
 * Context window guard — prevents runs from exceeding the model's context
 * limit.
 * Corresponds to TypeScript's context-window-guard.ts.
 */
@Slf4j
public class ContextWindowGuard {

    public static final int HARD_MIN_TOKENS = 16_000;
    public static final int WARN_BELOW_TOKENS = 32_000;

    public record ContextWindowInfo(int tokens, String source) {
    }

    public record GuardResult(int tokens, String source, boolean shouldWarn, boolean shouldBlock) {
    }

    /**
     * Resolve the context window size from config, model metadata, or defaults.
     *
     * @param cfg                the config (nullable)
     * @param provider           the model provider id
     * @param modelId            the model id
     * @param modelContextWindow context window reported by the model itself (0 if
     *                           unknown)
     * @param defaultTokens      fallback default
     */
    public static ContextWindowInfo resolveContextWindow(
            OpenClawConfig cfg,
            String provider,
            String modelId,
            int modelContextWindow,
            int defaultTokens) {

        // 1. Check per-model override in
        // models.providers.<provider>.models[].contextWindow
        Integer fromModelsConfig = resolveFromModelsConfig(cfg, provider, modelId);
        if (fromModelsConfig != null && fromModelsConfig > 0) {
            return applyCap(cfg, new ContextWindowInfo(fromModelsConfig, "modelsConfig"));
        }

        // 2. Use model-reported value
        if (modelContextWindow > 0) {
            return applyCap(cfg, new ContextWindowInfo(modelContextWindow, "model"));
        }

        // 3. Fall back to default
        return applyCap(cfg, new ContextWindowInfo(Math.max(1, defaultTokens), "default"));
    }

    /**
     * Evaluate whether the resolved context window should trigger a warning or
     * block.
     */
    public static GuardResult evaluate(ContextWindowInfo info) {
        return evaluate(info, WARN_BELOW_TOKENS, HARD_MIN_TOKENS);
    }

    public static GuardResult evaluate(ContextWindowInfo info, int warnBelowTokens, int hardMinTokens) {
        int warnBelow = Math.max(1, warnBelowTokens);
        int hardMin = Math.max(1, hardMinTokens);
        int tokens = Math.max(0, info.tokens());

        boolean shouldWarn = tokens > 0 && tokens < warnBelow;
        boolean shouldBlock = tokens > 0 && tokens < hardMin;

        return new GuardResult(tokens, info.source(), shouldWarn, shouldBlock);
    }

    // --- Private helpers ---

    private static Integer resolveFromModelsConfig(OpenClawConfig cfg, String provider, String modelId) {
        if (cfg == null || cfg.getModels() == null || cfg.getModels().getProviders() == null) {
            return null;
        }
        var providerConfig = cfg.getModels().getProviders().get(provider);
        if (providerConfig == null || providerConfig.getModels() == null) {
            return null;
        }
        for (var model : providerConfig.getModels()) {
            if (modelId.equals(model.getId()) && model.getContextWindow() != null) {
                return model.getContextWindow();
            }
        }
        return null;
    }

    private static ContextWindowInfo applyCap(OpenClawConfig cfg, ContextWindowInfo base) {
        if (cfg == null || cfg.getAgents() == null || cfg.getAgents().getDefaults() == null) {
            return base;
        }
        Integer capTokens = cfg.getAgents().getDefaults().getContextTokens();
        if (capTokens != null && capTokens > 0 && capTokens < base.tokens()) {
            return new ContextWindowInfo(capTokens, "agentContextTokens");
        }
        return base;
    }
}
