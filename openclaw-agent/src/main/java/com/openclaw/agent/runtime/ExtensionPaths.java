package com.openclaw.agent.runtime;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

/**
 * Agent extension path resolution (context pruning, compaction safeguard).
 * Corresponds to TypeScript pi-embedded-runner/extensions.ts.
 */
@Slf4j
public final class ExtensionPaths {

    /** Default context window token count. */
    public static final int DEFAULT_CONTEXT_TOKENS = 200_000;

    private ExtensionPaths() {
    }

    /**
     * Resolve the effective context window token limit.
     *
     * @param cfg                OpenClaw config
     * @param provider           LLM provider id
     * @param modelId            Model id
     * @param modelContextWindow Model-reported context window (may be null)
     * @return Effective token limit
     */
    public static int resolveContextWindowTokens(
            OpenClawConfig cfg, String provider, String modelId,
            Integer modelContextWindow) {

        // TODO: Check config-level model override once models map is in config
        // String key = provider + "/" + modelId;
        // var modelConfig = cfg.getAgents().getDefaults().getModels().get(key);
        // if (modelConfig != null && modelConfig.contextWindow != null)
        // return modelConfig.contextWindow;

        // Use model-reported context window
        if (modelContextWindow != null && modelContextWindow > 0) {
            return modelContextWindow;
        }

        return DEFAULT_CONTEXT_TOKENS;
    }

    /**
     * Resolve compaction mode from config.
     */
    public static String resolveCompactionMode(OpenClawConfig cfg) {
        // TODO: implement once compaction config is in OpenClawConfig
        return "default";
    }

    /**
     * Check if a provider is eligible for cache-TTL based context pruning.
     */
    static boolean isCacheTtlEligibleProvider(String provider) {
        return "anthropic".equals(provider);
    }

    /**
     * Build extension configuration for the embedded runner.
     *
     * @param cfg                OpenClaw config
     * @param provider           LLM provider id
     * @param modelId            Model id
     * @param modelContextWindow Model-reported context window
     * @return Extension info with compaction mode and context tokens
     */
    public static ExtensionInfo buildExtensionInfo(
            OpenClawConfig cfg, String provider, String modelId,
            Integer modelContextWindow) {

        String compactionMode = resolveCompactionMode(cfg);
        int contextTokens = resolveContextWindowTokens(cfg, provider, modelId, modelContextWindow);
        List<String> features = new ArrayList<>();

        if ("safeguard".equals(compactionMode)) {
            features.add("compaction-safeguard");
            log.debug("Extension: compaction safeguard enabled, contextTokens={}", contextTokens);
        }

        // TODO: context pruning check once config is available

        return new ExtensionInfo(compactionMode, contextTokens, features);
    }

    /**
     * Extension configuration result.
     */
    public record ExtensionInfo(
            String compactionMode,
            int contextWindowTokens,
            List<String> enabledFeatures) {
    }
}
