package com.openclaw.agent.runtime;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;

/**
 * Resolve provider-specific extra parameters (temperature, maxTokens, cache
 * control).
 * Corresponds to TypeScript pi-embedded-runner/extra-params.ts.
 */
@Slf4j
public final class ExtraParams {

    private static final Map<String, String> OPENROUTER_APP_HEADERS = Map.of(
            "HTTP-Referer", "https://openclaw.ai",
            "X-Title", "OpenClaw");

    private ExtraParams() {
    }

    /**
     * Resolve extra params from model-level config.
     * Looks up model-specific params via the config's model map.
     *
     * @param cfg      OpenClaw config
     * @param provider LLM provider id (e.g. "anthropic", "openai")
     * @param modelId  Model id (e.g. "claude-sonnet-4-20250514")
     * @return Params map or null if none configured
     */
    public static Map<String, Object> resolveExtraParams(
            OpenClawConfig cfg, String provider, String modelId) {
        if (cfg == null || provider == null || modelId == null)
            return null;

        // TODO: Once OpenClawConfig.AgentDefaults has a models map, use it here
        // String modelKey = provider + "/" + modelId;
        // var modelConfig = cfg.getAgents().getDefaults().getModels().get(modelKey);
        // return modelConfig != null ? modelConfig.getParams() : null;

        log.debug("ExtraParams.resolveExtraParams: stub for {}/{}", provider, modelId);
        return null;
    }

    /**
     * Cache retention mode for Anthropic provider.
     */
    public enum CacheRetention {
        NONE, SHORT, LONG
    }

    /**
     * Resolve cache retention from extra params.
     * Supports both new {@code cacheRetention} and legacy {@code cacheControlTtl}.
     * Only applies to Anthropic provider.
     */
    public static CacheRetention resolveCacheRetention(
            Map<String, Object> extraParams, String provider) {
        if (!"anthropic".equals(provider))
            return null;
        if (extraParams == null)
            return null;

        // Prefer new cacheRetention
        Object newVal = extraParams.get("cacheRetention");
        if (newVal instanceof String s) {
            return switch (s) {
                case "none" -> CacheRetention.NONE;
                case "short" -> CacheRetention.SHORT;
                case "long" -> CacheRetention.LONG;
                default -> null;
            };
        }

        // Legacy cacheControlTtl
        Object legacy = extraParams.get("cacheControlTtl");
        if ("5m".equals(legacy))
            return CacheRetention.SHORT;
        if ("1h".equals(legacy))
            return CacheRetention.LONG;

        return null;
    }

    /**
     * Resolved stream parameters for the LLM call.
     */
    public record StreamParams(
            Double temperature,
            Integer maxTokens,
            CacheRetention cacheRetention) {
        public boolean isEmpty() {
            return temperature == null && maxTokens == null && cacheRetention == null;
        }
    }

    /**
     * Build stream params from extra params map.
     */
    public static StreamParams buildStreamParams(
            Map<String, Object> extraParams, String provider) {
        if (extraParams == null || extraParams.isEmpty())
            return null;

        Double temperature = null;
        if (extraParams.get("temperature") instanceof Number n) {
            temperature = n.doubleValue();
        }

        Integer maxTokens = null;
        if (extraParams.get("maxTokens") instanceof Number n) {
            maxTokens = n.intValue();
        }

        CacheRetention cacheRetention = resolveCacheRetention(extraParams, provider);

        StreamParams sp = new StreamParams(temperature, maxTokens, cacheRetention);
        return sp.isEmpty() ? null : sp;
    }

    /**
     * Merge config-level extra params with any runtime overrides.
     * Override entries with null values are filtered out.
     */
    public static Map<String, Object> mergeExtraParams(
            Map<String, Object> base, Map<String, Object> override) {
        Map<String, Object> merged = new HashMap<>();
        if (base != null)
            merged.putAll(base);
        if (override != null) {
            override.forEach((k, v) -> {
                if (v != null)
                    merged.put(k, v);
            });
        }
        return merged.isEmpty() ? null : merged;
    }

    /**
     * Check if provider is OpenRouter (requires app attribution headers).
     */
    public static boolean isOpenRouter(String provider) {
        return "openrouter".equals(provider);
    }

    /**
     * Get OpenRouter app attribution headers.
     */
    public static Map<String, String> getOpenRouterHeaders() {
        return OPENROUTER_APP_HEADERS;
    }
}
