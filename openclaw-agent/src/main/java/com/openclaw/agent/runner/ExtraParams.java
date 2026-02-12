package com.openclaw.agent.runner;

import java.util.*;

/**
 * Provider-specific extra parameters (e.g., temperature, maxTokens,
 * cacheRetention)
 * and OpenRouter header attribution.
 * Mirrors {@code agents/pi-embedded-runner/extra-params.ts}.
 */
public final class ExtraParams {

    private ExtraParams() {
    }

    private static final Map<String, String> OPENROUTER_APP_HEADERS = Map.of(
            "HTTP-Referer", "https://openclaw.ai",
            "X-Title", "OpenClaw");

    /**
     * Resolve extra params from model config.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> resolveExtraParams(Map<String, Object> config,
            String provider, String modelId) {
        if (config == null)
            return null;
        Object agentsObj = config.get("agents");
        if (!(agentsObj instanceof Map<?, ?> agents))
            return null;
        Object defaultsObj = agents.get("defaults");
        if (!(defaultsObj instanceof Map<?, ?> defaults))
            return null;
        Object modelsObj = defaults.get("models");
        if (!(modelsObj instanceof Map<?, ?> models))
            return null;

        String modelKey = provider + "/" + modelId;
        Object modelConfig = models.get(modelKey);
        if (!(modelConfig instanceof Map<?, ?> mc))
            return null;
        Object params = mc.get("params");
        if (!(params instanceof Map<?, ?> p))
            return null;
        return new LinkedHashMap<>((Map<String, Object>) p);
    }

    /** Cache retention values for Anthropic prompt caching. */
    public enum CacheRetention {
        NONE, SHORT, LONG;

        public String toValue() {
            return name().toLowerCase();
        }
    }

    /**
     * Resolve cacheRetention from extra params.
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

        // Fall back to legacy cacheControlTtl
        Object legacy = extraParams.get("cacheControlTtl");
        if ("5m".equals(legacy))
            return CacheRetention.SHORT;
        if ("1h".equals(legacy))
            return CacheRetention.LONG;
        return null;
    }

    /**
     * Build stream parameters from extra params.
     *
     * @return map of stream params (temperature, maxTokens, cacheRetention),
     *         or null if no params to set
     */
    public static Map<String, Object> buildStreamParams(
            Map<String, Object> extraParams, String provider) {
        if (extraParams == null || extraParams.isEmpty())
            return null;

        Map<String, Object> streamParams = new LinkedHashMap<>();
        Object temp = extraParams.get("temperature");
        if (temp instanceof Number)
            streamParams.put("temperature", temp);
        Object maxTok = extraParams.get("maxTokens");
        if (maxTok instanceof Number)
            streamParams.put("maxTokens", maxTok);
        CacheRetention cr = resolveCacheRetention(extraParams, provider);
        if (cr != null)
            streamParams.put("cacheRetention", cr.toValue());

        return streamParams.isEmpty() ? null : streamParams;
    }

    /**
     * Apply extra params and OpenRouter headers to an agent configuration.
     */
    public static Map<String, Object> applyExtraParams(
            Map<String, Object> agentConfig,
            Map<String, Object> config,
            String provider,
            String modelId,
            Map<String, Object> extraParamsOverride) {
        Map<String, Object> extraParams = resolveExtraParams(config, provider, modelId);
        Map<String, Object> override = (extraParamsOverride != null && !extraParamsOverride.isEmpty())
                ? new LinkedHashMap<>(extraParamsOverride)
                : null;

        Map<String, Object> merged = new LinkedHashMap<>();
        if (extraParams != null)
            merged.putAll(extraParams);
        if (override != null) {
            override.entrySet().stream()
                    .filter(e -> e.getValue() != null)
                    .forEach(e -> merged.put(e.getKey(), e.getValue()));
        }

        Map<String, Object> streamParams = buildStreamParams(merged, provider);
        Map<String, Object> result = new LinkedHashMap<>(agentConfig != null ? agentConfig : Map.of());
        if (streamParams != null)
            result.put("streamParams", streamParams);

        if ("openrouter".equals(provider)) {
            result.put("openRouterHeaders", OPENROUTER_APP_HEADERS);
        }
        return result;
    }
}
