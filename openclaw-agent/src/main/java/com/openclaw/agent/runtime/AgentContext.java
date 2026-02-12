package com.openclaw.agent.runtime;

import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Model context window lookup cache.
 * Corresponds to TypeScript agents/context.ts.
 *
 * <p>
 * Lazy-loads model metadata to infer context windows when the agent reports a
 * model id.
 * </p>
 */
@Slf4j
public final class AgentContext {

    private static final Map<String, Integer> MODEL_CACHE = new ConcurrentHashMap<>();

    private AgentContext() {
    }

    /**
     * Register known model context windows.
     */
    public static void registerModelContextWindow(String modelId, int contextWindow) {
        if (modelId != null && !modelId.isBlank() && contextWindow > 0) {
            MODEL_CACHE.put(modelId, contextWindow);
        }
    }

    /**
     * Look up the context window token count for a model.
     *
     * @param modelId Model identifier
     * @return Context window tokens or null if unknown
     */
    public static Integer lookupContextTokens(String modelId) {
        if (modelId == null || modelId.isBlank())
            return null;
        return MODEL_CACHE.get(modelId);
    }

    /**
     * Clear cache (for testing).
     */
    public static void clearCache() {
        MODEL_CACHE.clear();
    }
}
