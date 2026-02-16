package com.openclaw.agent.providers;

import java.util.List;

/**
 * GitHub Copilot model catalog — default model IDs and definition builder.
 * Translates TS providers/github-copilot-models.ts.
 *
 */
public final class GitHubCopilotModels {

    private GitHubCopilotModels() {
    }

    public static final int DEFAULT_CONTEXT_WINDOW = 128_000;
    public static final int DEFAULT_MAX_TOKENS = 8192;

    /**
     * Default model IDs available through GitHub Copilot.
     * These may vary by plan/org and can change. If a model isn't available,
     * Copilot will return an error.
     */
    private static final List<String> DEFAULT_MODEL_IDS = List.of(
            "gpt-4o",
            "gpt-4.1",
            "gpt-4.1-mini",
            "gpt-4.1-nano",
            "o1",
            "o1-mini",
            "o3-mini");

    public static List<String> getDefaultModelIds() {
        return DEFAULT_MODEL_IDS;
    }

    /**
     * Model definition config for a Copilot model.
     */
    public record CopilotModelDefinition(
            String id,
            String name,
            String api,
            boolean reasoning,
            List<String> input,
            int contextWindow,
            int maxTokens) {
    }

    /**
     * Build a model definition for the given model ID.
     */
    public static CopilotModelDefinition buildModelDefinition(String modelId) {
        String id = modelId.trim();
        if (id.isEmpty()) {
            throw new IllegalArgumentException("Model id required");
        }
        return new CopilotModelDefinition(
                id, id,
                "openai-responses",
                false,
                List.of("text", "image"),
                DEFAULT_CONTEXT_WINDOW,
                DEFAULT_MAX_TOKENS);
    }
}
