package com.openclaw.agent.models;

import com.openclaw.common.config.OpenClawConfig.ModelCost;
import com.openclaw.common.config.OpenClawConfig.ModelDefinition;

import java.util.*;

/**
 * OpenCode Zen model catalog with aliases, static fallback, and cost data.
 * Mirrors {@code agents/opencode-zen-models.ts}.
 */
public final class OpencodeZenModels {

    private OpencodeZenModels() {
    }

    public static final String API_BASE_URL = "https://opencode.ai/zen/v1";
    public static final String DEFAULT_MODEL = "claude-opus-4-6";
    public static final String DEFAULT_MODEL_REF = "opencode/" + DEFAULT_MODEL;

    // --- Aliases ---

    private static final Map<String, String> ALIASES;
    static {
        Map<String, String> m = new LinkedHashMap<>();
        // Claude aliases
        m.put("opus", "claude-opus-4-6");
        m.put("opus-4.6", "claude-opus-4-6");
        m.put("opus-4.5", "claude-opus-4-5");
        m.put("opus-4", "claude-opus-4-6");
        m.put("sonnet", "claude-opus-4-6");
        m.put("sonnet-4", "claude-opus-4-6");
        m.put("haiku", "claude-opus-4-6");
        m.put("haiku-3.5", "claude-opus-4-6");
        // GPT aliases
        m.put("gpt5", "gpt-5.2");
        m.put("gpt-5", "gpt-5.2");
        m.put("gpt-5.1", "gpt-5.1");
        m.put("gpt4", "gpt-5.1");
        m.put("gpt-4", "gpt-5.1");
        m.put("gpt-mini", "gpt-5.1-codex-mini");
        m.put("o1", "gpt-5.2");
        m.put("o3", "gpt-5.2");
        m.put("o3-mini", "gpt-5.1-codex-mini");
        // Codex aliases
        m.put("codex", "gpt-5.1-codex");
        m.put("codex-mini", "gpt-5.1-codex-mini");
        m.put("codex-max", "gpt-5.1-codex-max");
        // Gemini aliases
        m.put("gemini", "gemini-3-pro");
        m.put("gemini-pro", "gemini-3-pro");
        m.put("gemini-3", "gemini-3-pro");
        m.put("flash", "gemini-3-flash");
        m.put("gemini-flash", "gemini-3-flash");
        m.put("gemini-2.5", "gemini-3-pro");
        m.put("gemini-2.5-pro", "gemini-3-pro");
        m.put("gemini-2.5-flash", "gemini-3-flash");
        // GLM
        m.put("glm", "glm-4.7");
        m.put("glm-free", "glm-4.7");
        ALIASES = Collections.unmodifiableMap(m);
    }

    /** Resolve a model alias to its full model ID. */
    public static String resolveAlias(String modelIdOrAlias) {
        return ALIASES.getOrDefault(modelIdOrAlias.toLowerCase().trim(), modelIdOrAlias);
    }

    /** Resolve model API type based on model ID prefix. */
    public static String resolveModelApi(String modelId) {
        String lower = modelId.toLowerCase();
        if (lower.startsWith("gpt-"))
            return "openai-responses";
        if (lower.startsWith("claude-") || lower.startsWith("minimax-"))
            return "anthropic-messages";
        if (lower.startsWith("gemini-"))
            return "google-generative-ai";
        return "openai-completions";
    }

    // --- Costs, context windows, max tokens ---

    private static final Map<String, double[]> MODEL_COSTS = Map.of(
            "gpt-5.1-codex", new double[] { 1.07, 8.5, 0.107, 0 },
            "claude-opus-4-6", new double[] { 5, 25, 0.5, 6.25 },
            "claude-opus-4-5", new double[] { 5, 25, 0.5, 6.25 },
            "gemini-3-pro", new double[] { 2, 12, 0.2, 0 },
            "gpt-5.1-codex-mini", new double[] { 0.25, 2, 0.025, 0 },
            "gpt-5.1", new double[] { 1.07, 8.5, 0.107, 0 },
            "glm-4.7", new double[] { 0, 0, 0, 0 },
            "gemini-3-flash", new double[] { 0.5, 3, 0.05, 0 },
            "gpt-5.1-codex-max", new double[] { 1.25, 10, 0.125, 0 },
            "gpt-5.2", new double[] { 1.75, 14, 0.175, 0 });

    private static final Map<String, Integer> CTX_WINDOWS = Map.of(
            "gpt-5.1-codex", 400000,
            "claude-opus-4-6", 1000000,
            "claude-opus-4-5", 200000,
            "gemini-3-pro", 1048576,
            "gpt-5.1-codex-mini", 400000,
            "gpt-5.1", 400000,
            "glm-4.7", 204800,
            "gemini-3-flash", 1048576,
            "gpt-5.1-codex-max", 400000,
            "gpt-5.2", 400000);

    private static final Map<String, Integer> MAX_TOKENS = Map.of(
            "gpt-5.1-codex", 128000,
            "claude-opus-4-6", 128000,
            "claude-opus-4-5", 64000,
            "gemini-3-pro", 65536,
            "gpt-5.1-codex-mini", 128000,
            "gpt-5.1", 128000,
            "glm-4.7", 131072,
            "gemini-3-flash", 65536,
            "gpt-5.1-codex-max", 128000,
            "gpt-5.2", 128000);

    private static final Map<String, String> MODEL_NAMES = Map.of(
            "gpt-5.1-codex", "GPT-5.1 Codex",
            "claude-opus-4-6", "Claude Opus 4.6",
            "claude-opus-4-5", "Claude Opus 4.5",
            "gemini-3-pro", "Gemini 3 Pro",
            "gpt-5.1-codex-mini", "GPT-5.1 Codex Mini",
            "gpt-5.1", "GPT-5.1",
            "glm-4.7", "GLM-4.7",
            "gemini-3-flash", "Gemini 3 Flash",
            "gpt-5.1-codex-max", "GPT-5.1 Codex Max",
            "gpt-5.2", "GPT-5.2");

    private static final List<String> FALLBACK_IDS = List.of(
            "gpt-5.1-codex", "claude-opus-4-6", "claude-opus-4-5", "gemini-3-pro",
            "gpt-5.1-codex-mini", "gpt-5.1", "glm-4.7", "gemini-3-flash",
            "gpt-5.1-codex-max", "gpt-5.2");

    /** Check if model supports image input. */
    public static boolean supportsImageInput(String modelId) {
        String lower = modelId.toLowerCase();
        return !lower.contains("glm") && !lower.contains("minimax");
    }

    /** Format a model ID to a human-readable name. */
    public static String formatModelName(String modelId) {
        String name = MODEL_NAMES.get(modelId);
        if (name != null)
            return name;
        String[] parts = modelId.split("-");
        StringBuilder sb = new StringBuilder();
        for (String part : parts) {
            if (sb.length() > 0)
                sb.append(' ');
            sb.append(Character.toUpperCase(part.charAt(0))).append(part.substring(1));
        }
        return sb.toString();
    }

    /** Build a ModelDefinition from a model ID. */
    public static ModelDefinition buildModelDefinition(String modelId) {
        ModelDefinition def = new ModelDefinition();
        def.setId(modelId);
        def.setName(formatModelName(modelId));
        def.setReasoning(true); // Zen models default to reasoning-capable
        def.setInput(supportsImageInput(modelId)
                ? new ArrayList<>(List.of("text", "image"))
                : new ArrayList<>(List.of("text")));
        double[] costs = MODEL_COSTS.getOrDefault(modelId, new double[] { 0, 0, 0, 0 });
        ModelCost cost = new ModelCost();
        cost.setInput(costs[0]);
        cost.setOutput(costs[1]);
        cost.setCacheRead(costs[2]);
        cost.setCacheWrite(costs[3]);
        def.setCost(cost);
        def.setContextWindow(CTX_WINDOWS.getOrDefault(modelId, 128000));
        def.setMaxTokens(MAX_TOKENS.getOrDefault(modelId, 8192));
        return def;
    }

    /** Get static fallback model definitions. */
    public static List<ModelDefinition> getStaticFallbackModels() {
        List<ModelDefinition> result = new ArrayList<>();
        for (String id : FALLBACK_IDS) {
            result.add(buildModelDefinition(id));
        }
        return result;
    }
}
