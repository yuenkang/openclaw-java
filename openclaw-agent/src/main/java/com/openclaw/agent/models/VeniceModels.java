package com.openclaw.agent.models;

import com.openclaw.common.config.OpenClawConfig.ModelCost;
import com.openclaw.common.config.OpenClawConfig.ModelDefinition;

import java.util.ArrayList;
import java.util.List;

/**
 * Venice AI model catalog with static fallback definitions.
 * Mirrors {@code agents/venice-models.ts}.
 */
public final class VeniceModels {

    private VeniceModels() {
    }

    public static final String VENICE_BASE_URL = "https://api.venice.ai/api/v1";
    public static final String VENICE_DEFAULT_MODEL_ID = "llama-3.3-70b";
    public static final String VENICE_DEFAULT_MODEL_REF = "venice/" + VENICE_DEFAULT_MODEL_ID;

    /**
     * Build a ModelDefinition for a Venice catalog entry.
     */
    public static ModelDefinition buildVeniceModelDefinition(CatalogEntry entry) {
        ModelDefinition def = new ModelDefinition();
        def.setId(entry.id());
        def.setName(entry.name());
        def.setReasoning(entry.reasoning());
        def.setInput(new ArrayList<>(entry.input()));
        def.setContextWindow(entry.contextWindow());
        def.setMaxTokens(entry.maxTokens());
        // Venice uses credit-based pricing – set to 0
        ModelCost cost = new ModelCost();
        cost.setInput(0);
        cost.setOutput(0);
        cost.setCacheRead(0);
        cost.setCacheWrite(0);
        def.setCost(cost);
        return def;
    }

    /** Get static fallback catalog as ModelDefinitions. */
    public static List<ModelDefinition> getStaticCatalog() {
        List<ModelDefinition> result = new ArrayList<>();
        for (CatalogEntry entry : CATALOG) {
            result.add(buildVeniceModelDefinition(entry));
        }
        return result;
    }

    // --- Catalog Entry ---

    public record CatalogEntry(
            String id, String name, boolean reasoning,
            List<String> input, int contextWindow, int maxTokens, String privacy) {
    }

    // --- Static catalog ---

    public static final List<CatalogEntry> CATALOG = List.of(
            // Private models
            new CatalogEntry("llama-3.3-70b", "Llama 3.3 70B", false,
                    List.of("text"), 131072, 8192, "private"),
            new CatalogEntry("llama-3.2-3b", "Llama 3.2 3B", false,
                    List.of("text"), 131072, 8192, "private"),
            new CatalogEntry("hermes-3-llama-3.1-405b", "Hermes 3 Llama 3.1 405B", false,
                    List.of("text"), 131072, 8192, "private"),
            new CatalogEntry("qwen3-235b-a22b-thinking-2507", "Qwen3 235B Thinking", true,
                    List.of("text"), 131072, 8192, "private"),
            new CatalogEntry("qwen3-235b-a22b-instruct-2507", "Qwen3 235B Instruct", false,
                    List.of("text"), 131072, 8192, "private"),
            new CatalogEntry("qwen3-coder-480b-a35b-instruct", "Qwen3 Coder 480B", false,
                    List.of("text"), 262144, 8192, "private"),
            new CatalogEntry("qwen3-next-80b", "Qwen3 Next 80B", false,
                    List.of("text"), 262144, 8192, "private"),
            new CatalogEntry("qwen3-vl-235b-a22b", "Qwen3 VL 235B (Vision)", false,
                    List.of("text", "image"), 262144, 8192, "private"),
            new CatalogEntry("qwen3-4b", "Venice Small (Qwen3 4B)", true,
                    List.of("text"), 32768, 8192, "private"),
            new CatalogEntry("deepseek-v3.2", "DeepSeek V3.2", true,
                    List.of("text"), 163840, 8192, "private"),
            new CatalogEntry("venice-uncensored", "Venice Uncensored (Dolphin-Mistral)", false,
                    List.of("text"), 32768, 8192, "private"),
            new CatalogEntry("mistral-31-24b", "Venice Medium (Mistral)", false,
                    List.of("text", "image"), 131072, 8192, "private"),
            new CatalogEntry("google-gemma-3-27b-it", "Google Gemma 3 27B Instruct", false,
                    List.of("text", "image"), 202752, 8192, "private"),
            new CatalogEntry("openai-gpt-oss-120b", "OpenAI GPT OSS 120B", false,
                    List.of("text"), 131072, 8192, "private"),
            new CatalogEntry("zai-org-glm-4.7", "GLM 4.7", true,
                    List.of("text"), 202752, 8192, "private"),
            // Anonymized models
            new CatalogEntry("claude-opus-45", "Claude Opus 4.5 (via Venice)", true,
                    List.of("text", "image"), 202752, 8192, "anonymized"),
            new CatalogEntry("claude-sonnet-45", "Claude Sonnet 4.5 (via Venice)", true,
                    List.of("text", "image"), 202752, 8192, "anonymized"),
            new CatalogEntry("openai-gpt-52", "GPT-5.2 (via Venice)", true,
                    List.of("text"), 262144, 8192, "anonymized"),
            new CatalogEntry("openai-gpt-52-codex", "GPT-5.2 Codex (via Venice)", true,
                    List.of("text", "image"), 262144, 8192, "anonymized"),
            new CatalogEntry("gemini-3-pro-preview", "Gemini 3 Pro (via Venice)", true,
                    List.of("text", "image"), 202752, 8192, "anonymized"),
            new CatalogEntry("gemini-3-flash-preview", "Gemini 3 Flash (via Venice)", true,
                    List.of("text", "image"), 262144, 8192, "anonymized"),
            new CatalogEntry("grok-41-fast", "Grok 4.1 Fast (via Venice)", true,
                    List.of("text", "image"), 262144, 8192, "anonymized"),
            new CatalogEntry("grok-code-fast-1", "Grok Code Fast 1 (via Venice)", true,
                    List.of("text"), 262144, 8192, "anonymized"),
            new CatalogEntry("kimi-k2-thinking", "Kimi K2 Thinking (via Venice)", true,
                    List.of("text"), 262144, 8192, "anonymized"),
            new CatalogEntry("minimax-m21", "MiniMax M2.1 (via Venice)", true,
                    List.of("text"), 202752, 8192, "anonymized"));
}
