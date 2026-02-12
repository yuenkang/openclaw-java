package com.openclaw.agent.models;

import java.util.*;

/**
 * Synthetic provider model catalog — model definitions for the
 * Synthetic API endpoint (MiniMax, DeepSeek, Qwen, GLM, Llama, etc).
 * Mirrors {@code agents/synthetic-models.ts}.
 */
public final class SyntheticModels {

    private SyntheticModels() {
    }

    public static final String SYNTHETIC_BASE_URL = "https://api.synthetic.new/anthropic";
    public static final String SYNTHETIC_DEFAULT_MODEL_ID = "hf:MiniMaxAI/MiniMax-M2.1";
    public static final String SYNTHETIC_DEFAULT_MODEL_REF = "synthetic/" + SYNTHETIC_DEFAULT_MODEL_ID;

    // --- Model catalog entry ---

    public record CatalogEntry(
            String id,
            String name,
            boolean reasoning,
            List<String> input,
            int contextWindow,
            int maxTokens) {
    }

    public record ModelCost(double input, double output, double cacheRead, double cacheWrite) {
    }

    public static final ModelCost DEFAULT_COST = new ModelCost(0, 0, 0, 0);

    // --- Catalog ---

    public static final List<CatalogEntry> CATALOG = List.of(
            entry("hf:MiniMaxAI/MiniMax-M2.1", "MiniMax M2.1", false, 192000, 65536),
            entry("hf:moonshotai/Kimi-K2-Thinking", "Kimi K2 Thinking", true, 256000, 8192),
            entry("hf:zai-org/GLM-4.7", "GLM-4.7", false, 198000, 128000),
            entry("hf:deepseek-ai/DeepSeek-R1-0528", "DeepSeek R1 0528", false, 128000, 8192),
            entry("hf:deepseek-ai/DeepSeek-V3-0324", "DeepSeek V3 0324", false, 128000, 8192),
            entry("hf:deepseek-ai/DeepSeek-V3.1", "DeepSeek V3.1", false, 128000, 8192),
            entry("hf:deepseek-ai/DeepSeek-V3.1-Terminus", "DeepSeek V3.1 Terminus", false, 128000, 8192),
            entry("hf:deepseek-ai/DeepSeek-V3.2", "DeepSeek V3.2", false, 159000, 8192),
            entry("hf:meta-llama/Llama-3.3-70B-Instruct", "Llama 3.3 70B Instruct", false, 128000, 8192),
            entry("hf:meta-llama/Llama-4-Maverick-17B-128E-Instruct-FP8",
                    "Llama 4 Maverick 17B 128E Instruct FP8", false, 524000, 8192),
            entry("hf:moonshotai/Kimi-K2-Instruct-0905", "Kimi K2 Instruct 0905", false, 256000, 8192),
            entry("hf:moonshotai/Kimi-K2.5", "Kimi K2.5", true, 256000, 8192),
            entry("hf:openai/gpt-oss-120b", "GPT OSS 120B", false, 128000, 8192),
            entry("hf:Qwen/Qwen3-235B-A22B-Instruct-2507",
                    "Qwen3 235B A22B Instruct 2507", false, 256000, 8192),
            entry("hf:Qwen/Qwen3-Coder-480B-A35B-Instruct",
                    "Qwen3 Coder 480B A35B Instruct", false, 256000, 8192),
            entryWithImage("hf:Qwen/Qwen3-VL-235B-A22B-Instruct",
                    "Qwen3 VL 235B A22B Instruct", false, 250000, 8192),
            entry("hf:zai-org/GLM-4.5", "GLM-4.5", false, 128000, 128000),
            entry("hf:zai-org/GLM-4.6", "GLM-4.6", false, 198000, 128000),
            entry("hf:deepseek-ai/DeepSeek-V3", "DeepSeek V3", false, 128000, 8192),
            entry("hf:Qwen/Qwen3-235B-A22B-Thinking-2507",
                    "Qwen3 235B A22B Thinking 2507", true, 256000, 8192));

    /**
     * Build a model definition map from a catalog entry, suitable for
     * inclusion in a models.json provider config.
     */
    public static Map<String, Object> buildModelDefinition(CatalogEntry entry) {
        Map<String, Object> def = new LinkedHashMap<>();
        def.put("id", entry.id());
        def.put("name", entry.name());
        def.put("reasoning", entry.reasoning());
        def.put("input", new ArrayList<>(entry.input()));
        def.put("cost", Map.of(
                "input", DEFAULT_COST.input(),
                "output", DEFAULT_COST.output(),
                "cacheRead", DEFAULT_COST.cacheRead(),
                "cacheWrite", DEFAULT_COST.cacheWrite()));
        def.put("contextWindow", entry.contextWindow());
        def.put("maxTokens", entry.maxTokens());
        return def;
    }

    // --- Helpers ---

    private static CatalogEntry entry(String id, String name, boolean reasoning,
            int contextWindow, int maxTokens) {
        return new CatalogEntry(id, name, reasoning, List.of("text"), contextWindow, maxTokens);
    }

    private static CatalogEntry entryWithImage(String id, String name, boolean reasoning,
            int contextWindow, int maxTokens) {
        return new CatalogEntry(id, name, reasoning, List.of("text", "image"),
                contextWindow, maxTokens);
    }
}
