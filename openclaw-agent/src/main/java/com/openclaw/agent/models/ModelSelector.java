package com.openclaw.agent.models;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Resolves model references from config, including aliases and provider/model
 * parsing.
 * Corresponds to TypeScript's model-selection.ts.
 */
@Slf4j
public class ModelSelector {

    public static final String DEFAULT_PROVIDER = "anthropic";
    public static final String DEFAULT_MODEL = "claude-opus-4-6";
    public static final int DEFAULT_CONTEXT_TOKENS = 200_000;

    /**
     * A model catalog entry with capability metadata.
     * Corresponds to TypeScript's ModelCatalogEntry.
     */
    public record ModelCatalogEntry(
            String id,
            String name,
            String provider,
            Integer contextWindow,
            Boolean reasoning,
            List<String> input) {
    }

    public record ModelRef(String provider, String model) {
        public String key() {
            return provider + "/" + model;
        }
    }

    /**
     * Parse a model reference string like "anthropic/claude-sonnet-4-20250514" or
     * just "claude-sonnet-4-20250514".
     *
     * @param raw             the raw model string
     * @param defaultProvider provider to use if none specified
     * @return parsed ref, or null if invalid
     */
    public static ModelRef parseModelRef(String raw, String defaultProvider) {
        if (raw == null || raw.isBlank())
            return null;
        String trimmed = raw.trim();
        int slash = trimmed.indexOf('/');
        if (slash == -1) {
            return new ModelRef(normalizeProviderId(defaultProvider), trimmed);
        }
        String provider = normalizeProviderId(trimmed.substring(0, slash).trim());
        String model = trimmed.substring(slash + 1).trim();
        if (provider.isEmpty() || model.isEmpty())
            return null;
        return new ModelRef(provider, model);
    }

    /**
     * Normalize provider ID (handle common aliases).
     */
    public static String normalizeProviderId(String provider) {
        if (provider == null)
            return "";
        String normalized = provider.trim().toLowerCase();
        return switch (normalized) {
            case "z.ai", "z-ai" -> "zai";
            case "opencode-zen" -> "opencode";
            case "qwen" -> "qwen-portal";
            case "kimi-code" -> "kimi-coding";
            default -> normalized;
        };
    }

    /**
     * Resolve the configured model from the OpenClaw config.
     * Checks agents.defaults.model.primary → config aliases → defaults.
     */
    public static ModelRef resolveConfiguredModel(OpenClawConfig cfg) {
        return resolveConfiguredModel(cfg, DEFAULT_PROVIDER, DEFAULT_MODEL);
    }

    public static ModelRef resolveConfiguredModel(OpenClawConfig cfg,
            String defaultProvider,
            String defaultModel) {
        if (cfg == null || cfg.getAgents() == null || cfg.getAgents().getDefaults() == null) {
            return new ModelRef(defaultProvider, defaultModel);
        }
        String rawModel = cfg.getAgents().getDefaults().getModel();
        if (rawModel == null || rawModel.isBlank()) {
            return new ModelRef(defaultProvider, defaultModel);
        }

        // Try alias resolution
        Map<String, String> aliases = resolveModelAliases(cfg, defaultProvider);
        String aliasKey = rawModel.trim().toLowerCase();
        if (aliases.containsKey(aliasKey)) {
            ModelRef aliasRef = parseModelRef(aliases.get(aliasKey), defaultProvider);
            if (aliasRef != null)
                return aliasRef;
        }

        // Direct parse
        ModelRef parsed = parseModelRef(rawModel, defaultProvider);
        return parsed != null ? parsed : new ModelRef(defaultProvider, defaultModel);
    }

    /**
     * Resolve the model for a specific agent, falling back to global defaults.
     */
    public static ModelRef resolveModelForAgent(OpenClawConfig cfg, String agentId) {
        if (cfg != null && cfg.getAgents() != null && cfg.getAgents().getEntries() != null && agentId != null) {
            for (var entry : cfg.getAgents().getEntries()) {
                if (agentId.equals(entry.getId()) && entry.getModel() != null && !entry.getModel().isBlank()) {
                    ModelRef parsed = parseModelRef(entry.getModel(), DEFAULT_PROVIDER);
                    if (parsed != null)
                        return parsed;
                }
            }
        }
        return resolveConfiguredModel(cfg);
    }

    /**
     * Build alias map from agents.defaults.models config section.
     */
    public static Map<String, String> resolveModelAliases(OpenClawConfig cfg, String defaultProvider) {
        Map<String, String> aliases = new HashMap<>();
        if (cfg == null || cfg.getAgents() == null || cfg.getAgents().getDefaults() == null) {
            return aliases;
        }
        Map<String, Object> models = cfg.getAgents().getDefaults().getModels();
        if (models == null)
            return aliases;

        for (Map.Entry<String, Object> entry : models.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();
            if (value instanceof Map) {
                @SuppressWarnings("unchecked")
                Map<String, Object> modelEntry = (Map<String, Object>) value;
                Object alias = modelEntry.get("alias");
                if (alias instanceof String && !((String) alias).isBlank()) {
                    aliases.put(((String) alias).trim().toLowerCase(), key);
                }
            }
        }
        return aliases;
    }

    /**
     * Build the list of fallback model candidates from config.
     * Returns primary model first, then configured fallbacks.
     */
    public static List<ModelRef> resolveFallbackCandidates(OpenClawConfig cfg,
            String provider,
            String model) {
        List<ModelRef> candidates = new ArrayList<>();
        Set<String> seen = new HashSet<>();

        // Primary
        ModelRef primary = new ModelRef(
                provider != null && !provider.isBlank() ? normalizeProviderId(provider) : DEFAULT_PROVIDER,
                model != null && !model.isBlank() ? model : DEFAULT_MODEL);
        candidates.add(primary);
        seen.add(primary.key());

        // Configured fallbacks from agents.defaults.model.fallbacks
        List<String> fallbacks = resolveFallbackList(cfg);
        for (String raw : fallbacks) {
            ModelRef ref = parseModelRef(raw, DEFAULT_PROVIDER);
            if (ref != null && !seen.contains(ref.key())) {
                candidates.add(ref);
                seen.add(ref.key());
            }
        }

        // Global configured primary as last resort
        ModelRef configPrimary = resolveConfiguredModel(cfg);
        if (!seen.contains(configPrimary.key())) {
            candidates.add(configPrimary);
        }

        return candidates;
    }

    /**
     * Extract the fallback model list from config.
     */
    private static List<String> resolveFallbackList(OpenClawConfig cfg) {
        if (cfg == null || cfg.getAgents() == null || cfg.getAgents().getDefaults() == null) {
            return Collections.emptyList();
        }
        List<String> fallbacks = cfg.getAgents().getDefaults().getModelFallbacks();
        return fallbacks != null ? fallbacks : Collections.emptyList();
    }

    // =========================================================================
    // Catalog + Vision + Allowlist + Thinking
    // =========================================================================

    /**
     * Check if a model supports image/vision input based on its catalog entry.
     */
    public static boolean modelSupportsVision(ModelCatalogEntry entry) {
        return entry != null && entry.input() != null && entry.input().contains("image");
    }

    /**
     * Find a model in the catalog by provider and model ID (case-insensitive).
     */
    public static ModelCatalogEntry findModelInCatalog(
            List<ModelCatalogEntry> catalog, String provider, String modelId) {
        if (catalog == null || provider == null || modelId == null)
            return null;
        String normProvider = provider.toLowerCase().trim();
        String normModel = modelId.toLowerCase().trim();
        return catalog.stream()
                .filter(e -> e.provider().toLowerCase().equals(normProvider)
                        && e.id().toLowerCase().equals(normModel))
                .findFirst()
                .orElse(null);
    }

    /**
     * Resolve the default thinking level for a model.
     * Checks config thinkingDefault, then uses "low" for reasoning models.
     */
    public static String resolveThinkingDefault(
            OpenClawConfig cfg, String provider, String model, List<ModelCatalogEntry> catalog) {
        // Check explicit config
        if (cfg != null && cfg.getAgents() != null && cfg.getAgents().getDefaults() != null) {
            String explicit = cfg.getAgents().getDefaults().getThinkingDefault();
            if (explicit != null && !explicit.isBlank()) {
                return explicit.trim().toLowerCase();
            }
        }
        // Default to "low" for reasoning-capable models, "off" otherwise
        if (catalog != null) {
            ModelCatalogEntry entry = findModelInCatalog(catalog, provider, model);
            if (entry != null && Boolean.TRUE.equals(entry.reasoning())) {
                return "low";
            }
        }
        return "off";
    }

    /**
     * Build the set of allowed model keys from config allowlist.
     * Returns null if no allowlist is configured (meaning any model is allowed).
     */
    public static Set<String> buildAllowedModelSet(
            OpenClawConfig cfg, List<ModelCatalogEntry> catalog) {
        if (cfg == null || cfg.getAgents() == null || cfg.getAgents().getDefaults() == null) {
            return null; // no restriction
        }
        List<String> allowlist = cfg.getAgents().getDefaults().getModelAllowlist();
        if (allowlist == null || allowlist.isEmpty()) {
            return null; // no restriction
        }
        Set<String> allowed = new HashSet<>();
        for (String raw : allowlist) {
            ModelRef ref = parseModelRef(raw, DEFAULT_PROVIDER);
            if (ref != null) {
                allowed.add(ref.key());
            }
        }
        // Always include the configured primary model
        ModelRef primary = resolveConfiguredModel(cfg);
        allowed.add(primary.key());
        return allowed;
    }
}
