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
    public static final String DEFAULT_MODEL = "claude-sonnet-4-20250514";

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
}
