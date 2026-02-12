package com.openclaw.agent.models;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Registry for model providers with alias resolution and auto-discovery.
 * Corresponds to TypeScript's models-config.providers.ts.
 */
@Slf4j
public class ModelProviderRegistry {

    private final Map<String, ModelProvider> providers = new ConcurrentHashMap<>();
    private final Map<String, String> aliases = new ConcurrentHashMap<>();

    /** Well-known environment variable names for API keys. */
    private static final Map<String, String> ENV_VAR_MAP = Map.of(
            "anthropic", "ANTHROPIC_API_KEY",
            "openai", "OPENAI_API_KEY",
            "google", "GEMINI_API_KEY",
            "minimax", "MINIMAX_API_KEY",
            "moonshot", "MOONSHOT_API_KEY",
            "venice", "VENICE_API_KEY");

    public ModelProviderRegistry() {
        // Default aliases
        aliases.put("sonnet", "anthropic/claude-sonnet-4-5");
        aliases.put("opus", "anthropic/claude-opus-4-6");
        aliases.put("gpt", "openai/gpt-5.2");
        aliases.put("gemini", "google/gemini-2.5-pro");
    }

    /**
     * Register a model provider.
     */
    public void register(ModelProvider provider) {
        providers.put(provider.getId(), provider);
        log.info("Registered model provider: {}", provider.getId());
    }

    /**
     * Add a model alias.
     */
    public void addAlias(String alias, String modelId) {
        aliases.put(alias, modelId);
    }

    /**
     * Resolve a model ID (handling aliases) and return the provider.
     */
    public ModelProvider resolve(String modelId) {
        // Resolve alias first
        String resolved = aliases.getOrDefault(modelId, modelId);

        // Extract provider from "provider/model" format
        String providerId = resolved.contains("/") ? resolved.split("/")[0] : resolved;

        return providers.get(providerId);
    }

    /**
     * Get a provider directly by ID (no alias resolution).
     */
    public ModelProvider getProvider(String providerId) {
        return providers.get(providerId);
    }

    /**
     * Resolve the full model ID from a potentially aliased name.
     */
    public String resolveModelId(String modelId) {
        return aliases.getOrDefault(modelId, modelId);
    }

    /**
     * Auto-discover providers based on environment variables.
     * Corresponds to TypeScript's resolveImplicitProviders().
     * Creates and registers providers when API keys are found.
     */
    public void discoverFromEnvironment() {
        Map<String, String> env = System.getenv();

        for (Map.Entry<String, String> entry : ENV_VAR_MAP.entrySet()) {
            String providerId = entry.getKey();
            String envVar = entry.getValue();
            String apiKey = env.get(envVar);

            if (apiKey != null && !apiKey.isBlank() && !providers.containsKey(providerId)) {
                log.info("Auto-discovered provider {} from env var {}", providerId, envVar);
                registerFromEnv(providerId, apiKey);
            }
        }

        // Ollama: probe local endpoint
        if (!providers.containsKey("ollama")) {
            discoverOllama();
        }
    }

    /**
     * Register a provider from an environment-supplied API key.
     */
    private void registerFromEnv(String providerId, String apiKey) {
        String baseUrl = switch (providerId) {
            case "anthropic" -> "https://api.anthropic.com";
            case "openai" -> "https://api.openai.com/v1";
            case "google" -> "https://generativelanguage.googleapis.com/v1beta";
            case "minimax" -> "https://api.minimax.chat/v1";
            case "moonshot" -> "https://api.moonshot.cn/v1";
            case "venice" -> "https://api.venice.ai/api/v1";
            default -> null;
        };
        if (baseUrl != null) {
            try {
                ModelProvider provider;
                if ("anthropic".equals(providerId)) {
                    provider = new AnthropicProvider(apiKey, baseUrl);
                } else {
                    provider = new OpenAICompatibleProvider(providerId, apiKey, baseUrl);
                }
                register(provider);
            } catch (Exception e) {
                log.warn("Failed to register provider {} from env: {}", providerId, e.getMessage());
            }
        }
    }

    private void discoverOllama() {
        try {
            java.net.HttpURLConnection conn = (java.net.HttpURLConnection) new java.net.URL(
                    "http://127.0.0.1:11434/api/tags").openConnection();
            conn.setConnectTimeout(1000);
            conn.setReadTimeout(1000);
            if (conn.getResponseCode() == 200) {
                log.info("Auto-discovered Ollama at localhost:11434");
            }
            conn.disconnect();
        } catch (Exception e) {
            // Ollama not available, that's fine
        }
    }

    /**
     * Load custom aliases from config.
     */
    public void loadAliasesFromConfig(OpenClawConfig config) {
        if (config.getModelAliases() != null) {
            aliases.putAll(config.getModelAliases());
        }
    }

    /**
     * Check if a provider is registered.
     */
    public boolean hasProvider(String providerId) {
        return providers.containsKey(providerId);
    }

    /**
     * List all registered provider IDs.
     */
    public Set<String> getProviderIds() {
        return Collections.unmodifiableSet(providers.keySet());
    }

    /**
     * List all aliases.
     */
    public Map<String, String> getAliases() {
        return Collections.unmodifiableMap(aliases);
    }

    public int size() {
        return providers.size();
    }

    /**
     * Aggregate all models from all registered providers.
     */
    public List<ModelProvider.ModelInfo> listAllModels() {
        List<ModelProvider.ModelInfo> all = new java.util.ArrayList<>();
        for (ModelProvider provider : providers.values()) {
            try {
                all.addAll(provider.listModels());
            } catch (Exception e) {
                log.debug("listModels failed for {}: {}", provider.getId(), e.getMessage());
            }
        }
        return all;
    }

    /**
     * Create and register a provider from an OpenClawConfig.ProviderConfig.
     */
    public void registerFromConfig(String providerId,
            com.openclaw.common.config.OpenClawConfig.ProviderConfig config) {
        if (config == null || !config.isEnabled())
            return;
        if (providers.containsKey(providerId)) {
            log.debug("Provider {} already registered, skipping config registration", providerId);
            return;
        }
        String apiKey = config.getApiKey();
        String baseUrl = config.getApiBaseUrl();
        if (apiKey == null || apiKey.isBlank())
            return;

        try {
            ModelProvider provider;
            if ("anthropic".equals(providerId)) {
                provider = new AnthropicProvider(apiKey,
                        baseUrl != null ? baseUrl : "https://api.anthropic.com");
            } else {
                provider = new OpenAICompatibleProvider(providerId, apiKey,
                        baseUrl != null ? baseUrl : "https://api.openai.com/v1");
            }
            register(provider);
        } catch (Exception e) {
            log.warn("Failed to register provider {} from config: {}", providerId, e.getMessage());
        }
    }
}
