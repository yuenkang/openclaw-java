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
     * Resolve the full model ID from a potentially aliased name.
     */
    public String resolveModelId(String modelId) {
        return aliases.getOrDefault(modelId, modelId);
    }

    /**
     * Auto-discover providers based on environment variables.
     * Corresponds to TypeScript's resolveImplicitProviders().
     */
    public void discoverFromEnvironment() {
        Map<String, String> env = System.getenv();

        for (Map.Entry<String, String> entry : ENV_VAR_MAP.entrySet()) {
            String providerId = entry.getKey();
            String envVar = entry.getValue();

            if (env.containsKey(envVar) && !providers.containsKey(providerId)) {
                log.info("Auto-discovered provider {} from env var {}", providerId, envVar);
                // Note: actual provider instances should be registered separately
                // This just detects availability
            }
        }

        // Ollama: probe local endpoint
        if (!providers.containsKey("ollama")) {
            discoverOllama();
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
}
