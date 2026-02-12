package com.openclaw.agent.models;

import com.openclaw.agent.models.ModelSelector.ModelCatalogEntry;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Service for loading and caching the model catalog.
 * Aggregates models from registered providers and config.
 * Corresponds to TypeScript's model-catalog.ts.
 */
@Slf4j
public class ModelCatalogService {

    private final ModelProviderRegistry providerRegistry;
    private final AtomicReference<CompletableFuture<List<ModelCatalogEntry>>> cacheRef = new AtomicReference<>();
    private volatile boolean hasLoggedError = false;

    public ModelCatalogService(ModelProviderRegistry providerRegistry) {
        this.providerRegistry = providerRegistry;
    }

    /**
     * Load the model catalog, using the cache if available.
     */
    public CompletableFuture<List<ModelCatalogEntry>> loadCatalog(OpenClawConfig config, boolean useCache) {
        if (!useCache) {
            cacheRef.set(null);
        }
        CompletableFuture<List<ModelCatalogEntry>> cached = cacheRef.get();
        if (cached != null) {
            return cached;
        }

        CompletableFuture<List<ModelCatalogEntry>> future = CompletableFuture.supplyAsync(() -> buildCatalog(config));

        // Atomically set the cache only if nobody else beat us.
        cacheRef.compareAndSet(null, future);
        return cacheRef.get();
    }

    /**
     * Load catalog with caching enabled.
     */
    public CompletableFuture<List<ModelCatalogEntry>> loadCatalog(OpenClawConfig config) {
        return loadCatalog(config, true);
    }

    /**
     * Invalidate the cached catalog.
     */
    public void invalidateCache() {
        cacheRef.set(null);
        hasLoggedError = false;
    }

    private List<ModelCatalogEntry> buildCatalog(OpenClawConfig config) {
        List<ModelCatalogEntry> models = new ArrayList<>();
        Set<String> seen = new HashSet<>();

        try {
            // 1. Collect from registered providers
            for (String providerId : providerRegistry.getProviderIds()) {
                ModelProvider provider = providerRegistry.getProvider(providerId);
                if (provider == null)
                    continue;
                try {
                    List<ModelProvider.ModelInfo> infos = provider.listModels();
                    for (ModelProvider.ModelInfo info : infos) {
                        String key = providerId + "/" + info.getId();
                        if (seen.add(key)) {
                            models.add(new ModelCatalogEntry(
                                    info.getId(),
                                    info.getName() != null ? info.getName() : info.getId(),
                                    providerId,
                                    info.getContextWindow() > 0 ? info.getContextWindow() : null,
                                    null, // reasoning unknown from ModelInfo
                                    null // input capabilities unknown
                            ));
                        }
                    }
                } catch (Exception e) {
                    log.debug("Failed to list models from provider {}: {}", providerId, e.getMessage());
                }
            }

            // 2. Merge from config definitions
            if (config != null && config.getModels() != null) {
                if (config.getModels().getDefinitions() != null) {
                    for (OpenClawConfig.ModelDefinition def : config.getModels().getDefinitions()) {
                        String provider = def.getProvider() != null ? def.getProvider() : "unknown";
                        String key = provider + "/" + def.getId();
                        if (seen.add(key)) {
                            models.add(new ModelCatalogEntry(
                                    def.getId(),
                                    def.getName() != null ? def.getName() : def.getId(),
                                    provider,
                                    def.getContextWindow(),
                                    def.getReasoning(),
                                    def.getInput()));
                        }
                    }
                }

                // 3. Models from provider configs
                if (config.getModels().getProviders() != null) {
                    for (Map.Entry<String, OpenClawConfig.ProviderConfig> pe : config.getModels().getProviders()
                            .entrySet()) {
                        String providerId = pe.getKey();
                        OpenClawConfig.ProviderConfig pc = pe.getValue();
                        if (pc.getModels() != null) {
                            for (OpenClawConfig.ModelDefinition def : pc.getModels()) {
                                String key = providerId + "/" + def.getId();
                                if (seen.add(key)) {
                                    models.add(new ModelCatalogEntry(
                                            def.getId(),
                                            def.getName() != null ? def.getName() : def.getId(),
                                            providerId,
                                            def.getContextWindow(),
                                            def.getReasoning(),
                                            def.getInput()));
                                }
                            }
                        }
                    }
                }
            }

            // Sort by provider then name
            models.sort(Comparator.comparing(ModelCatalogEntry::provider)
                    .thenComparing(ModelCatalogEntry::name));

            if (models.isEmpty()) {
                // Don't cache empty results
                cacheRef.set(null);
            }
        } catch (Exception e) {
            if (!hasLoggedError) {
                hasLoggedError = true;
                log.warn("[model-catalog] Failed to build model catalog: {}", e.getMessage());
            }
            cacheRef.set(null);
        }

        return models;
    }

    /**
     * Find a specific model in the catalog.
     */
    public ModelCatalogEntry findModel(
            List<ModelCatalogEntry> catalog, String provider, String modelId) {
        return ModelSelector.findModelInCatalog(catalog, provider, modelId);
    }
}
