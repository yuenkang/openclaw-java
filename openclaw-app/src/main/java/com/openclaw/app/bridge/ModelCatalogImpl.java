package com.openclaw.app.bridge;

import com.openclaw.agent.models.ModelProvider;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.gateway.methods.ModelCatalog;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.*;

/**
 * Implements ModelCatalog by delegating to ModelProviderRegistry.
 * Lives in the app module to bridge gateway ↔ agent modules.
 */
@Component
public class ModelCatalogImpl implements ModelCatalog {

    private final ModelProviderRegistry registry;

    public ModelCatalogImpl(ModelProviderRegistry registry) {
        this.registry = registry;
    }

    @Override
    public List<Map<String, Object>> listModels() {
        // Query all providers in parallel with a timeout to avoid
        // sequential blocking when provider APIs are slow.
        List<CompletableFuture<List<Map<String, Object>>>> futures = new ArrayList<>();

        for (String providerId : registry.getProviderIds()) {
            ModelProvider provider = registry.resolve(providerId);
            if (provider == null)
                continue;

            final String pid = providerId;
            futures.add(CompletableFuture.supplyAsync(() -> {
                List<Map<String, Object>> providerModels = new ArrayList<>();
                try {
                    for (ModelProvider.ModelInfo info : provider.listModels()) {
                        Map<String, Object> entry = new LinkedHashMap<>();
                        entry.put("id", pid + "/" + info.getId());
                        entry.put("provider", pid);
                        entry.put("name", info.getName());
                        entry.put("contextWindow", info.getContextWindow());
                        entry.put("maxTokens", info.getMaxTokens());
                        providerModels.add(entry);
                    }
                } catch (Exception e) {
                    // Provider may not support listing, skip
                }
                return providerModels;
            }));
        }

        // Wait for all providers with a total timeout of 3 seconds
        List<Map<String, Object>> models = new ArrayList<>();
        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                    .get(3, TimeUnit.SECONDS);
            for (CompletableFuture<List<Map<String, Object>>> f : futures) {
                if (f.isDone() && !f.isCompletedExceptionally()) {
                    models.addAll(f.join());
                }
            }
        } catch (TimeoutException e) {
            // Collect whatever finished in time
            for (CompletableFuture<List<Map<String, Object>>> f : futures) {
                if (f.isDone() && !f.isCompletedExceptionally()) {
                    models.addAll(f.join());
                }
            }
        } catch (Exception e) {
            // Collect whatever finished
            for (CompletableFuture<List<Map<String, Object>>> f : futures) {
                if (f.isDone() && !f.isCompletedExceptionally()) {
                    models.addAll(f.join());
                }
            }
        }
        return models;
    }

    @Override
    public List<String> listProviders() {
        return new ArrayList<>(registry.getProviderIds());
    }

    @Override
    public Map<String, String> listAliases() {
        return registry.getAliases();
    }
}
