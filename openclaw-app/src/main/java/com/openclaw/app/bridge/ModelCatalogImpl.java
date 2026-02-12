package com.openclaw.app.bridge;

import com.openclaw.agent.models.ModelProvider;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.gateway.methods.ModelCatalog;
import org.springframework.stereotype.Component;

import java.util.*;

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
        List<Map<String, Object>> models = new ArrayList<>();
        for (String providerId : registry.getProviderIds()) {
            ModelProvider provider = registry.resolve(providerId);
            if (provider != null) {
                try {
                    for (ModelProvider.ModelInfo info : provider.listModels()) {
                        Map<String, Object> entry = new LinkedHashMap<>();
                        entry.put("id", providerId + "/" + info.getId());
                        entry.put("provider", providerId);
                        entry.put("name", info.getName());
                        entry.put("contextWindow", info.getContextWindow());
                        entry.put("maxTokens", info.getMaxTokens());
                        models.add(entry);
                    }
                } catch (Exception e) {
                    // Provider may not support listing, skip
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
