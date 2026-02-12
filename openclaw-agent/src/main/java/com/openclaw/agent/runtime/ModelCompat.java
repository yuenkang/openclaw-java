package com.openclaw.agent.runtime;

import java.util.HashMap;
import java.util.Map;

/**
 * Model compatibility normalization for specific providers (e.g. zAI).
 * Mirrors {@code agents/model-compat.ts}.
 */
public final class ModelCompat {

    private ModelCompat() {
    }

    /**
     * Apply provider-specific compat overrides to a model config map.
     * Currently handles zAI models with openai-completions API, setting
     * supportsDeveloperRole=false.
     *
     * @param model mutable model property map (provider, api, baseUrl, compat,
     *              etc.)
     * @return the same map, possibly with compat fields adjusted
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> normalizeModelCompat(Map<String, Object> model) {
        String baseUrl = (String) model.getOrDefault("baseUrl", "");
        String provider = (String) model.getOrDefault("provider", "");
        String api = (String) model.getOrDefault("api", "");

        boolean isZai = "zai".equals(provider) || baseUrl.contains("api.z.ai");
        boolean isOpenaiCompletions = "openai-completions".equals(api);

        if (!isZai || !isOpenaiCompletions) {
            return model;
        }

        Map<String, Object> compat = (Map<String, Object>) model.get("compat");
        if (compat != null && Boolean.FALSE.equals(compat.get("supportsDeveloperRole"))) {
            return model;
        }

        Map<String, Object> newCompat = compat != null ? new HashMap<>(compat) : new HashMap<>();
        newCompat.put("supportsDeveloperRole", false);
        model.put("compat", newCompat);
        return model;
    }
}
