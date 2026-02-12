package com.openclaw.agent.runner;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Model resolution and inline provider model building for embedded agent runs.
 * Mirrors {@code agents/pi-embedded-runner/model.ts}.
 */
public final class RunnerModel {

    private RunnerModel() {
    }

    public static final int DEFAULT_CONTEXT_TOKENS = 200_000;

    /** An inline model entry from config.models.providers. */
    public record InlineModelEntry(
            String id,
            String provider,
            String baseUrl,
            String api,
            Boolean reasoning,
            List<String> input,
            Integer contextWindow,
            Integer maxTokens) {
    }

    /**
     * Build inline model entries from providers configuration.
     *
     * @param providers map of providerId → provider config (keys: baseUrl, api,
     *                  models[])
     * @return flat list of inline model entries
     */
    @SuppressWarnings("unchecked")
    public static List<InlineModelEntry> buildInlineProviderModels(
            Map<String, Object> providers) {
        if (providers == null || providers.isEmpty())
            return List.of();
        List<InlineModelEntry> result = new ArrayList<>();

        for (Map.Entry<String, Object> entry : providers.entrySet()) {
            String providerId = entry.getKey().trim();
            if (providerId.isEmpty())
                continue;
            if (!(entry.getValue() instanceof Map<?, ?> provCfg))
                continue;

            String baseUrl = provCfg.get("baseUrl") instanceof String s ? s : null;
            String provApi = provCfg.get("api") instanceof String s ? s : null;

            Object modelsObj = provCfg.get("models");
            if (!(modelsObj instanceof List<?> models))
                continue;

            for (Object m : models) {
                if (!(m instanceof Map<?, ?> modelMap))
                    continue;
                String id = modelMap.get("id") instanceof String s ? s : null;
                if (id == null)
                    continue;
                String modelApi = modelMap.get("api") instanceof String s ? s : provApi;
                String modelBaseUrl = modelMap.get("baseUrl") instanceof String s ? s : baseUrl;
                Boolean reasoning = modelMap.get("reasoning") instanceof Boolean b ? b : null;
                Integer contextWindow = modelMap.get("contextWindow") instanceof Number n ? n.intValue() : null;
                Integer maxTokens = modelMap.get("maxTokens") instanceof Number n ? n.intValue() : null;

                List<String> input = null;
                if (modelMap.get("input") instanceof List<?> inputList) {
                    input = inputList.stream()
                            .filter(String.class::isInstance)
                            .map(String.class::cast)
                            .collect(Collectors.toList());
                }

                result.add(new InlineModelEntry(
                        id, providerId, modelBaseUrl, modelApi,
                        reasoning, input, contextWindow, maxTokens));
            }
        }
        return result;
    }

    /**
     * Build model alias display lines from config.
     *
     * @param config the agents config map
     * @return sorted list of "- alias: model" strings
     */
    @SuppressWarnings("unchecked")
    public static List<String> buildModelAliasLines(Map<String, Object> config) {
        if (config == null)
            return List.of();
        Object agentsObj = config.get("agents");
        if (!(agentsObj instanceof Map<?, ?> agents))
            return List.of();
        Object defaultsObj = agents.get("defaults");
        if (!(defaultsObj instanceof Map<?, ?> defaults))
            return List.of();
        Object modelsObj = defaults.get("models");
        if (!(modelsObj instanceof Map<?, ?> models))
            return List.of();

        List<String[]> entries = new ArrayList<>();
        for (Map.Entry<?, ?> entry : models.entrySet()) {
            String model = String.valueOf(entry.getKey()).trim();
            if (model.isEmpty())
                continue;
            if (!(entry.getValue() instanceof Map<?, ?> entryVal))
                continue;
            Object aliasObj = entryVal.get("alias");
            String alias = aliasObj instanceof String s ? s.trim() : "";
            if (alias.isEmpty())
                continue;
            entries.add(new String[] { alias, model });
        }
        entries.sort(Comparator.comparing(a -> a[0]));
        return entries.stream()
                .map(e -> "- " + e[0] + ": " + e[1])
                .collect(Collectors.toList());
    }
}
