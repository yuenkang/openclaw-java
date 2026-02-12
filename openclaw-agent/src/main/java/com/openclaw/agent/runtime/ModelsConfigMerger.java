package com.openclaw.agent.runtime;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Models configuration: implicit provider discovery and merging
 * with explicit (user-configured) providers.
 * Mirrors {@code agents/models-config.ts}.
 */
@Slf4j
public final class ModelsConfigMerger {

    private ModelsConfigMerger() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper();

    // --- Public API ---

    /**
     * Read, merge, and write the models.json file for the agent.
     *
     * @return whether data was written to disk.
     */
    public static ModelsResult ensureModelsJson(
            Map<String, Object> modelsConfig,
            Map<String, Map<String, Object>> implicitProviders,
            String agentDir) throws IOException {

        Map<String, Map<String, Object>> explicitProviders = extractProviders(modelsConfig);
        Map<String, Map<String, Object>> providers = mergeProviders(
                implicitProviders, explicitProviders);

        if (providers.isEmpty()) {
            return new ModelsResult(agentDir, false);
        }

        String mode = resolveMode(modelsConfig);
        Path targetPath = Path.of(agentDir, "models.json");

        Map<String, Map<String, Object>> mergedProviders = providers;
        if ("merge".equals(mode)) {
            Map<String, Map<String, Object>> existing = readExistingProviders(targetPath);
            if (existing != null) {
                Map<String, Map<String, Object>> combined = new LinkedHashMap<>(existing);
                combined.putAll(providers);
                mergedProviders = combined;
            }
        }

        // Normalize and write
        Map<String, Object> output = new LinkedHashMap<>();
        output.put("providers", mergedProviders);
        String next = MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(output) + "\n";

        String existingRaw = readFileQuietly(targetPath);
        if (next.equals(existingRaw)) {
            return new ModelsResult(agentDir, false);
        }

        Files.createDirectories(Path.of(agentDir));
        Files.writeString(targetPath, next, StandardOpenOption.CREATE,
                StandardOpenOption.TRUNCATE_EXISTING);
        return new ModelsResult(agentDir, true);
    }

    // --- Result ---

    public record ModelsResult(String agentDir, boolean wrote) {
    }

    // --- Provider merging ---

    static Map<String, Map<String, Object>> mergeProviders(
            Map<String, Map<String, Object>> implicit,
            Map<String, Map<String, Object>> explicit) {

        Map<String, Map<String, Object>> out = implicit != null
                ? new LinkedHashMap<>(implicit)
                : new LinkedHashMap<>();

        if (explicit == null)
            return out;
        for (var entry : explicit.entrySet()) {
            String key = entry.getKey().trim();
            if (key.isEmpty())
                continue;
            Map<String, Object> existing = out.get(key);
            out.put(key, existing != null
                    ? mergeProviderModels(existing, entry.getValue())
                    : entry.getValue());
        }
        return out;
    }

    @SuppressWarnings("unchecked")
    static Map<String, Object> mergeProviderModels(
            Map<String, Object> implicit, Map<String, Object> explicit) {

        List<Map<String, Object>> implicitModels = asList(implicit.get("models"));
        List<Map<String, Object>> explicitModels = asList(explicit.get("models"));

        if (implicitModels.isEmpty()) {
            Map<String, Object> merged = new LinkedHashMap<>(implicit);
            merged.putAll(explicit);
            return merged;
        }

        Set<String> seen = explicitModels.stream()
                .map(m -> getId(m))
                .filter(id -> !id.isEmpty())
                .collect(Collectors.toCollection(LinkedHashSet::new));

        List<Map<String, Object>> mergedModels = new ArrayList<>(explicitModels);
        for (Map<String, Object> model : implicitModels) {
            String id = getId(model);
            if (id.isEmpty() || seen.contains(id))
                continue;
            seen.add(id);
            mergedModels.add(model);
        }

        Map<String, Object> result = new LinkedHashMap<>(implicit);
        result.putAll(explicit);
        result.put("models", mergedModels);
        return result;
    }

    // --- Internal helpers ---

    @SuppressWarnings("unchecked")
    private static Map<String, Map<String, Object>> extractProviders(Map<String, Object> modelsConfig) {
        if (modelsConfig == null)
            return new LinkedHashMap<>();
        Object providers = modelsConfig.get("providers");
        if (providers instanceof Map<?, ?> m) {
            return (Map<String, Map<String, Object>>) m;
        }
        return new LinkedHashMap<>();
    }

    private static String resolveMode(Map<String, Object> modelsConfig) {
        if (modelsConfig == null)
            return "merge";
        Object mode = modelsConfig.get("mode");
        return mode instanceof String s ? s : "merge";
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Map<String, Object>> readExistingProviders(Path path) {
        try {
            String raw = Files.readString(path);
            Map<String, Object> parsed = MAPPER.readValue(raw, Map.class);
            Object providers = parsed.get("providers");
            if (providers instanceof Map<?, ?> m) {
                return (Map<String, Map<String, Object>>) m;
            }
        } catch (Exception ignored) {
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    private static List<Map<String, Object>> asList(Object value) {
        if (value instanceof List<?> list) {
            return (List<Map<String, Object>>) list;
        }
        return List.of();
    }

    private static String getId(Map<String, Object> model) {
        Object id = model.get("id");
        return id instanceof String s ? s.trim() : "";
    }

    private static String readFileQuietly(Path path) {
        try {
            return Files.readString(path);
        } catch (Exception e) {
            return "";
        }
    }
}
