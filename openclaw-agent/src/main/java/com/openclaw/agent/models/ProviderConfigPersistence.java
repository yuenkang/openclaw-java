package com.openclaw.agent.models;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * Handles reading, writing and merging of models.json.
 * Corresponds to TypeScript's models-config.ts (ensureOpenClawModelsJson).
 */
@Slf4j
public class ProviderConfigPersistence {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private final Path agentDir;

    public ProviderConfigPersistence(Path agentDir) {
        this.agentDir = agentDir;
    }

    /**
     * Resolve the models.json file path.
     */
    public Path getModelsJsonPath() {
        return agentDir.resolve("models.json");
    }

    /**
     * Read the current models.json content.
     * Returns null if the file does not exist or is invalid.
     */
    @SuppressWarnings("unchecked")
    public Map<String, OpenClawConfig.ProviderConfig> readProviders() {
        Path path = getModelsJsonPath();
        if (!Files.exists(path))
            return null;
        try {
            Map<String, Object> root = MAPPER.readValue(path.toFile(),
                    new TypeReference<Map<String, Object>>() {
                    });
            Object providers = root.get("providers");
            if (providers instanceof Map) {
                Map<String, Object> provMap = (Map<String, Object>) providers;
                Map<String, OpenClawConfig.ProviderConfig> result = new LinkedHashMap<>();
                for (Map.Entry<String, Object> entry : provMap.entrySet()) {
                    OpenClawConfig.ProviderConfig pc = MAPPER.convertValue(
                            entry.getValue(), OpenClawConfig.ProviderConfig.class);
                    result.put(entry.getKey(), pc);
                }
                return result;
            }
        } catch (IOException e) {
            log.warn("Failed to read models.json: {}", e.getMessage());
        }
        return null;
    }

    /**
     * Write providers to models.json, optionally merging with existing content.
     *
     * @param providers the provider configs to write
     * @param merge     if true, merge with existing providers; if false, replace
     * @return true if the file was actually written (content changed)
     */
    public boolean writeProviders(
            Map<String, OpenClawConfig.ProviderConfig> providers, boolean merge) {
        try {
            Map<String, OpenClawConfig.ProviderConfig> toWrite;
            if (merge) {
                Map<String, OpenClawConfig.ProviderConfig> existing = readProviders();
                toWrite = mergeProviders(existing, providers);
            } else {
                toWrite = providers;
            }

            Map<String, Object> root = new LinkedHashMap<>();
            root.put("providers", toWrite);
            String next = MAPPER.writerWithDefaultPrettyPrinter()
                    .writeValueAsString(root) + "\n";

            // Check if content changed
            Path path = getModelsJsonPath();
            if (Files.exists(path)) {
                String existing = Files.readString(path);
                if (existing.equals(next)) {
                    return false;
                }
            }

            Files.createDirectories(agentDir);
            Files.writeString(path, next);
            log.info("Wrote models.json with {} providers", toWrite.size());
            return true;
        } catch (IOException e) {
            log.warn("Failed to write models.json: {}", e.getMessage());
            return false;
        }
    }

    /**
     * Merge implicit and explicit providers. Explicit values take precedence,
     * but models lists are merged (explicit first, then implicit without dupes).
     */
    public static Map<String, OpenClawConfig.ProviderConfig> mergeProviders(
            Map<String, OpenClawConfig.ProviderConfig> implicit,
            Map<String, OpenClawConfig.ProviderConfig> explicit) {
        Map<String, OpenClawConfig.ProviderConfig> out = new LinkedHashMap<>();
        if (implicit != null)
            out.putAll(implicit);
        if (explicit == null)
            return out;

        for (Map.Entry<String, OpenClawConfig.ProviderConfig> entry : explicit.entrySet()) {
            String key = entry.getKey().trim();
            if (key.isEmpty())
                continue;
            OpenClawConfig.ProviderConfig implicitPc = out.get(key);
            if (implicitPc != null) {
                out.put(key, mergeProviderModels(implicitPc, entry.getValue()));
            } else {
                out.put(key, entry.getValue());
            }
        }
        return out;
    }

    /**
     * Merge two provider configs. Explicit values override implicit, but model
     * lists are merged (explicit models first, then implicit without dupes).
     */
    private static OpenClawConfig.ProviderConfig mergeProviderModels(
            OpenClawConfig.ProviderConfig implicit,
            OpenClawConfig.ProviderConfig explicit) {
        OpenClawConfig.ProviderConfig result = new OpenClawConfig.ProviderConfig();
        // Explicit values take priority
        result.setId(explicit.getId() != null ? explicit.getId() : implicit.getId());
        result.setApiBaseUrl(explicit.getApiBaseUrl() != null ? explicit.getApiBaseUrl() : implicit.getApiBaseUrl());
        result.setApiKey(explicit.getApiKey() != null ? explicit.getApiKey() : implicit.getApiKey());
        result.setEnabled(explicit.isEnabled());

        // Merge model lists
        List<OpenClawConfig.ModelDefinition> implicitModels = implicit.getModels() != null ? implicit.getModels()
                : List.of();
        List<OpenClawConfig.ModelDefinition> explicitModels = explicit.getModels() != null ? explicit.getModels()
                : List.of();
        if (implicitModels.isEmpty()) {
            result.setModels(explicitModels.isEmpty() ? null : new ArrayList<>(explicitModels));
        } else {
            Set<String> seen = new HashSet<>();
            List<OpenClawConfig.ModelDefinition> merged = new ArrayList<>();
            for (OpenClawConfig.ModelDefinition m : explicitModels) {
                if (m.getId() != null)
                    seen.add(m.getId().trim());
                merged.add(m);
            }
            for (OpenClawConfig.ModelDefinition m : implicitModels) {
                if (m.getId() != null && !seen.contains(m.getId().trim())) {
                    merged.add(m);
                    seen.add(m.getId().trim());
                }
            }
            result.setModels(merged);
        }
        return result;
    }
}
