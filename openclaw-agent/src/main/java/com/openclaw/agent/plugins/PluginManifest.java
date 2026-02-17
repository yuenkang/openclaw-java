package com.openclaw.agent.plugins;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;

/**
 * Plugin manifest loading and parsing.
 * Corresponds to TypeScript's plugins/manifest.ts.
 */
@Slf4j
public final class PluginManifest {

    private PluginManifest() {
    }

    public static final String PLUGIN_MANIFEST_FILENAME = "openclaw.plugin.json";
    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    // =========================================================================
    // Manifest data model
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Manifest {
        private String id;
        @Builder.Default
        private Map<String, Object> configSchema = Map.of();
        private PluginTypes.PluginKind kind;
        @Builder.Default
        private List<String> channels = List.of();
        @Builder.Default
        private List<String> providers = List.of();
        @Builder.Default
        private List<String> skills = List.of();
        private String name;
        private String description;
        private String version;
        private Map<String, PluginTypes.PluginConfigUiHint> uiHints;
    }

    // =========================================================================
    // Load result
    // =========================================================================

    public sealed interface ManifestLoadResult {
        record Success(Manifest manifest, String manifestPath) implements ManifestLoadResult {
        }

        record Failure(String error, String manifestPath) implements ManifestLoadResult {
        }
    }

    // =========================================================================
    // Manifest resolution
    // =========================================================================

    /**
     * Resolve the plugin manifest file path in the given root directory.
     */
    public static String resolveManifestPath(String rootDir) {
        Path candidate = Path.of(rootDir, PLUGIN_MANIFEST_FILENAME);
        if (Files.exists(candidate)) {
            return candidate.toString();
        }
        return candidate.toString(); // Return default path even if not found
    }

    /**
     * Load and parse a plugin manifest from a directory.
     */
    @SuppressWarnings("unchecked")
    public static ManifestLoadResult loadManifest(String rootDir) {
        String manifestPath = resolveManifestPath(rootDir);
        if (!Files.exists(Path.of(manifestPath))) {
            return new ManifestLoadResult.Failure(
                    "plugin manifest not found: " + manifestPath, manifestPath);
        }

        Map<String, Object> raw;
        try {
            raw = OBJECT_MAPPER.readValue(
                    Files.readString(Path.of(manifestPath)), Map.class);
        } catch (IOException e) {
            return new ManifestLoadResult.Failure(
                    "failed to parse plugin manifest: " + e.getMessage(), manifestPath);
        }

        // Validate required fields
        String id = raw.get("id") instanceof String s ? s.trim() : "";
        if (id.isEmpty()) {
            return new ManifestLoadResult.Failure(
                    "plugin manifest requires id", manifestPath);
        }

        Object configSchemaRaw = raw.get("configSchema");
        if (!(configSchemaRaw instanceof Map)) {
            return new ManifestLoadResult.Failure(
                    "plugin manifest requires configSchema", manifestPath);
        }
        Map<String, Object> configSchema = (Map<String, Object>) configSchemaRaw;

        // Optional fields
        PluginTypes.PluginKind kind = raw.get("kind") instanceof String s
                ? PluginTypes.PluginKind.fromString(s)
                : null;
        String name = raw.get("name") instanceof String s ? s.trim() : null;
        String description = raw.get("description") instanceof String s ? s.trim() : null;
        String version = raw.get("version") instanceof String s ? s.trim() : null;
        List<String> channels = normalizeStringList(raw.get("channels"));
        List<String> providers = normalizeStringList(raw.get("providers"));
        List<String> skills = normalizeStringList(raw.get("skills"));

        Manifest manifest = Manifest.builder()
                .id(id)
                .configSchema(configSchema)
                .kind(kind)
                .channels(channels)
                .providers(providers)
                .skills(skills)
                .name(name)
                .description(description)
                .version(version)
                .build();

        return new ManifestLoadResult.Success(manifest, manifestPath);
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    static List<String> normalizeStringList(Object value) {
        if (!(value instanceof List<?> list)) {
            return List.of();
        }
        return list.stream()
                .filter(String.class::isInstance)
                .map(e -> ((String) e).trim())
                .filter(s -> !s.isEmpty())
                .toList();
    }
}
