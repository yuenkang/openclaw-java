package com.openclaw.plugin;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

/**
 * Plugin manifest loading from {@code openclaw.plugin.json} files.
 * Corresponds to TypeScript's plugins/manifest.ts.
 */
@Slf4j
public final class PluginManifest {

    private PluginManifest() {
    }

    public static final String MANIFEST_FILENAME = "openclaw.plugin.json";

    private static final ObjectMapper MAPPER = new ObjectMapper();

    // =========================================================================
    // Manifest model
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class Manifest {
        private String id;
        private String name;
        private String description;
        private String version;
        private String kind;
        private List<String> channels;
        private List<String> providers;
        private List<String> skills;

        @JsonProperty("configSchema")
        private Map<String, Object> configSchema;

        @JsonProperty("uiHints")
        private Map<String, PluginTypes.PluginConfigUiHint> uiHints;
    }

    // =========================================================================
    // Load result
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class LoadResult {
        private boolean ok;
        private Manifest manifest;
        private String manifestPath;
        private String error;
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Resolve the path to the plugin manifest file in the given directory.
     */
    public static Path resolveManifestPath(String rootDir) {
        return Path.of(rootDir, MANIFEST_FILENAME);
    }

    /**
     * Load and parse a plugin manifest from the given directory.
     */
    public static LoadResult load(String rootDir) {
        Path manifestPath = resolveManifestPath(rootDir);
        String pathStr = manifestPath.toAbsolutePath().toString();

        if (!Files.isRegularFile(manifestPath)) {
            return LoadResult.builder()
                    .ok(false)
                    .manifestPath(pathStr)
                    .error("plugin manifest not found: " + pathStr)
                    .build();
        }

        try {
            String content = Files.readString(manifestPath);
            Manifest manifest = MAPPER.readValue(content, Manifest.class);

            if (manifest.getId() == null || manifest.getId().isBlank()) {
                return LoadResult.builder()
                        .ok(false)
                        .manifestPath(pathStr)
                        .error("plugin manifest requires id")
                        .build();
            }
            if (manifest.getConfigSchema() == null) {
                return LoadResult.builder()
                        .ok(false)
                        .manifestPath(pathStr)
                        .error("plugin manifest requires configSchema")
                        .build();
            }

            return LoadResult.builder()
                    .ok(true)
                    .manifest(manifest)
                    .manifestPath(pathStr)
                    .build();
        } catch (IOException e) {
            return LoadResult.builder()
                    .ok(false)
                    .manifestPath(pathStr)
                    .error("failed to parse plugin manifest: " + e.getMessage())
                    .build();
        }
    }
}
