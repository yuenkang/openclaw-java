package com.openclaw.plugin.loader;

import com.openclaw.plugin.PluginTypes;
import com.openclaw.plugin.PluginTypes.PluginKind;
import com.openclaw.plugin.PluginTypes.PluginConfigUiHint;
import com.openclaw.common.config.OpenClawConfig;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Plugin manifest registry — discovers, parses, and caches plugin manifests.
 * Corresponds to TypeScript's plugins/manifest-registry.ts (201 lines).
 */
@Slf4j
public final class PluginManifestRegistry {

    private PluginManifestRegistry() {
    }

    private static final long DEFAULT_CACHE_MS = 200;
    private static final Map<String, CachedEntry> cache = new ConcurrentHashMap<>();

    // =========================================================================
    // Types
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ManifestRecord {
        private String id;
        private String name;
        private String description;
        private String version;
        private PluginKind kind;
        @Builder.Default
        private List<String> channels = List.of();
        @Builder.Default
        private List<String> providers = List.of();
        @Builder.Default
        private List<String> skills = List.of();
        private PluginTypes.PluginOrigin origin;
        private String workspaceDir;
        private String rootDir;
        private String source;
        private String manifestPath;
        private String schemaCacheKey;
        private Map<String, Object> configSchema;
        private Map<String, PluginConfigUiHint> configUiHints;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ManifestRegistryResult {
        @Builder.Default
        private List<ManifestRecord> plugins = new ArrayList<>();
        @Builder.Default
        private List<PluginTypes.PluginDiagnostic> diagnostics = new ArrayList<>();
    }

    private record CachedEntry(long expiresAt, ManifestRegistryResult registry) {
    }

    // =========================================================================
    // Loading
    // =========================================================================

    /**
     * Load the plugin manifest registry — discovers candidates, parses
     * manifests, deduplicates by id, and caches the result.
     */
    public static ManifestRegistryResult load(OpenClawConfig config, String workspaceDir) {
        return load(config, workspaceDir, true);
    }

    public static ManifestRegistryResult load(OpenClawConfig config,
            String workspaceDir, boolean useCache) {
        String cacheKey = buildCacheKey(workspaceDir, config);
        if (useCache) {
            CachedEntry entry = cache.get(cacheKey);
            if (entry != null && entry.expiresAt > System.currentTimeMillis()) {
                return entry.registry;
            }
        }

        // Discover candidates using PluginDiscovery
        PluginDiscovery.PluginDiscoveryResult discovery = PluginDiscovery.discoverPlugins(
                workspaceDir, List.of());

        List<PluginTypes.PluginDiagnostic> diagnostics = new ArrayList<>(discovery.getDiagnostics());
        List<ManifestRecord> records = new ArrayList<>();
        Set<String> seenIds = new HashSet<>();

        for (var candidate : discovery.getCandidates()) {
            PluginManifest.ManifestLoadResult manifestResult = PluginManifest.loadManifest(candidate.getRootDir());
            if (manifestResult instanceof PluginManifest.ManifestLoadResult.Failure f) {
                diagnostics.add(PluginTypes.PluginDiagnostic.builder()
                        .level("error").message(f.error()).build());
                continue;
            }

            PluginManifest.Manifest manifest = ((PluginManifest.ManifestLoadResult.Success) manifestResult).manifest();
            String manifestPath = ((PluginManifest.ManifestLoadResult.Success) manifestResult).manifestPath();

            if (seenIds.contains(manifest.getId())) {
                diagnostics.add(PluginTypes.PluginDiagnostic.builder()
                        .pluginId(manifest.getId())
                        .level("warn")
                        .message("duplicate plugin id detected: " + candidate.getSource())
                        .build());
            } else {
                seenIds.add(manifest.getId());
            }

            records.add(ManifestRecord.builder()
                    .id(manifest.getId())
                    .name(manifest.getName())
                    .description(manifest.getDescription())
                    .version(manifest.getVersion())
                    .kind(manifest.getKind())
                    .channels(manifest.getChannels())
                    .providers(manifest.getProviders())
                    .skills(manifest.getSkills())
                    .origin(candidate.getOrigin())
                    .workspaceDir(candidate.getWorkspaceDir())
                    .rootDir(candidate.getRootDir())
                    .source(candidate.getSource())
                    .manifestPath(manifestPath)
                    .configSchema(manifest.getConfigSchema())
                    .configUiHints(manifest.getUiHints())
                    .build());
        }

        ManifestRegistryResult result = ManifestRegistryResult.builder()
                .plugins(records)
                .diagnostics(diagnostics)
                .build();

        if (useCache) {
            long ttl = resolveCacheMs();
            if (ttl > 0) {
                cache.put(cacheKey, new CachedEntry(
                        System.currentTimeMillis() + ttl, result));
            }
        }

        return result;
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private static String buildCacheKey(String workspaceDir, OpenClawConfig config) {
        String ws = workspaceDir != null ? workspaceDir : "";
        return ws + "::" + (config != null ? config.hashCode() : "null");
    }

    private static long resolveCacheMs() {
        String raw = System.getenv("OPENCLAW_PLUGIN_MANIFEST_CACHE_MS");
        if (raw != null && !raw.isBlank()) {
            try {
                return Math.max(0, Long.parseLong(raw.trim()));
            } catch (NumberFormatException e) {
                return DEFAULT_CACHE_MS;
            }
        }
        return DEFAULT_CACHE_MS;
    }

    public static void clearCache() {
        cache.clear();
    }
}
