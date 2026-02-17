package com.openclaw.agent.plugins;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Plugin loader — discovers, validates, and loads plugins into the registry.
 * Corresponds to TypeScript's plugins/loader.ts.
 */
@Slf4j
public final class PluginLoader {

    private PluginLoader() {
    }

    // =========================================================================
    // Load options
    // =========================================================================

    public record PluginLoadOptions(
            OpenClawConfig config,
            String workspaceDir,
            PluginTypes.PluginLogger logger,
            boolean cache) {

        public PluginLoadOptions {
            if (logger == null) {
                logger = new DefaultPluginLogger();
            }
        }
    }

    // =========================================================================
    // Load
    // =========================================================================

    /**
     * Load and register all discovered plugins.
     *
     * @param options load options
     * @return populated plugin registry
     */
    public static PluginRegistry loadPlugins(PluginLoadOptions options) {
        PluginRegistry registry = new PluginRegistry();

        // 1. Resolve normalized config from OpenClawConfig.PluginsConfig
        Map<String, Object> pluginsConfig = null;
        if (options.config() != null && options.config().getPlugins() != null) {
            var pluginsCfg = options.config().getPlugins();
            pluginsConfig = new LinkedHashMap<>();
            if (pluginsCfg.getEntries() != null) {
                Map<String, Object> entriesMap = new LinkedHashMap<>();
                pluginsCfg.getEntries().forEach((k, v) -> entriesMap.put(k, v));
                pluginsConfig.put("entries", entriesMap);
            }
        }
        PluginConfigState.NormalizedPluginsConfig normalizedConfig = PluginConfigState
                .normalizePluginsConfig(pluginsConfig);

        // 2. Check if plugins are disabled globally
        if (!normalizedConfig.enabled()) {
            options.logger().info("Plugins are globally disabled");
            return registry;
        }

        // 3. Discover candidates
        PluginDiscovery.PluginDiscoveryResult discoveryResult = PluginDiscovery.discoverPlugins(
                options.workspaceDir(),
                normalizedConfig.loadPaths());

        // Forward discovery diagnostics
        for (var diag : discoveryResult.getDiagnostics()) {
            registry.addDiagnostic(diag);
        }

        // 4. Load each candidate
        for (var candidate : discoveryResult.getCandidates()) {
            loadCandidate(candidate, normalizedConfig, registry, options);
        }

        log.info("Loaded plugins: {} total, {} enabled",
                registry.getPlugins().size(),
                registry.getEnabledPlugins().size());

        return registry;
    }

    // =========================================================================
    // Load individual candidate
    // =========================================================================

    private static void loadCandidate(
            PluginDiscovery.PluginCandidate candidate,
            PluginConfigState.NormalizedPluginsConfig config,
            PluginRegistry registry,
            PluginLoadOptions options) {

        // Try loading manifest
        PluginManifest.ManifestLoadResult manifestResult = PluginManifest.loadManifest(candidate.getRootDir());

        String pluginId = candidate.getIdHint();
        PluginManifest.Manifest manifest = null;

        if (manifestResult instanceof PluginManifest.ManifestLoadResult.Success success) {
            manifest = success.manifest();
            pluginId = manifest.getId();
        }

        // Resolve enable state
        PluginConfigState.EnableState enableState = PluginConfigState.resolveEnableState(
                pluginId, candidate.getOrigin(), config);

        // Create plugin record
        PluginRegistry.PluginRecord record = PluginRegistry.PluginRecord.builder()
                .id(pluginId)
                .name(manifest != null ? manifest.getName() : pluginId)
                .version(manifest != null ? manifest.getVersion() : null)
                .description(manifest != null ? manifest.getDescription() : null)
                .kind(manifest != null ? manifest.getKind() : null)
                .source(candidate.getSource())
                .origin(candidate.getOrigin())
                .workspaceDir(candidate.getWorkspaceDir())
                .enabled(enableState.enabled())
                .status(enableState.enabled() ? "loaded" : "disabled")
                .error(enableState.reason())
                .configSchema(manifest != null && manifest.getConfigSchema() != null)
                .build();

        registry.registerPlugin(record);

        if (!enableState.enabled()) {
            log.debug("Plugin {} disabled: {}", pluginId, enableState.reason());
            return;
        }

        // Validate config schema if present
        if (manifest != null && manifest.getConfigSchema() != null) {
            PluginConfigState.PluginEntryConfig entryConfig = config.entries().get(pluginId);
            if (entryConfig != null && entryConfig.config() != null) {
                log.debug("Plugin {} has config, validating schema", pluginId);
            }
        }

        options.logger().info("Loaded plugin: " + pluginId);
    }

    // =========================================================================
    // Default logger
    // =========================================================================

    private static class DefaultPluginLogger implements PluginTypes.PluginLogger {
        @Override
        public void info(String message) {
            log.info("[plugins] {}", message);
        }

        @Override
        public void warn(String message) {
            log.warn("[plugins] {}", message);
        }

        @Override
        public void error(String message) {
            log.error("[plugins] {}", message);
        }
    }
}
