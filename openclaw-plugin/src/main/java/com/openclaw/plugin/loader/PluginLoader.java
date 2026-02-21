package com.openclaw.plugin.loader;

import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.plugin.PluginTypes;
import com.openclaw.plugin.commands.PluginCommandProcessor;
import com.openclaw.plugin.config.PluginEnable;
import com.openclaw.plugin.config.PluginSchemaValidator;
import com.openclaw.plugin.registry.PluginRegistry;
import com.openclaw.plugin.slots.PluginSlots;
import lombok.extern.slf4j.Slf4j;

import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.*;

/**
 * Plugin loader — discovers, validates, loads, and activates plugins.
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
            boolean cache,
            PluginCommandProcessor commandProcessor) {

        /** Compact constructor for backward compatibility (no commandProcessor). */
        public PluginLoadOptions(OpenClawConfig config, String workspaceDir,
                PluginTypes.PluginLogger logger, boolean cache) {
            this(config, workspaceDir, logger, cache, null);
        }

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

        // 5. Apply exclusive slot selection for plugins with 'kind'
        applySlots(registry, options);

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

        // Resolve enable state (PluginConfigState + PluginEnable cross-check)
        PluginConfigState.EnableState enableState = PluginConfigState.resolveEnableState(
                pluginId, candidate.getOrigin(), config);

        // Also check PluginEnable against OpenClawConfig if available
        if (enableState.enabled() && options.config() != null) {
            PluginEnable.EnableResult enableResult = PluginEnable.enablePluginInConfig(options.config(), pluginId);
            if (!enableResult.isEnabled()) {
                enableState = new PluginConfigState.EnableState(false, enableResult.getReason());
            }
        }

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

        // Validate config schema if present (using PluginSchemaValidator)
        if (manifest != null && manifest.getConfigSchema() != null) {
            PluginConfigState.PluginEntryConfig entryConfig = config.entries().get(pluginId);
            if (entryConfig != null && entryConfig.config() != null) {
                PluginSchemaValidator.ValidationResult validResult = PluginSchemaValidator.validate(
                        manifest.getConfigSchema(),
                        pluginId + ":config",
                        entryConfig.config());
                if (validResult instanceof PluginSchemaValidator.ValidationResult.Fail fail) {
                    options.logger().warn("Plugin " + pluginId +
                            " config validation failed: " + String.join("; ", fail.errors()));
                    record.setStatus("config-error");
                    record.setError("Config validation: " + String.join("; ", fail.errors()));
                }
            }
        }

        // --- Activate plugin: load entryClass and call register(api) ---
        activatePlugin(pluginId, manifest, candidate, record, registry, options);

        options.logger().info("Loaded plugin: " + pluginId);
    }

    // =========================================================================
    // Plugin activation — load entryClass, create PluginApi, call register()
    // =========================================================================

    /**
     * Activate a plugin by loading its entryClass and calling
     * {@code PluginDefinition.register(api)}.
     * <p>
     * This is the Java equivalent of TS's {@code plugin.register(api)} call.
     * The PluginApi carries registrar callbacks that populate the Registry
     * and PluginCommandProcessor.
     */
    private static void activatePlugin(
            String pluginId,
            PluginManifest.Manifest manifest,
            PluginDiscovery.PluginCandidate candidate,
            PluginRegistry.PluginRecord record,
            PluginRegistry registry,
            PluginLoadOptions options) {

        if (manifest == null || manifest.getEntryClass() == null
                || manifest.getEntryClass().isBlank()) {
            // No entryClass → declarative-only plugin (channels/providers via manifest)
            // Register manifest-level channels/providers as-is
            if (manifest != null) {
                record.setChannels(manifest.getChannels());
                record.setProviders(manifest.getProviders());
            }
            return;
        }

        String entryClassName = manifest.getEntryClass();
        try {
            // 1. Build classloader (plugin JAR + classpath)
            ClassLoader pluginClassLoader = buildPluginClassLoader(candidate.getRootDir());

            // 2. Load and instantiate the PluginDefinition
            Class<?> clazz = pluginClassLoader.loadClass(entryClassName);
            if (!PluginTypes.PluginDefinition.class.isAssignableFrom(clazz)) {
                log.warn("Plugin {} entryClass {} does not implement PluginDefinition",
                        pluginId, entryClassName);
                record.setStatus("error");
                record.setError("entryClass does not implement PluginDefinition");
                return;
            }
            PluginTypes.PluginDefinition definition = (PluginTypes.PluginDefinition) clazz.getDeclaredConstructor()
                    .newInstance();

            // 3. Build PluginApi with registrars wired to registry + commandProcessor
            PluginTypes.PluginApi api = buildPluginApi(
                    pluginId, candidate.getSource(), manifest, record, registry, options);

            // 4. Call register(api) — this is where the plugin registers
            // hooks/tools/commands
            definition.getRegister().accept(api);

            log.debug("Activated plugin {} via entryClass {}", pluginId, entryClassName);

        } catch (ClassNotFoundException e) {
            log.warn("Plugin {} entryClass {} not found on classpath", pluginId, entryClassName);
            record.setStatus("error");
            record.setError("entryClass not found: " + entryClassName);
        } catch (Exception e) {
            log.warn("Plugin {} activation failed: {}", pluginId, e.getMessage());
            record.setStatus("error");
            record.setError("activation failed: " + e.getMessage());
        }
    }

    /**
     * Build a PluginApi with registrars wired to the PluginRegistry
     * and optionally the PluginCommandProcessor.
     */
    private static PluginTypes.PluginApi buildPluginApi(
            String pluginId,
            String source,
            PluginManifest.Manifest manifest,
            PluginRegistry.PluginRecord record,
            PluginRegistry registry,
            PluginLoadOptions options) {

        // Hook registrar → PluginRegistry.registerHook
        java.util.function.Consumer<PluginTypes.HookRegistration> hookRegistrar = hookReg -> {
            registry.registerHook(PluginRegistry.PluginHookRegistration.builder()
                    .pluginId(pluginId)
                    .events(hookReg.getEvents())
                    .source(source)
                    .build());
            record.setHookCount(record.getHookCount() + 1);
        };

        // Tool registrar → PluginRegistry.registerTool
        java.util.function.Consumer<PluginTypes.ToolRegistration> toolRegistrar = toolReg -> {
            List<String> names = toolReg.getNames() != null ? toolReg.getNames()
                    : (toolReg.getName() != null ? List.of(toolReg.getName()) : List.of());
            registry.registerTool(PluginRegistry.PluginToolRegistration.builder()
                    .pluginId(pluginId)
                    .names(names)
                    .optional(toolReg.isOptional())
                    .source(source)
                    .build());
            // Update record's tool list
            var currentTools = new ArrayList<>(record.getTools());
            currentTools.addAll(names);
            record.setTools(currentTools);
        };

        // Command registrar → PluginRegistry.registerCommand +
        // PluginCommandProcessor.register
        java.util.function.Consumer<PluginTypes.CommandRegistration> commandRegistrar = cmdReg -> {
            registry.registerCommand(PluginRegistry.PluginCommandRegistration.builder()
                    .pluginId(pluginId)
                    .name(cmdReg.getName())
                    .description(cmdReg.getDescription())
                    .source(source)
                    .build());

            // Also register in PluginCommandProcessor if available
            if (options.commandProcessor() != null) {
                options.commandProcessor().register(pluginId,
                        PluginCommandProcessor.RegisteredCommand.builder()
                                .name(cmdReg.getName())
                                .description(cmdReg.getDescription())
                                .acceptsArgs(cmdReg.isAcceptsArgs())
                                .requireAuth(cmdReg.isRequireAuth())
                                .build());
            }

            // Update record's command list
            var currentCmds = new ArrayList<>(record.getCommands());
            currentCmds.add(cmdReg.getName());
            record.setCommands(currentCmds);
        };

        // Resolve plugin-specific config from entries
        Map<String, Object> pluginConfig = null;
        if (options.config() != null && options.config().getPlugins() != null
                && options.config().getPlugins().getEntries() != null) {
            var entry = options.config().getPlugins().getEntries().get(pluginId);
            if (entry != null) {
                pluginConfig = entry.getConfig();
            }
        }

        return PluginTypes.PluginApi.builder()
                .id(pluginId)
                .source(source)
                .config(options.config())
                .pluginConfig(pluginConfig)
                .logger(options.logger())
                .hookRegistrar(hookRegistrar)
                .toolRegistrar(toolRegistrar)
                .commandRegistrar(commandRegistrar)
                .build();
    }

    /**
     * Build a ClassLoader for the plugin directory.
     * Scans for JAR files and adds the directory itself for class files.
     */
    private static ClassLoader buildPluginClassLoader(String rootDir) {
        try {
            Path pluginPath = Path.of(rootDir);
            List<URL> urls = new ArrayList<>();

            // Add the plugin directory itself
            urls.add(pluginPath.toUri().toURL());

            // Add any JAR files in the plugin directory
            var jarFiles = java.nio.file.Files.list(pluginPath)
                    .filter(p -> p.toString().endsWith(".jar"))
                    .toList();
            for (var jar : jarFiles) {
                urls.add(jar.toUri().toURL());
            }

            // Add JARs in lib/ subdirectory if present
            Path libDir = pluginPath.resolve("lib");
            if (java.nio.file.Files.isDirectory(libDir)) {
                var libJars = java.nio.file.Files.list(libDir)
                        .filter(p -> p.toString().endsWith(".jar"))
                        .toList();
                for (var jar : libJars) {
                    urls.add(jar.toUri().toURL());
                }
            }

            return new URLClassLoader(urls.toArray(URL[]::new),
                    PluginLoader.class.getClassLoader());
        } catch (Exception e) {
            log.debug("Failed to build plugin classloader for {}: {}", rootDir, e.getMessage());
            return PluginLoader.class.getClassLoader();
        }
    }

    // =========================================================================
    // Slot management
    // =========================================================================

    private static void applySlots(PluginRegistry registry, PluginLoadOptions options) {
        for (var plugin : registry.getEnabledPlugins()) {
            if (plugin.getKind() != null) {
                String slotKey = PluginSlots.slotKeyForPluginKind(plugin.getKind());
                if (slotKey != null) {
                    log.debug("Plugin {} occupies exclusive slot: {}", plugin.getId(), slotKey);
                }
            }
        }
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
