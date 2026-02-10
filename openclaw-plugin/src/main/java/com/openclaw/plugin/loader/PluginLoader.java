package com.openclaw.plugin.loader;

import com.openclaw.plugin.OpenClawPlugin;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Plugin loader using Java SPI (ServiceLoader).
 * Discovers plugins on the classpath and initializes them.
 * Corresponds to TypeScript's plugins/loader.ts.
 */
@Slf4j
public class PluginLoader {

    private final List<OpenClawPlugin> loadedPlugins = new ArrayList<>();
    private boolean initialized = false;

    /**
     * Discover and load all plugins via ServiceLoader.
     */
    public List<OpenClawPlugin> loadAll() {
        if (initialized) {
            return Collections.unmodifiableList(loadedPlugins);
        }

        ServiceLoader<OpenClawPlugin> loader = ServiceLoader.load(OpenClawPlugin.class);

        for (OpenClawPlugin plugin : loader) {
            try {
                loadedPlugins.add(plugin);
                log.info("Discovered plugin: {} v{} by {}",
                        plugin.getName(), plugin.getVersion(), plugin.getAuthor());
            } catch (Exception e) {
                log.error("Failed to load plugin: {}", e.getMessage(), e);
            }
        }

        initialized = true;
        log.info("Loaded {} plugins", loadedPlugins.size());
        return Collections.unmodifiableList(loadedPlugins);
    }

    /**
     * Get a loaded plugin by name.
     */
    public Optional<OpenClawPlugin> get(String name) {
        return loadedPlugins.stream()
                .filter(p -> p.getName().equalsIgnoreCase(name))
                .findFirst();
    }

    /**
     * Get all loaded plugins.
     */
    public List<OpenClawPlugin> getAll() {
        return Collections.unmodifiableList(loadedPlugins);
    }

    public int size() {
        return loadedPlugins.size();
    }
}
