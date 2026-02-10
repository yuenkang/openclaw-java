package com.openclaw.plugin.registry;

import com.openclaw.plugin.OpenClawPlugin;
import com.openclaw.plugin.loader.PluginLoader;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Manages registered plugin components (tools, hooks, commands).
 * Corresponds to TypeScript's plugins/registry.ts.
 */
@Slf4j
public class PluginRegistry {

    /** Registered components by type → name → handler. */
    private final Map<String, Map<String, Object>> components = new ConcurrentHashMap<>();

    private final PluginLoader pluginLoader;

    public PluginRegistry(PluginLoader pluginLoader) {
        this.pluginLoader = pluginLoader;
    }

    /**
     * Initialize all plugins by calling their register() methods.
     */
    public void initializeAll() {
        List<OpenClawPlugin> plugins = pluginLoader.loadAll();

        OpenClawPlugin.OpenClawPluginApi api = new OpenClawPlugin.OpenClawPluginApi() {
            @Override
            public void registerTool(String name, Object handler) {
                register("tool", name, handler);
            }

            @Override
            public void registerHook(String event, Object handler) {
                register("hook", event, handler);
            }

            @Override
            public void registerCommand(String name, Object handler) {
                register("command", name, handler);
            }
        };

        for (OpenClawPlugin plugin : plugins) {
            try {
                plugin.register(api);
                log.info("Initialized plugin: {}", plugin.getName());
            } catch (Exception e) {
                log.error("Failed to initialize plugin {}: {}", plugin.getName(), e.getMessage(), e);
            }
        }

        log.info("Initialized {} plugins, registered {} components",
                plugins.size(), countComponents());
    }

    /**
     * Register a component.
     */
    public void register(String type, String name, Object handler) {
        components.computeIfAbsent(type, k -> new ConcurrentHashMap<>()).put(name, handler);
        log.debug("Registered {} component: {}", type, name);
    }

    /**
     * Get a component by type and name.
     */
    @SuppressWarnings("unchecked")
    public <T> Optional<T> get(String type, String name) {
        Map<String, Object> typeMap = components.get(type);
        if (typeMap == null)
            return Optional.empty();
        return Optional.ofNullable((T) typeMap.get(name));
    }

    /**
     * List all component names of a given type.
     */
    public Set<String> list(String type) {
        Map<String, Object> typeMap = components.get(type);
        return typeMap != null ? Collections.unmodifiableSet(typeMap.keySet()) : Set.of();
    }

    private int countComponents() {
        return components.values().stream().mapToInt(Map::size).sum();
    }
}
