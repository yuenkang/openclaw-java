package com.openclaw.plugin.runtime;

import com.openclaw.plugin.registry.PluginRegistry;
import lombok.extern.slf4j.Slf4j;

/**
 * Plugin runtime state — manages the active PluginRegistry singleton
 * (analogous to module-scoped state in JS).
 * Corresponds to TypeScript's plugins/runtime.ts.
 */
@Slf4j
public final class PluginRuntime {

    private static volatile PluginRegistry activeRegistry = new PluginRegistry();
    private static volatile String cacheKey;

    private PluginRuntime() {
    }

    public static void setActivePluginRegistry(PluginRegistry registry, String key) {
        activeRegistry = registry;
        cacheKey = key;
    }

    public static PluginRegistry getActivePluginRegistry() {
        return activeRegistry;
    }

    /**
     * Get or create the active registry (never null).
     */
    public static PluginRegistry requireActivePluginRegistry() {
        PluginRegistry r = activeRegistry;
        if (r == null) {
            r = new PluginRegistry();
            activeRegistry = r;
        }
        return r;
    }

    public static String getActivePluginRegistryKey() {
        return cacheKey;
    }
}
