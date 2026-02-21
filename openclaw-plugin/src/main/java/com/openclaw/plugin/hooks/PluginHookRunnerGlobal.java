package com.openclaw.plugin.hooks;

import com.openclaw.plugin.registry.PluginRegistry;
import lombok.extern.slf4j.Slf4j;

/**
 * Global singleton PluginHookRunner — initialized when plugins are loaded,
 * callable from anywhere in the codebase.
 * Corresponds to TypeScript's plugins/hook-runner-global.ts.
 */
@Slf4j
public final class PluginHookRunnerGlobal {

    private static volatile PluginHookRunner globalRunner;
    private static volatile PluginRegistry globalRegistry;

    private PluginHookRunnerGlobal() {
    }

    /**
     * Initialize the global hook runner with a plugin registry.
     * Called once when plugins are loaded during gateway startup.
     */
    public static void initialize(PluginRegistry registry) {
        globalRegistry = registry;
        globalRunner = new PluginHookRunner(registry, true);
        int hookCount = registry.getHooks().size();
        if (hookCount > 0) {
            log.info("hook runner initialized with {} registered hooks", hookCount);
        }
    }

    /**
     * Get the global hook runner, or NOOP if not initialized.
     */
    public static PluginHookRunner get() {
        PluginHookRunner r = globalRunner;
        return r != null ? r : PluginHookRunner.NOOP;
    }

    /**
     * Get the global plugin registry.
     */
    public static PluginRegistry getRegistry() {
        return globalRegistry;
    }

    /**
     * Check if any hooks are registered for a given hook name.
     */
    public static boolean hasHooks(com.openclaw.plugin.PluginTypes.PluginHookName hookName) {
        PluginHookRunner r = globalRunner;
        return r != null && r.hasHooks(hookName);
    }

    /**
     * Reset the global hook runner (for testing).
     */
    public static void reset() {
        globalRunner = null;
        globalRegistry = null;
    }
}
