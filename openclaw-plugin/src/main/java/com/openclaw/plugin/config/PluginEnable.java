package com.openclaw.plugin.config;

import com.openclaw.common.config.OpenClawConfig;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.*;

/**
 * Plugin enable/disable configuration utilities.
 * Corresponds to TypeScript's plugins/enable.ts.
 */
public final class PluginEnable {

    private PluginEnable() {
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class EnableResult {
        private boolean enabled;
        private String reason;
    }

    /**
     * Check if a plugin should be enabled in the configuration.
     */
    public static EnableResult enablePluginInConfig(OpenClawConfig config, String pluginId) {
        OpenClawConfig.PluginsConfig plugins = config.getPlugins();
        if (plugins == null) {
            return new EnableResult(true, null);
        }

        Map<String, OpenClawConfig.PluginEntryConfig> entries = plugins.getEntries();
        if (entries == null) {
            return new EnableResult(true, null);
        }

        OpenClawConfig.PluginEntryConfig entry = entries.get(pluginId);
        if (entry != null && Boolean.FALSE.equals(entry.getEnabled())) {
            return new EnableResult(false, "disabled in config");
        }

        return new EnableResult(true, null);
    }

    /**
     * Set a plugin's enabled state in config.
     */
    public static void setPluginEnabled(OpenClawConfig config, String pluginId, boolean enabled) {
        OpenClawConfig.PluginsConfig plugins = config.getPlugins();
        if (plugins == null) {
            plugins = new OpenClawConfig.PluginsConfig();
            config.setPlugins(plugins);
        }

        Map<String, OpenClawConfig.PluginEntryConfig> entries = plugins.getEntries();
        if (entries == null) {
            entries = new HashMap<>();
            plugins.setEntries(entries);
        }

        OpenClawConfig.PluginEntryConfig entry = entries.get(pluginId);
        if (entry == null) {
            entry = new OpenClawConfig.PluginEntryConfig();
            entries.put(pluginId, entry);
        }
        entry.setEnabled(enabled);
    }
}
