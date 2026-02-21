package com.openclaw.plugin.install;

import com.openclaw.common.config.OpenClawConfig;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;
import java.util.*;

/**
 * Plugin install record utilities — records plugin installations in config.
 * Corresponds to TypeScript's plugins/installs.ts.
 */
public final class PluginInstallRecord {

    private PluginInstallRecord() {
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class InstallUpdate {
        private String pluginId;
        private String source; // "jar", "dir", "maven"
        private String version;
        private String installedAt;
    }

    /**
     * Record a plugin installation in the config — enables the plugin
     * and stores its config entry with version info.
     */
    public static void recordInstall(OpenClawConfig config, InstallUpdate update) {
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

        OpenClawConfig.PluginEntryConfig entry = entries.get(update.getPluginId());
        if (entry == null) {
            entry = new OpenClawConfig.PluginEntryConfig();
            entries.put(update.getPluginId(), entry);
        }
        entry.setEnabled(true);

        // Store install metadata in config
        Map<String, Object> entryConfig = entry.getConfig();
        if (entryConfig == null) {
            entryConfig = new HashMap<>();
            entry.setConfig(entryConfig);
        }
        entryConfig.put("_installSource", update.getSource());
        entryConfig.put("_installVersion", update.getVersion());
        entryConfig.put("_installedAt", update.getInstalledAt() != null
                ? update.getInstalledAt()
                : Instant.now().toString());
    }
}
