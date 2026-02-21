package com.openclaw.plugin.status;

import com.openclaw.plugin.loader.PluginLoader;
import com.openclaw.plugin.registry.PluginRegistry;
import com.openclaw.common.config.OpenClawConfig;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Plugin status report builder — builds a status report of all loaded plugins.
 * Corresponds to TypeScript's plugins/status.ts.
 */
@Slf4j
public final class PluginStatus {

    private PluginStatus() {
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class StatusReport {
        private PluginRegistry registry;
        private String workspaceDir;
    }

    /**
     * Build a plugin status report.
     */
    public static StatusReport buildReport(OpenClawConfig config, String workspaceDir) {
        PluginRegistry registry;
        try {
            registry = PluginLoader.loadPlugins(
                    new PluginLoader.PluginLoadOptions(config, workspaceDir, null, false));
        } catch (Exception e) {
            log.warn("Failed to load plugins for status: {}", e.getMessage());
            registry = new PluginRegistry();
        }
        return StatusReport.builder()
                .registry(registry)
                .workspaceDir(workspaceDir)
                .build();
    }
}
