package com.openclaw.app.config;

import com.openclaw.agent.plugins.PluginLoader;
import com.openclaw.agent.plugins.PluginRegistry;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Application-level plugin bootstrap — loads and provides PluginRegistry
 * as a shared singleton, independent of any specific channel.
 */
@Slf4j
@Component
public class PluginBootstrap {

    private final ConfigService configService;
    private PluginRegistry pluginRegistry;

    public PluginBootstrap(ConfigService configService) {
        this.configService = configService;
    }

    @PostConstruct
    public void init() {
        try {
            OpenClawConfig config = configService.loadConfig();
            this.pluginRegistry = PluginLoader.loadPlugins(
                    new PluginLoader.PluginLoadOptions(config, null, null, false));
            log.info("Plugins loaded: {} total, {} enabled",
                    pluginRegistry.getPlugins().size(),
                    pluginRegistry.getEnabledPlugins().size());
        } catch (Exception e) {
            log.warn("Failed to load plugins: {}", e.getMessage());
            this.pluginRegistry = new PluginRegistry();
        }
    }

    /**
     * Get the shared plugin registry.
     */
    public PluginRegistry getPluginRegistry() {
        return pluginRegistry;
    }
}
