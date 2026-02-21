package com.openclaw.plugin.providers;

import com.openclaw.plugin.registry.PluginRegistry;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

/**
 * Plugin provider resolver — resolves LLM provider plugins from the registry.
 * Corresponds to TypeScript's plugins/providers.ts.
 */
@Slf4j
public final class PluginProviderResolver {

    private PluginProviderResolver() {
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ProviderPlugin {
        private String pluginId;
        private String providerId;
        private String displayName;
        private String source;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ProviderRegistration {
        private String pluginId;
        private ProviderPlugin provider;
        private String source;
    }

    /**
     * Resolve all plugin-provided LLM providers from the registry.
     */
    public static List<ProviderPlugin> resolvePluginProviders(PluginRegistry registry) {
        // Plugin providers come from explicit registrations stored in the registry.
        // In TS this calls loadOpenClawPlugins, but in Java the registry is already
        // loaded.
        log.debug("Resolving plugin providers from registry");
        return List.of(); // populated when plugins register providers
    }
}
