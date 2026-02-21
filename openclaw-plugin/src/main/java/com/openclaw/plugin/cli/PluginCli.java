package com.openclaw.plugin.cli;

import com.openclaw.plugin.PluginTypes;
import com.openclaw.plugin.loader.PluginLoader;
import com.openclaw.plugin.registry.PluginRegistry;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Plugin CLI integration — registers plugin-provided CLI commands
 * into the Spring Boot CLI or any command framework.
 * Corresponds to TypeScript's plugins/cli.ts (which uses Commander.js).
 * In Java, this would integrate with Spring Shell or Picocli.
 */
@Slf4j
public final class PluginCli {

    private PluginCli() {
    }

    @FunctionalInterface
    public interface CliRegistrar {
        void register(CliContext ctx);
    }

    public record CliRegistration(
            String pluginId,
            CliRegistrar registrar,
            List<String> commands,
            String source) {
    }

    public record CliContext(
            OpenClawConfig config,
            String workspaceDir,
            PluginTypes.PluginLogger logger) {
    }

    private static final List<CliRegistration> registrations = new ArrayList<>();

    /**
     * Register a plugin CLI command registrar.
     */
    public static void registerCliRegistrar(CliRegistration registration) {
        registrations.add(registration);
        log.debug("Registered plugin CLI registrar: {} (commands: {})",
                registration.pluginId(), registration.commands());
    }

    /**
     * Execute all registered CLI registrars.
     * In Spring Boot context, this is called during application startup.
     */
    public static void registerPluginCliCommands(OpenClawConfig config,
            String workspaceDir) {
        Set<String> existingCommands = new HashSet<>();
        PluginTypes.PluginLogger logger = new PluginTypes.PluginLogger() {
            @Override
            public void info(String message) {
                log.info(message);
            }

            @Override
            public void warn(String message) {
                log.warn(message);
            }

            @Override
            public void error(String message) {
                log.error(message);
            }
        };

        CliContext ctx = new CliContext(config, workspaceDir, logger);

        for (CliRegistration reg : registrations) {
            // Check for overlap with existing commands
            List<String> overlaps = reg.commands().stream()
                    .filter(existingCommands::contains)
                    .toList();
            if (!overlaps.isEmpty()) {
                log.debug("plugin CLI register skipped ({}): commands already registered ({})",
                        reg.pluginId(), String.join(", ", overlaps));
                continue;
            }
            try {
                reg.registrar().register(ctx);
                existingCommands.addAll(reg.commands());
            } catch (Exception e) {
                log.warn("plugin CLI register failed ({}): {}",
                        reg.pluginId(), e.getMessage());
            }
        }
    }

    /**
     * Clear all registrations (for testing).
     */
    public static void clear() {
        registrations.clear();
    }
}
