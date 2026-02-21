package com.openclaw.app.config;

import com.openclaw.app.commands.CommandProcessor;
import com.openclaw.app.commands.CommandResult;
import com.openclaw.plugin.PluginTypes;
import com.openclaw.plugin.cli.PluginCli;
import com.openclaw.plugin.commands.PluginCommandProcessor;
import com.openclaw.plugin.hooks.PluginHookRunner;
import com.openclaw.plugin.hooks.PluginHookRunnerGlobal;
import com.openclaw.plugin.http.PluginHttpRegistry;
import com.openclaw.plugin.install.PluginInstallRecord;
import com.openclaw.plugin.install.PluginInstaller;
import com.openclaw.plugin.install.PluginUpdater;
import com.openclaw.plugin.loader.PluginLoader;
import com.openclaw.plugin.providers.PluginProviderResolver;
import com.openclaw.plugin.registry.PluginRegistry;
import com.openclaw.plugin.runtime.PluginRuntime;
import com.openclaw.plugin.services.PluginServiceManager;
import com.openclaw.plugin.status.PluginStatus;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

/**
 * Application-level plugin bootstrap — loads plugins and provides shared
 * PluginRegistry, PluginHookRunner, PluginCommandProcessor,
 * PluginServiceManager, PluginHttpRegistry as singletons.
 */
@Slf4j
@Component
public class PluginBootstrap {

    private final ConfigService configService;
    private final ApplicationContext applicationContext;
    private PluginRegistry pluginRegistry;
    private PluginHookRunner hookRunner;
    private PluginCommandProcessor commandProcessor;
    private PluginServiceManager serviceManager;
    private PluginServiceManager.ServicesHandle servicesHandle;
    private PluginHttpRegistry httpRegistry;

    public PluginBootstrap(ConfigService configService,
            ApplicationContext applicationContext) {
        this.configService = configService;
        this.applicationContext = applicationContext;
    }

    @PostConstruct
    public void init() {
        try {
            OpenClawConfig config = configService.loadConfig();

            // 1. Create command processor (needed during plugin loading for command
            // registration)
            this.commandProcessor = new PluginCommandProcessor();

            // 2. Load + activate plugins (pass commandProcessor so plugins can register
            // commands)
            this.pluginRegistry = PluginLoader.loadPlugins(
                    new PluginLoader.PluginLoadOptions(config, null, null, false, commandProcessor));
            log.info("Plugins loaded: {} total, {} enabled",
                    pluginRegistry.getPlugins().size(),
                    pluginRegistry.getEnabledPlugins().size());

            // 3. Set global runtime state
            PluginRuntime.setActivePluginRegistry(pluginRegistry, "boot");

            // 4. Create hook runner + initialize global singleton
            this.hookRunner = new PluginHookRunner(pluginRegistry);
            PluginHookRunnerGlobal.initialize(pluginRegistry);

            // 5. Create service manager
            this.serviceManager = new PluginServiceManager();

            // 6. Create HTTP registry
            this.httpRegistry = new PluginHttpRegistry();

            // 7. Resolve plugin providers (log count)
            var providers = PluginProviderResolver.resolvePluginProviders(pluginRegistry);
            if (!providers.isEmpty()) {
                log.info("Plugin providers resolved: {}", providers.size());
            }

            // 8. Register plugin CLI commands + bridge to app CommandProcessor
            PluginCli.registerPluginCliCommands(config, null);
            bridgePluginCommands();

            // 9. Fire gateway_start hook
            hookRunner.runGatewayStart(
                    PluginTypes.GatewayStartEvent.builder()
                            .port(0) // actual port set by Spring
                            .build());

            log.info("Plugin system initialized: hookRunner, commandProcessor, " +
                    "serviceManager, httpRegistry, runtime");
        } catch (Exception e) {
            log.warn("Failed to load plugins: {}", e.getMessage());
            this.pluginRegistry = new PluginRegistry();
            this.hookRunner = PluginHookRunner.NOOP;
            this.commandProcessor = new PluginCommandProcessor();
            this.serviceManager = new PluginServiceManager();
            this.httpRegistry = new PluginHttpRegistry();
            PluginRuntime.setActivePluginRegistry(pluginRegistry, null);
        }
    }

    @PreDestroy
    public void shutdown() {
        // Stop plugin services
        if (servicesHandle != null) {
            servicesHandle.stop();
        }
        if (serviceManager != null) {
            serviceManager.stopAll();
        }
        // Fire gateway_stop hook
        if (hookRunner != null) {
            hookRunner.runGatewayStop(
                    PluginTypes.GatewayStopEvent.builder()
                            .reason("shutdown")
                            .build());
        }
        // Reset global state
        PluginHookRunnerGlobal.reset();
        PluginCli.clear();
        log.info("Plugin system shut down");
    }

    // --- Getters for Spring injection ---

    public PluginRegistry getPluginRegistry() {
        return pluginRegistry;
    }

    public PluginHookRunner getHookRunner() {
        return hookRunner;
    }

    public PluginCommandProcessor getCommandProcessor() {
        return commandProcessor;
    }

    public PluginServiceManager getServiceManager() {
        return serviceManager;
    }

    public PluginHttpRegistry getHttpRegistry() {
        return httpRegistry;
    }

    /**
     * Build a plugin status report for diagnostics.
     */
    public PluginStatus.StatusReport getStatusReport() {
        OpenClawConfig config = configService.loadConfig();
        return PluginStatus.buildReport(config, null);
    }

    /**
     * Install a plugin from a JAR file.
     * Records the installation in config on success.
     */
    public PluginInstaller.InstallResult installPlugin(String jarPath) {
        PluginInstaller.InstallResult result = PluginInstaller.installFromJar(jarPath, null);
        if (result.isOk()) {
            OpenClawConfig config = configService.loadConfig();
            PluginInstallRecord.recordInstall(config, PluginInstallRecord.InstallUpdate.builder()
                    .pluginId(result.getPluginId())
                    .source("jar")
                    .version(result.getVersion())
                    .build());
            log.info("Recorded plugin install: {} v{}", result.getPluginId(), result.getVersion());
        }
        return result;
    }

    /**
     * Update installed plugins.
     */
    public PluginUpdater.UpdateSummary updatePlugins() {
        OpenClawConfig config = configService.loadConfig();
        return PluginUpdater.updateInstalledPlugins(config, null, null, null);
    }

    /**
     * Bridge plugin-registered commands to Spring CommandProcessor.
     * Uses ApplicationContext.getBean() to avoid circular dependency.
     * For each command in PluginCommandProcessor, creates a dynamic
     * handler in the app-level CommandProcessor.
     */
    private void bridgePluginCommands() {
        if (commandProcessor == null)
            return;
        var cmds = commandProcessor.listCommands();
        if (cmds.isEmpty())
            return;

        try {
            CommandProcessor appCmdProcessor = applicationContext.getBean(CommandProcessor.class);
            for (var cmd : cmds) {
                String name = cmd.getName();
                appCmdProcessor.registerDynamic(name, (args, ctx) -> {
                    var match = commandProcessor.match("/" + name +
                            (args.isEmpty() ? "" : " " + args));
                    if (match == null) {
                        return CommandResult.text("❌ Plugin command /" + name + " not matched");
                    }
                    var result = commandProcessor.execute(match,
                            ctx.senderId(), null, ctx.isAuthorizedSender(),
                            "/" + name + (args.isEmpty() ? "" : " " + args),
                            ctx.config());
                    return CommandResult.text(result != null ? result.getText()
                            : "✅ 命令执行完成");
                });
            }
            log.info("Bridged {} plugin commands to app CommandProcessor", cmds.size());
        } catch (Exception e) {
            log.debug("CommandProcessor not available for bridging: {}", e.getMessage());
        }
    }
}
