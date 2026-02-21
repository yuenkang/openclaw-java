package com.openclaw.plugin.services;

import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.plugin.PluginTypes;
import com.openclaw.plugin.registry.PluginRegistry;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Plugin service manager — starts and stops long-running plugin background
 * services.
 * Corresponds to TypeScript's plugins/services.ts startPluginServices().
 */
@Slf4j
public class PluginServiceManager {

    // =========================================================================
    // Types
    // =========================================================================

    /**
     * A plugin service that can be started and stopped.
     */
    public interface PluginService {
        String getId();

        void start(ServiceContext ctx) throws Exception;

        default void stop(ServiceContext ctx) throws Exception {
        }
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ServiceContext {
        private OpenClawConfig config;
        private String workspaceDir;
        private String stateDir;
        private PluginTypes.PluginLogger logger;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ServiceRegistration {
        private String pluginId;
        private PluginService service;
        private String source;
    }

    /**
     * Handle for stopping all started services.
     */
    public interface ServicesHandle {
        void stop();
    }

    // =========================================================================
    // State
    // =========================================================================

    private final List<ServiceRegistration> registrations = new ArrayList<>();
    private final List<RunningService> running = new ArrayList<>();

    private record RunningService(String id, PluginService service, ServiceContext ctx) {
    }

    // =========================================================================
    // Registration
    // =========================================================================

    public void register(String pluginId, PluginService service, String source) {
        registrations.add(ServiceRegistration.builder()
                .pluginId(pluginId)
                .service(service)
                .source(source)
                .build());
        log.debug("Registered plugin service: {} (plugin: {})", service.getId(), pluginId);
    }

    // =========================================================================
    // Lifecycle
    // =========================================================================

    /**
     * Start all registered plugin services.
     */
    public ServicesHandle startAll(OpenClawConfig config, String workspaceDir,
            String stateDir) {
        PluginTypes.PluginLogger logger = createLogger();

        for (var reg : registrations) {
            ServiceContext ctx = ServiceContext.builder()
                    .config(config)
                    .workspaceDir(workspaceDir)
                    .stateDir(stateDir)
                    .logger(logger)
                    .build();
            try {
                reg.getService().start(ctx);
                running.add(new RunningService(reg.getService().getId(), reg.getService(), ctx));
                log.info("Started plugin service: {} (plugin: {})",
                        reg.getService().getId(), reg.getPluginId());
            } catch (Exception e) {
                log.error("Plugin service failed ({}): {}",
                        reg.getService().getId(), e.getMessage(), e);
            }
        }

        return this::stopAll;
    }

    /**
     * Stop all running plugin services (in reverse order).
     */
    public void stopAll() {
        List<RunningService> reversed = new ArrayList<>(running);
        Collections.reverse(reversed);
        for (var entry : reversed) {
            try {
                entry.service().stop(entry.ctx());
                log.info("Stopped plugin service: {}", entry.id());
            } catch (Exception e) {
                log.warn("Plugin service stop failed ({}): {}", entry.id(), e.getMessage());
            }
        }
        running.clear();
    }

    // =========================================================================
    // Queries
    // =========================================================================

    public List<ServiceRegistration> getRegistrations() {
        return List.copyOf(registrations);
    }

    public int getRunningCount() {
        return running.size();
    }

    public void clear() {
        stopAll();
        registrations.clear();
    }

    // =========================================================================
    // Internal
    // =========================================================================

    private PluginTypes.PluginLogger createLogger() {
        return new PluginTypes.PluginLogger() {
            @Override
            public void info(String message) {
                log.info("[plugin-service] {}", message);
            }

            @Override
            public void warn(String message) {
                log.warn("[plugin-service] {}", message);
            }

            @Override
            public void error(String message) {
                log.error("[plugin-service] {}", message);
            }
        };
    }
}
