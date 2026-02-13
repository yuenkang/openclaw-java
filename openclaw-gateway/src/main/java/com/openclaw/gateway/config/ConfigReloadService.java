package com.openclaw.gateway.config;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Watches the configuration file for changes and triggers reload callbacks.
 * <p>
 * Corresponds to TypeScript's {@code config-reload.ts} (389 lines).
 * Uses Java's WatchService instead of chokidar.
 */
@Slf4j
public class ConfigReloadService implements AutoCloseable {

    private final ConfigService configService;
    private final List<ReloadCallback> callbacks = new ArrayList<>();
    private final AtomicBoolean running = new AtomicBoolean(false);
    private volatile Thread watchThread;
    private volatile WatchService watchService;

    /** Debounce: at most one reload per 500 ms. */
    private static final long DEBOUNCE_MS = 500;

    @FunctionalInterface
    public interface ReloadCallback {
        void onConfigReloaded(OpenClawConfig oldConfig, OpenClawConfig newConfig, ConfigDiff diff);
    }

    public ConfigReloadService(ConfigService configService) {
        this.configService = configService;
    }

    /**
     * Register a callback to be invoked when config changes.
     */
    public void addCallback(ReloadCallback callback) {
        callbacks.add(callback);
    }

    /**
     * Start watching the config file for changes.
     */
    public void start() {
        if (!running.compareAndSet(false, true)) {
            log.warn("ConfigReloadService is already running");
            return;
        }

        Path configPath = configService.getConfigPath();
        Path parentDir = configPath.getParent();
        String fileName = configPath.getFileName().toString();

        try {
            watchService = FileSystems.getDefault().newWatchService();
            parentDir.register(watchService, StandardWatchEventKinds.ENTRY_MODIFY);
        } catch (IOException e) {
            log.error("Failed to start config file watcher: {}", e.getMessage());
            running.set(false);
            return;
        }

        watchThread = new Thread(() -> {
            log.info("Config file watcher started for: {}", configPath);
            OpenClawConfig lastConfig = configService.loadConfig();
            long lastReloadAt = 0;

            while (running.get()) {
                try {
                    WatchKey key = watchService.take();
                    boolean relevant = false;
                    for (var event : key.pollEvents()) {
                        if (event.context() instanceof Path changedPath
                                && fileName.equals(changedPath.toString())) {
                            relevant = true;
                        }
                    }
                    key.reset();

                    if (!relevant)
                        continue;

                    // Debounce
                    long now = System.currentTimeMillis();
                    if (now - lastReloadAt < DEBOUNCE_MS)
                        continue;
                    lastReloadAt = now;

                    // Small delay for file write to complete
                    Thread.sleep(50);

                    log.info("Config file changed, reloading...");
                    OpenClawConfig newConfig = configService.reloadConfig();
                    ConfigDiff diff = computeDiff(lastConfig, newConfig);

                    for (var cb : callbacks) {
                        try {
                            cb.onConfigReloaded(lastConfig, newConfig, diff);
                        } catch (Exception e) {
                            log.error("Config reload callback failed: {}", e.getMessage(), e);
                        }
                    }

                    lastConfig = newConfig;
                    log.info("Config reloaded successfully (changes: {})", diff.changedPaths());

                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                } catch (ClosedWatchServiceException e) {
                    break;
                } catch (Exception e) {
                    log.error("Config watcher error: {}", e.getMessage(), e);
                }
            }

            log.info("Config file watcher stopped");
        }, "config-reload-watcher");
        watchThread.setDaemon(true);
        watchThread.start();
    }

    /**
     * Stop watching for config changes.
     */
    @Override
    public void close() {
        if (running.compareAndSet(true, false)) {
            try {
                if (watchService != null) {
                    watchService.close();
                }
            } catch (IOException e) {
                log.warn("Error closing watch service: {}", e.getMessage());
            }
            if (watchThread != null) {
                watchThread.interrupt();
            }
        }
    }

    // ─── Config diff ────────────────────────────────────────────────────

    /**
     * Shallow diff of two configs — reports which top-level sections changed.
     */
    static ConfigDiff computeDiff(OpenClawConfig oldCfg, OpenClawConfig newCfg) {
        Set<String> changed = new LinkedHashSet<>();
        if (!Objects.equals(oldCfg.getModels(), newCfg.getModels()))
            changed.add("model");
        if (!Objects.equals(oldCfg.getAgents(), newCfg.getAgents()))
            changed.add("agents");
        if (!Objects.equals(oldCfg.getGateway(), newCfg.getGateway()))
            changed.add("gateway");
        if (!Objects.equals(oldCfg.getSession(), newCfg.getSession()))
            changed.add("session");
        if (!Objects.equals(oldCfg.getModels(), newCfg.getModels()))
            changed.add("models");
        if (!Objects.equals(oldCfg.getChannels(), newCfg.getChannels()))
            changed.add("channels");
        if (!Objects.equals(oldCfg.getCron(), newCfg.getCron()))
            changed.add("cron");
        if (!Objects.equals(oldCfg.getTools(), newCfg.getTools()))
            changed.add("tools");
        // sandbox config is per-agent, not top-level
        if (!Objects.equals(oldCfg.getLogging(), newCfg.getLogging()))
            changed.add("logging");
        if (!Objects.equals(oldCfg.getPlugins(), newCfg.getPlugins()))
            changed.add("plugins");
        if (!Objects.equals(oldCfg.getAuth(), newCfg.getAuth()))
            changed.add("auth");
        return new ConfigDiff(List.copyOf(changed));
    }

    public record ConfigDiff(List<String> changedPaths) {
        public boolean hasChange(String path) {
            return changedPaths.contains(path);
        }
    }
}
