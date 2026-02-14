package com.openclaw.gateway.runtime;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Gateway startup orchestrator — coordinates the boot sequence.
 * Corresponds to TS {@code gateway/server-startup.ts}.
 *
 * <p>
 * Orchestrates: runtime config resolution → runtime state init →
 * lanes → cron → channels → boot.md → periodic tasks.
 */
@Slf4j
public class GatewayStartup {

    /** Default dedupe cleanup interval (60s). */
    private static final long DEDUPE_CLEANUP_INTERVAL_MS = 60_000;
    /** Default dedupe TTL (60s). */
    private static final long DEDUPE_MAX_AGE_MS = 60_000;

    private final GatewayRuntimeConfig runtimeConfig;
    private final GatewayRuntimeState runtimeState;
    private final GatewayLanes lanes;
    private final GatewayCronRunner cronRunner;
    private final GatewayChannelsBridge channelsBridge;
    private final GatewayShutdown shutdownHandler;
    private final ConfigService configService;

    private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(2, r -> {
        Thread t = new Thread(r, "gw-scheduler");
        t.setDaemon(true);
        return t;
    });

    public GatewayStartup(GatewayRuntimeConfig runtimeConfig,
            GatewayRuntimeState runtimeState,
            GatewayLanes lanes,
            GatewayCronRunner cronRunner,
            GatewayChannelsBridge channelsBridge,
            GatewayShutdown shutdownHandler,
            ConfigService configService) {
        this.runtimeConfig = runtimeConfig;
        this.runtimeState = runtimeState;
        this.lanes = lanes;
        this.cronRunner = cronRunner;
        this.channelsBridge = channelsBridge;
        this.shutdownHandler = shutdownHandler;
        this.configService = configService;
    }

    /**
     * Execute the full gateway startup sequence.
     */
    public void start() {
        log.info("gateway starting on {}:{}", runtimeConfig.getBindHost(), runtimeConfig.getPort());
        log.info("  auth mode: {}", runtimeConfig.getAuthMode());
        log.info("  control UI: {} ({})",
                runtimeConfig.isControlUiEnabled() ? "enabled" : "disabled",
                runtimeConfig.getControlUiBasePath());

        // 1. Apply lane concurrency from config
        applyLaneConcurrency();

        // 2. Start cron
        if (cronRunner != null) {
            cronRunner.start();
        }

        // 3. Schedule periodic dedupe cleanup
        var dedupeTask = scheduler.scheduleAtFixedRate(
                () -> runtimeState.cleanupDedupe(DEDUPE_MAX_AGE_MS),
                DEDUPE_CLEANUP_INTERVAL_MS,
                DEDUPE_CLEANUP_INTERVAL_MS,
                TimeUnit.MILLISECONDS);
        shutdownHandler.setDedupeCleanupTask(dedupeTask);

        // 4. Boot.md
        runBoot();

        log.info("gateway started successfully — ws://{}:{}/ws",
                runtimeConfig.getBindHost(), runtimeConfig.getPort());
    }

    /**
     * Apply lane concurrency from config.
     */
    private void applyLaneConcurrency() {
        OpenClawConfig cfg = configService.loadConfig();
        int mainConcurrent = 1;
        int cronConcurrent = 1;
        int subagentConcurrent = 2;

        if (cfg.getCron() != null && cfg.getCron().getMaxConcurrentRuns() != null) {
            cronConcurrent = cfg.getCron().getMaxConcurrentRuns();
        }
        // agent.maxConcurrent and subagent limits would be resolved here too

        lanes.applyFromConfig(mainConcurrent, cronConcurrent, subagentConcurrent);
        log.debug("lanes: main={} cron={} subagent={}",
                mainConcurrent, cronConcurrent, subagentConcurrent);
    }

    /**
     * Run BOOT.md if present.
     */
    private void runBoot() {
        String home = System.getProperty("user.home");
        String workspaceDir = home + "/.openclaw";

        var result = GatewayBootstrap.tryLoadBoot(workspaceDir);
        switch (result.status()) {
            case SKIPPED -> log.debug("boot: skipped ({})", result.reason());
            case RAN -> log.info("boot: completed");
            case FAILED -> log.warn("boot: failed ({})", result.reason());
        }
    }

    /**
     * Shutdown the gateway.
     */
    public void stop() {
        shutdownHandler.shutdown();
    }

    // ---- Getters ----

    public GatewayRuntimeConfig getRuntimeConfig() {
        return runtimeConfig;
    }

    public GatewayRuntimeState getRuntimeState() {
        return runtimeState;
    }

    public GatewayLanes getLanes() {
        return lanes;
    }

    public GatewayCronRunner getCronRunner() {
        return cronRunner;
    }

    public GatewayChannelsBridge getChannelsBridge() {
        return channelsBridge;
    }

    public ScheduledExecutorService getScheduler() {
        return scheduler;
    }
}
