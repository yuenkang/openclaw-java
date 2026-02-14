package com.openclaw.gateway.runtime;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.gateway.server.GatewayBroadcaster;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * Configuration hot-reload handlers — watches for config changes
 * and applies them to the running gateway.
 * Corresponds to TS {@code gateway/server-reload-handlers.ts}.
 */
@Slf4j
public class ReloadHandlers {

    private final ConfigService configService;
    private final GatewayLanes lanes;
    private final GatewayBroadcaster broadcaster;

    public ReloadHandlers(ConfigService configService,
            GatewayLanes lanes,
            GatewayBroadcaster broadcaster) {
        this.configService = configService;
        this.lanes = lanes;
        this.broadcaster = broadcaster;
    }

    /**
     * Handle a configuration reload event.
     * Re-reads the config and applies changes that can be hot-reloaded.
     */
    public void onConfigReload() {
        log.info("config reload triggered");

        try {
            OpenClawConfig cfg = configService.loadConfig();

            // Re-apply lane concurrency
            int mainConcurrent = 1;
            int cronConcurrent = 1;
            int subagentConcurrent = 2;
            if (cfg.getCron() != null && cfg.getCron().getMaxConcurrentRuns() != null) {
                cronConcurrent = cfg.getCron().getMaxConcurrentRuns();
            }
            lanes.applyFromConfig(mainConcurrent, cronConcurrent, subagentConcurrent);

            // Broadcast config change event
            broadcaster.broadcast("config.changed", Map.of(
                    "ts", System.currentTimeMillis()));

            log.info("config reload complete");
        } catch (Exception e) {
            log.error("config reload failed: {}", e.getMessage());
        }
    }

    /**
     * Determine the reload mode from config.
     *
     * @return "off", "restart", "hot", or "hybrid"
     */
    public String resolveReloadMode() {
        OpenClawConfig cfg = configService.loadConfig();
        if (cfg.getGateway() != null && cfg.getGateway().getReload() != null) {
            String mode = cfg.getGateway().getReload().getMode();
            if (mode != null)
                return mode;
        }
        return "off";
    }
}
