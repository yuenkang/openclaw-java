package com.openclaw.gateway.runtime;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.gateway.cron.CronService;
import com.openclaw.gateway.server.GatewayBroadcaster;
import lombok.extern.slf4j.Slf4j;

/**
 * Builds and configures the gateway-level cron service.
 * Corresponds to TS {@code gateway/server-cron.ts}.
 *
 * <p>
 * Bridges between the CronService and the gateway runtime:
 * resolves agent IDs, emits broadcast events, and runs isolated agent turns.
 */
@Slf4j
public class GatewayCronRunner {

    private final CronService cronService;
    private final GatewayBroadcaster broadcaster;
    private final ConfigService configService;
    private final boolean cronEnabled;

    public GatewayCronRunner(CronService cronService,
            GatewayBroadcaster broadcaster,
            ConfigService configService) {
        this.cronService = cronService;
        this.broadcaster = broadcaster;
        this.configService = configService;

        OpenClawConfig cfg = configService.loadConfig();
        this.cronEnabled = !"1".equals(System.getenv("OPENCLAW_SKIP_CRON"))
                && (cfg.getCron() == null || cfg.getCron().getEnabled() == null
                        || cfg.getCron().getEnabled());
    }

    /**
     * Start the cron service if enabled.
     */
    public void start() {
        if (!cronEnabled) {
            log.info("cron: disabled by config or OPENCLAW_SKIP_CRON");
            return;
        }
        cronService.start();
        log.info("cron: started");
    }

    /**
     * Stop the cron service.
     */
    public void stop() {
        cronService.stop();
        log.info("cron: stopped");
    }

    /**
     * Handle a cron event by broadcasting it to connected clients.
     */
    public void onCronEvent(String action, Object payload) {
        broadcaster.broadcast("cron",
                java.util.Map.of("action", action, "payload", payload),
                new GatewayBroadcaster.BroadcastOptions(true, null, null));
    }

    public boolean isCronEnabled() {
        return cronEnabled;
    }

    public CronService getCronService() {
        return cronService;
    }
}
