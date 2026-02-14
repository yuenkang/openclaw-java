package com.openclaw.gateway.runtime;

import com.openclaw.gateway.server.GatewayBroadcaster;
import com.openclaw.gateway.websocket.GatewayConnection;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.socket.CloseStatus;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;

/**
 * Gateway graceful shutdown handler.
 * Corresponds to TS {@code gateway/server-close.ts}.
 *
 * <p>
 * Orchestrates orderly shutdown by:
 * 1. Broadcasting shutdown event to clients
 * 2. Stopping channels, cron, heartbeat
 * 3. Closing WebSocket connections
 * 4. Clearing runtime state
 */
@Slf4j
public class GatewayShutdown {

    private final GatewayRuntimeState runtimeState;
    private final GatewayBroadcaster broadcaster;
    private final GatewayCronRunner cronRunner;
    private final ScheduledExecutorService scheduler;

    // Optional cleanup handles (may be null if not initialized)
    private volatile ScheduledFuture<?> dedupeCleanupTask;
    private volatile ScheduledFuture<?> healthCheckTask;
    private volatile Runnable channelStopCallback;

    public GatewayShutdown(GatewayRuntimeState runtimeState,
            GatewayBroadcaster broadcaster,
            GatewayCronRunner cronRunner,
            ScheduledExecutorService scheduler) {
        this.runtimeState = runtimeState;
        this.broadcaster = broadcaster;
        this.cronRunner = cronRunner;
        this.scheduler = scheduler;
    }

    // ---- Setters for optional cleanup handles ----

    public void setDedupeCleanupTask(ScheduledFuture<?> task) {
        this.dedupeCleanupTask = task;
    }

    public void setHealthCheckTask(ScheduledFuture<?> task) {
        this.healthCheckTask = task;
    }

    public void setChannelStopCallback(Runnable callback) {
        this.channelStopCallback = callback;
    }

    /**
     * Execute graceful shutdown.
     *
     * @param reason            human-readable shutdown reason
     * @param restartExpectedMs if non-null, tells clients when restart is expected
     */
    public void shutdown(String reason, Long restartExpectedMs) {
        String effectiveReason = (reason != null && !reason.trim().isEmpty())
                ? reason.trim()
                : "gateway stopping";

        log.info("gateway shutdown: {}", effectiveReason);

        // 1. Stop channels
        if (channelStopCallback != null) {
            try {
                channelStopCallback.run();
            } catch (Exception e) {
                log.warn("error stopping channels: {}", e.getMessage());
            }
        }

        // 2. Stop cron
        if (cronRunner != null) {
            try {
                cronRunner.stop();
            } catch (Exception e) {
                log.warn("error stopping cron: {}", e.getMessage());
            }
        }

        // 3. Broadcast shutdown event
        broadcaster.broadcast("shutdown", Map.of(
                "reason", effectiveReason,
                "restartExpectedMs", restartExpectedMs != null ? restartExpectedMs : 0));

        // 4. Cancel scheduled tasks
        cancelTask(dedupeCleanupTask);
        cancelTask(healthCheckTask);

        // 5. Clear runtime state
        runtimeState.clear();

        // 6. Close all WebSocket connections
        Set<GatewayConnection> clients = runtimeState.getClients();
        for (GatewayConnection conn : clients) {
            try {
                if (conn.getSession().isOpen()) {
                    conn.getSession().close(
                            new CloseStatus(1012, "service restart"));
                }
            } catch (Exception e) {
                log.debug("error closing ws {}: {}", conn.getConnectionId(), e.getMessage());
            }
        }
        clients.clear();

        // 7. Shutdown scheduler
        if (scheduler != null && !scheduler.isShutdown()) {
            scheduler.shutdown();
        }

        log.info("gateway shutdown complete");
    }

    /**
     * Shutdown with default reason.
     */
    public void shutdown() {
        shutdown("gateway stopping", null);
    }

    private void cancelTask(ScheduledFuture<?> task) {
        if (task != null && !task.isCancelled()) {
            task.cancel(false);
        }
    }
}
