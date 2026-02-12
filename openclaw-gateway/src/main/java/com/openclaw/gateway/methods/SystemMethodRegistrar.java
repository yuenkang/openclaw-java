package com.openclaw.gateway.methods;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.gateway.cron.CronJob;
import com.openclaw.gateway.cron.CronService;
import com.openclaw.gateway.websocket.GatewayConnection;
import com.openclaw.gateway.websocket.GatewayMethodRouter;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

/**
 * RPC methods for cron job management + system/heartbeat methods.
 * <p>
 * Corresponds to TypeScript's:
 * - server-methods/cron.ts (cron.list, cron.status, cron.add, cron.update,
 * cron.remove, cron.run, cron.runs)
 * - server-methods/system.ts (system-presence, system-event, last-heartbeat,
 * set-heartbeats, wake)
 * - server-methods/connect.ts (connect)
 * </p>
 */
@Slf4j
@Component
public class SystemMethodRegistrar {

    private final GatewayMethodRouter methodRouter;
    private final CronService cronService;
    private final ObjectMapper objectMapper;
    private final long startTime = System.currentTimeMillis();
    private volatile long lastHeartbeat = System.currentTimeMillis();

    public SystemMethodRegistrar(
            GatewayMethodRouter methodRouter,
            CronService cronService,
            ObjectMapper objectMapper) {
        this.methodRouter = methodRouter;
        this.cronService = cronService;
        this.objectMapper = objectMapper;
    }

    @PostConstruct
    public void registerMethods() {
        // Cron
        methodRouter.registerMethod("cron.list", this::handleCronList);
        methodRouter.registerMethod("cron.status", this::handleCronStatus);
        methodRouter.registerMethod("cron.add", this::handleCronAdd);
        methodRouter.registerMethod("cron.update", this::handleCronUpdate);
        methodRouter.registerMethod("cron.remove", this::handleCronRemove);
        methodRouter.registerMethod("cron.run", this::handleCronRun);
        methodRouter.registerMethod("cron.runs", this::handleCronRuns);

        // System / Heartbeat
        methodRouter.registerMethod("last-heartbeat", this::handleLastHeartbeat);
        methodRouter.registerMethod("set-heartbeats", this::handleSetHeartbeats);
        methodRouter.registerMethod("wake", this::handleWake);
        methodRouter.registerMethod("system-presence", this::handleSystemPresence);
        methodRouter.registerMethod("system-event", this::handleSystemEvent);

        // Connect
        methodRouter.registerMethod("connect", this::handleConnect);

        // Send (generic cross-channel message)
        methodRouter.registerMethod("send", this::handleSend);
    }

    // ========================================
    // Cron Methods
    // ========================================

    private CompletableFuture<Object> handleCronList(JsonNode params, GatewayConnection conn) {
        var jobs = cronService.listJobs();
        List<Map<String, Object>> list = jobs.stream().map(this::cronJobToMap).collect(Collectors.toList());
        return CompletableFuture.completedFuture(Map.of("ok", true, "jobs", list));
    }

    private CompletableFuture<Object> handleCronStatus(JsonNode params, GatewayConnection conn) {
        var jobs = cronService.listJobs();
        long active = jobs.stream().filter(CronJob::isEnabled).count();
        return CompletableFuture.completedFuture(Map.of(
                "ok", true,
                "total", jobs.size(),
                "active", active,
                "paused", jobs.size() - active));
    }

    private CompletableFuture<Object> handleCronAdd(JsonNode params, GatewayConnection conn) {
        try {
            CronJob job = objectMapper.treeToValue(params, CronJob.class);
            if (job.getId() == null || job.getId().isBlank()) {
                job.setId(UUID.randomUUID().toString().substring(0, 8));
            }
            cronService.addJob(job);
            return CompletableFuture.completedFuture(Map.of("ok", true, "job", cronJobToMap(job)));
        } catch (Exception e) {
            return CompletableFuture.failedFuture(e);
        }
    }

    private CompletableFuture<Object> handleCronUpdate(JsonNode params, GatewayConnection conn) {
        String id = params.has("id") ? params.get("id").asText() : null;
        if (id == null) {
            return CompletableFuture.failedFuture(new IllegalArgumentException("id is required"));
        }

        var existing = cronService.getJob(id);
        if (existing.isEmpty()) {
            return CompletableFuture.failedFuture(new IllegalArgumentException("Cron job not found: " + id));
        }

        try {
            CronJob updated = objectMapper.treeToValue(params, CronJob.class);
            updated.setId(id);
            cronService.removeJob(id);
            cronService.addJob(updated);
            return CompletableFuture.completedFuture(Map.of("ok", true, "job", cronJobToMap(updated)));
        } catch (Exception e) {
            return CompletableFuture.failedFuture(e);
        }
    }

    private CompletableFuture<Object> handleCronRemove(JsonNode params, GatewayConnection conn) {
        String id = params.has("id") ? params.get("id").asText() : null;
        if (id == null) {
            return CompletableFuture.failedFuture(new IllegalArgumentException("id is required"));
        }
        boolean removed = cronService.removeJob(id);
        return CompletableFuture.completedFuture(Map.of("ok", true, "removed", removed, "id", id));
    }

    private CompletableFuture<Object> handleCronRun(JsonNode params, GatewayConnection conn) {
        String id = params.has("id") ? params.get("id").asText() : null;
        if (id == null) {
            return CompletableFuture.failedFuture(new IllegalArgumentException("id is required"));
        }

        var runLog = cronService.forceRun(id);
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("id", id);
        result.put("started", true);
        result.put("runLog", runLog);
        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleCronRuns(JsonNode params, GatewayConnection conn) {
        String jobId = params.has("jobId") ? params.get("jobId").asText() : null;
        int limit = params.has("limit") ? params.get("limit").asInt(20) : 20;

        var logs = cronService.getRunLogs(jobId, limit);
        return CompletableFuture.completedFuture(Map.of("ok", true, "runs", logs));
    }

    // ========================================
    // System / Heartbeat
    // ========================================

    private CompletableFuture<Object> handleLastHeartbeat(JsonNode params, GatewayConnection conn) {
        return CompletableFuture.completedFuture(Map.of(
                "ok", true,
                "lastHeartbeat", lastHeartbeat,
                "agoMs", System.currentTimeMillis() - lastHeartbeat));
    }

    private CompletableFuture<Object> handleSetHeartbeats(JsonNode params, GatewayConnection conn) {
        lastHeartbeat = System.currentTimeMillis();
        return CompletableFuture.completedFuture(Map.of("ok", true, "ts", lastHeartbeat));
    }

    private CompletableFuture<Object> handleWake(JsonNode params, GatewayConnection conn) {
        lastHeartbeat = System.currentTimeMillis();
        log.info("Wake signal received from {}", conn.getConnectionId());
        return CompletableFuture.completedFuture(Map.of("ok", true, "woken", true));
    }

    private CompletableFuture<Object> handleSystemPresence(JsonNode params, GatewayConnection conn) {
        String status = params.has("status") ? params.get("status").asText("online") : "online";
        log.debug("Presence update from {}: {}", conn.getConnectionId(), status);
        return CompletableFuture.completedFuture(Map.of("ok", true, "status", status));
    }

    private CompletableFuture<Object> handleSystemEvent(JsonNode params, GatewayConnection conn) {
        String event = params.has("event") ? params.get("event").asText() : "unknown";
        log.debug("System event from {}: {}", conn.getConnectionId(), event);
        return CompletableFuture.completedFuture(Map.of("ok", true, "event", event, "received", true));
    }

    // ========================================
    // Connect
    // ========================================

    private CompletableFuture<Object> handleConnect(JsonNode params, GatewayConnection conn) {
        String clientName = params.has("name") ? params.get("name").asText() : "unknown";
        String clientVersion = params.has("version") ? params.get("version").asText() : "unknown";

        log.info("Client connected: {} v{} (conn={})", clientName, clientVersion, conn.getConnectionId());

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("serverVersion", "0.1.0-SNAPSHOT");
        result.put("sessionId", conn.getConnectionId());
        result.put("serverTime", System.currentTimeMillis());
        result.put("uptimeMs", System.currentTimeMillis() - startTime);
        return CompletableFuture.completedFuture(result);
    }

    // ========================================
    // Send (generic message)
    // ========================================

    private CompletableFuture<Object> handleSend(JsonNode params, GatewayConnection conn) {
        String channel = params.has("channel") ? params.get("channel").asText(null) : null;
        String message = params.has("message") ? params.get("message").asText(null) : null;

        if (message == null || message.isBlank()) {
            return CompletableFuture.failedFuture(new IllegalArgumentException("message is required"));
        }

        log.info("Send requested: channel={}, msg_len={}", channel, message.length());

        // TODO: Route through ChannelRegistry/ChannelDock when channels are implemented
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("sent", true);
        result.put("channel", channel != null ? channel : "default");
        return CompletableFuture.completedFuture(result);
    }

    // ========================================
    // Helpers
    // ========================================

    private Map<String, Object> cronJobToMap(CronJob job) {
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("id", job.getId());
        map.put("name", job.getName());
        map.put("schedule", job.getSchedule());
        map.put("enabled", job.isEnabled());
        map.put("message", job.getMessage());
        map.put("agentId", job.getAgentId());
        if (job.getLastRun() != null)
            map.put("lastRun", job.getLastRun().toString());
        if (job.getNextRun() != null)
            map.put("nextRun", job.getNextRun().toString());
        return map;
    }
}
