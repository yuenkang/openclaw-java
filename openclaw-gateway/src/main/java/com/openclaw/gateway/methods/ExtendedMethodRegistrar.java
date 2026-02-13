package com.openclaw.gateway.methods;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.gateway.session.SessionStore;
import com.openclaw.gateway.websocket.GatewayConnection;
import com.openclaw.gateway.websocket.GatewayMethodRouter;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Handles extended RPC methods: config write, usage tracking, logs.
 * <p>
 * Corresponds to TypeScript's:
 * - server-methods/config.ts (config.set, config.patch, config.apply,
 * config.schema)
 * - server-methods/usage.ts (usage.status, usage.cost, sessions.usage)
 * - server-methods/logs.ts (logs.tail)
 * </p>
 */
@Slf4j
@Component
public class ExtendedMethodRegistrar {

    private final GatewayMethodRouter methodRouter;
    private final ConfigService configService;
    private final SessionStore sessionStore;
    private final ObjectMapper objectMapper;
    private final long startTime = System.currentTimeMillis();

    /**
     * Per-session usage accumulator: sessionId -> {inputTokens, outputTokens, cost}
     */
    private final Map<String, UsageRecord> usageBySession = new ConcurrentHashMap<>();

    /**
     * Per-session usage log: sessionId -> list of individual request entries.
     */
    private final Map<String, List<UsageLogEntry>> usageLogs = new ConcurrentHashMap<>();

    public ExtendedMethodRegistrar(
            GatewayMethodRouter methodRouter,
            ConfigService configService,
            SessionStore sessionStore,
            ObjectMapper objectMapper) {
        this.methodRouter = methodRouter;
        this.configService = configService;
        this.sessionStore = sessionStore;
        this.objectMapper = objectMapper;
    }

    @PostConstruct
    public void registerMethods() {
        // Config write methods
        methodRouter.registerMethod("config.set", this::handleConfigSet);
        methodRouter.registerMethod("config.patch", this::handleConfigPatch);
        methodRouter.registerMethod("config.apply", this::handleConfigApply);
        methodRouter.registerMethod("config.schema", this::handleConfigSchema);

        // Usage methods
        methodRouter.registerMethod("usage.status", this::handleUsageStatus);
        methodRouter.registerMethod("usage.cost", this::handleUsageCost);
        methodRouter.registerMethod("sessions.usage", this::handleSessionsUsage);
        methodRouter.registerMethod("sessions.usage.timeseries", this::handleSessionsUsageTimeseries);
        methodRouter.registerMethod("sessions.usage.logs", this::handleSessionsUsageLogs);

        // Logs
        methodRouter.registerMethod("logs.tail", this::handleLogsTail);
    }

    // ========================================
    // Config Write Methods
    // ========================================

    /**
     * config.set — Replace the entire config with the provided value.
     * Corresponds to TypeScript's config.set handler.
     */
    private CompletableFuture<Object> handleConfigSet(JsonNode params, GatewayConnection conn) {
        try {
            JsonNode configNode = params.get("config");
            if (configNode == null || configNode.isNull()) {
                return CompletableFuture.failedFuture(
                        new IllegalArgumentException("config is required"));
            }

            OpenClawConfig newConfig = objectMapper.treeToValue(configNode, OpenClawConfig.class);
            configService.saveConfig(newConfig);

            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("path", configService.getConfigPath().toString());
            return CompletableFuture.completedFuture(result);
        } catch (Exception e) {
            log.error("Failed to set config: {}", e.getMessage(), e);
            return CompletableFuture.failedFuture(e);
        }
    }

    /**
     * config.patch — Merge-patch fields into the current config.
     * Corresponds to TypeScript's config.patch handler.
     */
    private CompletableFuture<Object> handleConfigPatch(JsonNode params, GatewayConnection conn) {
        try {
            JsonNode patchNode = params.get("patch");
            if (patchNode == null || patchNode.isNull()) {
                return CompletableFuture.failedFuture(
                        new IllegalArgumentException("patch is required"));
            }

            // Load current config as a tree, merge patch into it
            OpenClawConfig current = configService.loadConfig();
            JsonNode currentTree = objectMapper.valueToTree(current);
            JsonNode merged = mergePatch((ObjectNode) currentTree, patchNode);

            OpenClawConfig newConfig = objectMapper.treeToValue(merged, OpenClawConfig.class);
            configService.saveConfig(newConfig);

            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("path", configService.getConfigPath().toString());
            return CompletableFuture.completedFuture(result);
        } catch (Exception e) {
            log.error("Failed to patch config: {}", e.getMessage(), e);
            return CompletableFuture.failedFuture(e);
        }
    }

    /**
     * config.apply — Apply config changes and trigger reload.
     * Corresponds to TypeScript's config.apply handler.
     */
    private CompletableFuture<Object> handleConfigApply(JsonNode params, GatewayConnection conn) {
        try {
            configService.reloadConfig();

            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("reloaded", true);
            return CompletableFuture.completedFuture(result);
        } catch (Exception e) {
            log.error("Failed to apply config: {}", e.getMessage(), e);
            return CompletableFuture.failedFuture(e);
        }
    }

    /**
     * config.schema — Return the config JSON schema.
     * Simplified version of TypeScript's config.schema handler.
     */
    private CompletableFuture<Object> handleConfigSchema(JsonNode params, GatewayConnection conn) {
        // Return a basic schema describing the config structure
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("description", "OpenClaw configuration schema");

        Map<String, Object> properties = new LinkedHashMap<>();
        properties.put("gateway", Map.of("type", "object", "description", "Gateway settings (port, host)"));
        properties.put("auth", Map.of("type", "object", "description", "Authentication settings"));
        properties.put("agents", Map.of("type", "object", "description", "Agent definitions"));
        properties.put("models", Map.of("type", "object", "description", "Model provider configuration"));
        properties.put("cron", Map.of("type", "object", "description", "Cron job schedules"));
        properties.put("logging", Map.of("type", "object", "description", "Logging configuration"));
        schema.put("properties", properties);

        return CompletableFuture.completedFuture(Map.of("schema", schema));
    }

    // ========================================
    // Usage Methods
    // ========================================

    /**
     * Record token usage for a session (called internally by chat handlers).
     */
    public void recordUsage(String sessionId, int inputTokens, int outputTokens, double cost) {
        usageBySession.compute(sessionId, (k, existing) -> {
            if (existing == null) {
                return new UsageRecord(inputTokens, outputTokens, cost);
            }
            return new UsageRecord(
                    existing.inputTokens + inputTokens,
                    existing.outputTokens + outputTokens,
                    existing.cost + cost);
        });

        // Record individual log entry
        usageLogs.computeIfAbsent(sessionId, k -> Collections.synchronizedList(new ArrayList<>()))
                .add(new UsageLogEntry(
                        System.currentTimeMillis(),
                        inputTokens,
                        outputTokens,
                        cost,
                        null // modelId populated by caller if available
                ));
    }

    /**
     * Record token usage for a session with model information.
     */
    public void recordUsage(String sessionId, String modelId,
            int inputTokens, int outputTokens, double cost) {
        recordUsage(sessionId, inputTokens, outputTokens, cost);
        // Override last log entry with model id
        var logs = usageLogs.get(sessionId);
        if (logs != null && !logs.isEmpty()) {
            var last = logs.get(logs.size() - 1);
            logs.set(logs.size() - 1, new UsageLogEntry(
                    last.timestampMs, last.inputTokens, last.outputTokens, last.cost, modelId));
        }
    }

    /**
     * usage.status — Return overall usage status.
     * Corresponds to TypeScript's usage.status handler.
     */
    private CompletableFuture<Object> handleUsageStatus(JsonNode params, GatewayConnection conn) {
        long totalInput = 0, totalOutput = 0;
        double totalCost = 0;
        for (UsageRecord r : usageBySession.values()) {
            totalInput += r.inputTokens;
            totalOutput += r.outputTokens;
            totalCost += r.cost;
        }

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("sessions", usageBySession.size());
        result.put("totalInputTokens", totalInput);
        result.put("totalOutputTokens", totalOutput);
        result.put("totalTokens", totalInput + totalOutput);
        result.put("totalCostUsd", Math.round(totalCost * 10000.0) / 10000.0);
        result.put("uptimeMs", System.currentTimeMillis() - startTime);
        return CompletableFuture.completedFuture(result);
    }

    /**
     * usage.cost — Return cost breakdown.
     * Corresponds to TypeScript's usage.cost handler.
     */
    private CompletableFuture<Object> handleUsageCost(JsonNode params, GatewayConnection conn) {
        String period = "session"; // default to current session window
        if (params.has("period")) {
            period = params.get("period").asText("session");
        }

        double totalCost = 0;
        for (UsageRecord r : usageBySession.values()) {
            totalCost += r.cost;
        }

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("period", period);
        result.put("costUsd", Math.round(totalCost * 10000.0) / 10000.0);
        result.put("sessions", usageBySession.size());
        return CompletableFuture.completedFuture(result);
    }

    /**
     * sessions.usage — Return usage for a specific session.
     * Corresponds to TypeScript's sessions.usage handler.
     */
    private CompletableFuture<Object> handleSessionsUsage(JsonNode params, GatewayConnection conn) {
        String sessionId = null;
        if (params.has("sessionId") && !params.get("sessionId").isNull()) {
            sessionId = params.get("sessionId").asText();
        } else if (params.has("key") && !params.get("key").isNull()) {
            String key = params.get("key").asText();
            var session = sessionStore.findBySessionKey(key);
            if (session.isPresent()) {
                sessionId = session.get().getSessionId();
            }
        }

        if (sessionId == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("sessionId or key is required"));
        }

        UsageRecord record = usageBySession.get(sessionId);
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("sessionId", sessionId);
        if (record != null) {
            result.put("inputTokens", record.inputTokens);
            result.put("outputTokens", record.outputTokens);
            result.put("totalTokens", record.inputTokens + record.outputTokens);
            result.put("costUsd", Math.round(record.cost * 10000.0) / 10000.0);
        } else {
            result.put("inputTokens", 0);
            result.put("outputTokens", 0);
            result.put("totalTokens", 0);
            result.put("costUsd", 0);
        }
        return CompletableFuture.completedFuture(result);
    }

    /**
     * sessions.usage.timeseries — Return daily model usage for a session.
     * Corresponds to TypeScript's sessions.usage.timeseries handler.
     */
    private CompletableFuture<Object> handleSessionsUsageTimeseries(JsonNode params, GatewayConnection conn) {
        String sessionId = resolveSessionId(params);
        if (sessionId == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("sessionId or key is required"));
        }

        var logs = usageLogs.getOrDefault(sessionId, Collections.emptyList());

        // Bucket by date string (yyyy-MM-dd)
        Map<String, Map<String, Object>> dailyBuckets = new LinkedHashMap<>();
        java.time.format.DateTimeFormatter fmt = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd");
        for (UsageLogEntry entry : logs) {
            String day = java.time.Instant.ofEpochMilli(entry.timestampMs)
                    .atZone(java.time.ZoneId.systemDefault())
                    .format(fmt);

            dailyBuckets.compute(day, (k, existing) -> {
                if (existing == null) {
                    Map<String, Object> bucket = new LinkedHashMap<>();
                    bucket.put("date", day);
                    bucket.put("inputTokens", (long) entry.inputTokens);
                    bucket.put("outputTokens", (long) entry.outputTokens);
                    bucket.put("costUsd", entry.cost);
                    bucket.put("requests", 1);
                    return bucket;
                }
                existing.put("inputTokens", (long) existing.get("inputTokens") + entry.inputTokens);
                existing.put("outputTokens", (long) existing.get("outputTokens") + entry.outputTokens);
                existing.put("costUsd", (double) existing.get("costUsd") + entry.cost);
                existing.put("requests", (int) existing.get("requests") + 1);
                return existing;
            });
        }

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("sessionId", sessionId);
        result.put("timeseries", new ArrayList<>(dailyBuckets.values()));
        return CompletableFuture.completedFuture(result);
    }

    /**
     * sessions.usage.logs — Return per-request cost log entries for a session.
     * Corresponds to TypeScript's sessions.usage.logs handler.
     */
    private CompletableFuture<Object> handleSessionsUsageLogs(JsonNode params, GatewayConnection conn) {
        String sessionId = resolveSessionId(params);
        if (sessionId == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("sessionId or key is required"));
        }

        int limit = params.has("limit") ? Math.max(1, params.get("limit").asInt(100)) : 100;
        int offset = params.has("offset") ? Math.max(0, params.get("offset").asInt(0)) : 0;

        var logs = usageLogs.getOrDefault(sessionId, Collections.emptyList());
        int total = logs.size();

        // Paginate (most recent first)
        List<Map<String, Object>> entries = new ArrayList<>();
        int start = Math.max(0, total - offset - limit);
        int end = Math.max(0, total - offset);
        for (int i = end - 1; i >= start; i--) {
            var entry = logs.get(i);
            Map<String, Object> row = new LinkedHashMap<>();
            row.put("timestampMs", entry.timestampMs);
            row.put("inputTokens", entry.inputTokens);
            row.put("outputTokens", entry.outputTokens);
            row.put("costUsd", Math.round(entry.cost * 10000.0) / 10000.0);
            if (entry.modelId != null) {
                row.put("modelId", entry.modelId);
            }
            entries.add(row);
        }

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("sessionId", sessionId);
        result.put("total", total);
        result.put("offset", offset);
        result.put("limit", limit);
        result.put("entries", entries);
        return CompletableFuture.completedFuture(result);
    }

    // ========================================
    // Logs
    // ========================================

    /**
     * logs.tail — Tail the gateway log file.
     * Corresponds to TypeScript's server-methods/logs.ts.
     */
    private CompletableFuture<Object> handleLogsTail(JsonNode params, GatewayConnection conn) {
        int lines = params.has("lines") ? Math.max(1, params.get("lines").asInt(50)) : 50;
        String logFile = params.has("file") ? params.get("file").asText(null) : null;

        Path logPath;
        if (logFile != null) {
            logPath = Path.of(logFile);
        } else {
            // Default to Spring Boot's log output location
            String home = System.getProperty("user.home");
            logPath = Path.of(home, ".openclaw", "logs", "gateway.log");
        }

        if (!Files.exists(logPath)) {
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("lines", Collections.emptyList());
            result.put("file", logPath.toString());
            result.put("exists", false);
            return CompletableFuture.completedFuture(result);
        }

        try {
            List<String> allLines = Files.readAllLines(logPath);
            int start = Math.max(0, allLines.size() - lines);
            List<String> tailLines = allLines.subList(start, allLines.size());

            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("lines", tailLines);
            result.put("file", logPath.toString());
            result.put("total", allLines.size());
            result.put("returned", tailLines.size());
            return CompletableFuture.completedFuture(result);
        } catch (IOException e) {
            log.warn("Failed to read log file {}: {}", logPath, e.getMessage());
            return CompletableFuture.failedFuture(e);
        }
    }

    // ========================================
    // Helpers
    // ========================================

    /**
     * Resolve session ID from either "sessionId" or "key" params.
     */
    private String resolveSessionId(JsonNode params) {
        if (params.has("sessionId") && !params.get("sessionId").isNull()) {
            return params.get("sessionId").asText();
        }
        if (params.has("key") && !params.get("key").isNull()) {
            String key = params.get("key").asText();
            var session = sessionStore.findBySessionKey(key);
            return session.map(s -> s.getSessionId()).orElse(null);
        }
        return null;
    }

    /**
     * JSON Merge Patch (RFC 7396) implementation.
     */
    private JsonNode mergePatch(ObjectNode target, JsonNode patch) {
        if (!patch.isObject()) {
            return patch;
        }
        var it = patch.fields();
        while (it.hasNext()) {
            var entry = it.next();
            String key = entry.getKey();
            JsonNode value = entry.getValue();
            if (value.isNull()) {
                target.remove(key);
            } else if (value.isObject() && target.has(key) && target.get(key).isObject()) {
                mergePatch((ObjectNode) target.get(key), value);
            } else {
                target.set(key, value);
            }
        }
        return target;
    }

    /**
     * Simple in-memory usage record.
     */
    private record UsageRecord(long inputTokens, long outputTokens, double cost) {
    }

    /**
     * Individual usage log entry for timeseries and logs.
     */
    private record UsageLogEntry(long timestampMs, int inputTokens, int outputTokens,
            double cost, String modelId) {
    }
}
