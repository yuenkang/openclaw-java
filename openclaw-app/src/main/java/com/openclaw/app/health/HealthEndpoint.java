package com.openclaw.app.health;

import com.openclaw.gateway.runtime.RuntimeMetrics;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.lang.management.ManagementFactory;

/**
 * Health and metrics REST endpoint.
 * Provides /health for liveness probes and /metrics for runtime stats.
 */
@RestController
public class HealthEndpoint {

    private final ObjectMapper mapper = new ObjectMapper();

    /**
     * Liveness probe — returns 200 OK if the JVM is running.
     * Suitable for container health checks and load balancer probes.
     */
    @GetMapping("/health")
    public ObjectNode health() {
        var node = mapper.createObjectNode();
        node.put("status", "ok");
        node.put("uptime", ManagementFactory.getRuntimeMXBean().getUptime());

        var memory = node.putObject("memory");
        Runtime rt = Runtime.getRuntime();
        memory.put("total_mb", rt.totalMemory() / (1024 * 1024));
        memory.put("free_mb", rt.freeMemory() / (1024 * 1024));
        memory.put("used_mb", (rt.totalMemory() - rt.freeMemory()) / (1024 * 1024));
        memory.put("max_mb", rt.maxMemory() / (1024 * 1024));

        var threads = node.putObject("threads");
        threads.put("active", Thread.activeCount());

        return node;
    }

    /**
     * Runtime metrics snapshot.
     */
    @GetMapping("/metrics")
    public RuntimeMetrics.Snapshot metrics() {
        return RuntimeMetrics.getInstance().snapshot();
    }
}
