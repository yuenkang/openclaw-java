package com.openclaw.common.infra;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.Map;

/**
 * Manages a restart-sentinel JSON file for cross-restart state transfer.
 * The sentinel is written before a restart and consumed after the process
 * comes back up, allowing the gateway to relay restart information
 * (e.g. flash messages to CLI clients).
 * <p>
 * Corresponds to TypeScript's infra/restart-sentinel.ts.
 */
public final class RestartSentinel {

    private RestartSentinel() {
    }

    private static final Logger log = LoggerFactory.getLogger(RestartSentinel.class);
    private static final ObjectMapper mapper = new ObjectMapper();
    private static final String FILENAME = "restart-sentinel.json";
    /**
     * Sentinel version for forward compatibility.
     */
    private static final int CURRENT_VERSION = 1;

    // =========================================================================
    // Data model
    // =========================================================================

    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record Payload(
            String kind,
            String status,
            String message,
            Map<String, Object> stats,
            Long timestampMs) {

        public static Payload of(String kind, String status, String message) {
            return new Payload(kind, status, message, null, Instant.now().toEpochMilli());
        }

        public static Payload of(String kind, String status, String message,
                Map<String, Object> stats) {
            return new Payload(kind, status, message, stats, Instant.now().toEpochMilli());
        }
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record Sentinel(int version, Payload payload) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Write a restart sentinel to the state directory.
     *
     * @param stateDir the openclaw state directory (e.g. ~/.openclaw)
     * @param payload  the restart payload
     * @return the path to the sentinel file
     */
    public static Path writeSentinel(Path stateDir, Payload payload) throws IOException {
        Path filepath = resolveSentinelPath(stateDir);
        Files.createDirectories(filepath.getParent());
        Sentinel sentinel = new Sentinel(CURRENT_VERSION, payload);
        String json = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(sentinel);
        Files.writeString(filepath, json + "\n");
        log.debug("restart-sentinel: wrote {}", filepath);
        return filepath;
    }

    /**
     * Read the restart sentinel without consuming it.
     *
     * @return the sentinel, or null if it doesn't exist or is malformed
     */
    public static Sentinel readSentinel(Path stateDir) {
        Path filepath = resolveSentinelPath(stateDir);
        if (!Files.exists(filepath)) {
            return null;
        }
        try {
            String json = Files.readString(filepath);
            return mapper.readValue(json, Sentinel.class);
        } catch (IOException e) {
            log.warn("restart-sentinel: failed to read {}: {}", filepath, e.getMessage());
            return null;
        }
    }

    /**
     * Read and delete the restart sentinel (consume it).
     *
     * @return the sentinel payload, or null if not present
     */
    public static Payload consumeSentinel(Path stateDir) {
        Sentinel sentinel = readSentinel(stateDir);
        if (sentinel == null) {
            return null;
        }
        Path filepath = resolveSentinelPath(stateDir);
        try {
            Files.deleteIfExists(filepath);
            log.debug("restart-sentinel: consumed {}", filepath);
        } catch (IOException e) {
            log.warn("restart-sentinel: failed to delete {}: {}", filepath, e.getMessage());
        }
        return sentinel.payload();
    }

    /**
     * Resolve the path to the sentinel file.
     */
    public static Path resolveSentinelPath(Path stateDir) {
        return stateDir.resolve(FILENAME);
    }
}
