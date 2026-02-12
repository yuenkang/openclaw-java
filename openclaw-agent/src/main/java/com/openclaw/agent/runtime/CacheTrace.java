package com.openclaw.agent.runtime;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Diagnostic cache trace for debugging LLM caching behavior.
 * Records stage events as JSONL lines with SHA-256 message/system digests.
 * Corresponds to TypeScript's cache-trace.ts.
 */
@Slf4j
public class CacheTrace {

    private static final ObjectMapper JSON = new ObjectMapper();
    private static final Map<String, CacheTraceWriter> WRITERS = new ConcurrentHashMap<>();

    // =========================================================================
    // Types
    // =========================================================================

    /**
     * Cache trace processing stages.
     */
    public enum Stage {
        SESSION_LOADED("session:loaded"),
        SESSION_SANITIZED("session:sanitized"),
        SESSION_LIMITED("session:limited"),
        PROMPT_BEFORE("prompt:before"),
        PROMPT_IMAGES("prompt:images"),
        STREAM_CONTEXT("stream:context"),
        SESSION_AFTER("session:after");

        private final String key;

        Stage(String key) {
            this.key = key;
        }

        public String key() {
            return key;
        }
    }

    /**
     * Configuration for cache tracing.
     */
    public record CacheTraceConfig(
            boolean enabled,
            String filePath,
            boolean includeMessages,
            boolean includePrompt,
            boolean includeSystem) {
    }

    /**
     * A single trace event.
     */
    public record CacheTraceEvent(
            String ts,
            int seq,
            String stage,
            String runId,
            String sessionId,
            String sessionKey,
            String provider,
            String modelId,
            String workspaceDir,
            String prompt,
            Object system,
            Map<String, Object> options,
            List<Map<String, Object>> messages,
            Integer messageCount,
            List<String> messageRoles,
            List<String> messageFingerprints,
            String messagesDigest,
            String systemDigest,
            String note,
            String error) {
    }

    /**
     * Functional interface for recording a stage event.
     */
    @FunctionalInterface
    public interface StageRecorder {
        void record(Stage stage, Map<String, Object> payload);

        default void record(Stage stage) {
            record(stage, Map.of());
        }
    }

    // =========================================================================
    // Writer
    // =========================================================================

    private record CacheTraceWriter(String filePath, ExecutorService executor) {

        void write(String line) {
            executor.submit(() -> {
                try {
                    Path path = Path.of(filePath);
                    Files.createDirectories(path.getParent());
                    Files.writeString(path, line, StandardCharsets.UTF_8,
                            StandardOpenOption.CREATE, StandardOpenOption.APPEND);
                } catch (IOException e) {
                    // silently ignore write failures
                }
            });
        }
    }

    private static CacheTraceWriter getWriter(String filePath) {
        return WRITERS.computeIfAbsent(filePath, fp -> {
            ExecutorService exec = Executors.newSingleThreadExecutor(r -> {
                Thread t = new Thread(r, "cache-trace-writer");
                t.setDaemon(true);
                return t;
            });
            return new CacheTraceWriter(fp, exec);
        });
    }

    // =========================================================================
    // Factory
    // =========================================================================

    /**
     * Create a cache trace instance. Returns null if tracing is disabled.
     *
     * @param config       trace configuration
     * @param runId        current run ID
     * @param sessionId    session ID
     * @param sessionKey   session key
     * @param provider     LLM provider
     * @param modelId      model ID
     * @param workspaceDir workspace directory
     * @return CacheTrace instance or null
     */
    public static StageRecorder create(
            CacheTraceConfig config,
            String runId, String sessionId, String sessionKey,
            String provider, String modelId, String workspaceDir) {

        if (config == null || !config.enabled())
            return null;

        CacheTraceWriter writer = getWriter(config.filePath());
        AtomicInteger seq = new AtomicInteger(0);

        return (stage, payload) -> {
            try {
                Map<String, Object> event = new LinkedHashMap<>();
                event.put("ts", Instant.now().toString());
                event.put("seq", seq.incrementAndGet());
                event.put("stage", stage.key());

                // Base fields
                if (runId != null)
                    event.put("runId", runId);
                if (sessionId != null)
                    event.put("sessionId", sessionId);
                if (sessionKey != null)
                    event.put("sessionKey", sessionKey);
                if (provider != null)
                    event.put("provider", provider);
                if (modelId != null)
                    event.put("modelId", modelId);
                if (workspaceDir != null)
                    event.put("workspaceDir", workspaceDir);

                // Payload fields
                if (payload != null) {
                    Object promptVal = payload.get("prompt");
                    if (promptVal != null && config.includePrompt()) {
                        event.put("prompt", promptVal.toString());
                    }

                    Object systemVal = payload.get("system");
                    if (systemVal != null && config.includeSystem()) {
                        event.put("system", systemVal);
                        event.put("systemDigest", sha256Digest(systemVal));
                    }

                    Object optionsVal = payload.get("options");
                    if (optionsVal instanceof Map) {
                        event.put("options", optionsVal);
                    }

                    Object modelVal = payload.get("model");
                    if (modelVal instanceof Map) {
                        event.put("model", modelVal);
                    }

                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> messages = payload.get("messages") instanceof List<?> list
                            ? (List<Map<String, Object>>) list
                            : null;
                    if (messages != null) {
                        int count = messages.size();
                        event.put("messageCount", count);

                        List<String> roles = new ArrayList<>();
                        List<String> fingerprints = new ArrayList<>();
                        for (Map<String, Object> msg : messages) {
                            Object role = msg.get("role");
                            roles.add(role != null ? role.toString() : null);
                            fingerprints.add(sha256Digest(msg));
                        }
                        event.put("messageRoles", roles);
                        event.put("messageFingerprints", fingerprints);
                        event.put("messagesDigest", sha256Digest(
                                String.join("|", fingerprints)));

                        if (config.includeMessages()) {
                            event.put("messages", messages);
                        }
                    }

                    Object note = payload.get("note");
                    if (note != null)
                        event.put("note", note.toString());

                    Object error = payload.get("error");
                    if (error != null)
                        event.put("error", error.toString());
                }

                String line = safeJsonLine(event);
                if (line != null) {
                    writer.write(line + "\n");
                }
            } catch (Exception e) {
                log.debug("Cache trace record error: {}", e.getMessage());
            }
        };
    }

    // =========================================================================
    // Config resolution
    // =========================================================================

    /**
     * Resolve cache trace config from environment variables and config.
     *
     * @param envEnabled     OPENCLAW_CACHE_TRACE env var value
     * @param envFile        OPENCLAW_CACHE_TRACE_FILE env var value
     * @param configEnabled  config.diagnostics.cacheTrace.enabled
     * @param configFilePath config.diagnostics.cacheTrace.filePath
     * @param stateDir       state directory for default file path
     * @return resolved config
     */
    public static CacheTraceConfig resolveConfig(
            String envEnabled, String envFile,
            Boolean configEnabled, String configFilePath,
            String stateDir) {

        boolean enabled = parseBool(envEnabled, configEnabled != null ? configEnabled : false);
        String filePath;
        if (configFilePath != null && !configFilePath.isBlank()) {
            filePath = configFilePath.trim();
        } else if (envFile != null && !envFile.isBlank()) {
            filePath = envFile.trim();
        } else {
            filePath = Path.of(stateDir, "logs", "cache-trace.jsonl").toString();
        }

        return new CacheTraceConfig(enabled, filePath, true, true, true);
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    /**
     * Compute SHA-256 hex digest of a value.
     */
    static String sha256Digest(Object value) {
        try {
            String serialized = stableStringify(value);
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] hash = md.digest(serialized.getBytes(StandardCharsets.UTF_8));
            return bytesToHex(hash);
        } catch (NoSuchAlgorithmException e) {
            return "error";
        }
    }

    private static String stableStringify(Object value) {
        if (value == null)
            return "null";
        if (value instanceof String s)
            return JSON_quote(s);
        if (value instanceof Number n) {
            double d = n.doubleValue();
            if (Double.isInfinite(d) || Double.isNaN(d))
                return JSON_quote(String.valueOf(d));
            return String.valueOf(value);
        }
        if (value instanceof Boolean)
            return String.valueOf(value);
        if (value instanceof List<?> list) {
            StringJoiner sj = new StringJoiner(",", "[", "]");
            for (Object item : list)
                sj.add(stableStringify(item));
            return sj.toString();
        }
        if (value instanceof Map<?, ?> map) {
            List<String> keys = new ArrayList<>();
            for (Object k : map.keySet())
                keys.add(String.valueOf(k));
            Collections.sort(keys);
            StringJoiner sj = new StringJoiner(",", "{", "}");
            for (String key : keys) {
                sj.add(JSON_quote(key) + ":" + stableStringify(map.get(key)));
            }
            return sj.toString();
        }
        // Fallback: JSON serialize
        try {
            return JSON.writeValueAsString(value);
        } catch (JsonProcessingException e) {
            return JSON_quote(String.valueOf(value));
        }
    }

    private static String JSON_quote(String s) {
        try {
            return JSON.writeValueAsString(s);
        } catch (JsonProcessingException e) {
            return "\"" + s.replace("\"", "\\\"") + "\"";
        }
    }

    private static String safeJsonLine(Map<String, Object> event) {
        try {
            return JSON.writeValueAsString(event);
        } catch (JsonProcessingException e) {
            return null;
        }
    }

    private static String bytesToHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder(bytes.length * 2);
        for (byte b : bytes) {
            sb.append(String.format("%02x", b & 0xff));
        }
        return sb.toString();
    }

    private static boolean parseBool(String value, boolean fallback) {
        if (value == null || value.isBlank())
            return fallback;
        String v = value.trim().toLowerCase();
        return switch (v) {
            case "true", "yes", "1", "on" -> true;
            case "false", "no", "0", "off" -> false;
            default -> fallback;
        };
    }
}
