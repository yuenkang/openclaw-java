package com.openclaw.agent.runtime;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.time.Instant;
import java.util.HexFormat;
import java.util.Map;

/**
 * Anthropic payload logging for debugging.
 * Corresponds to TypeScript agents/anthropic-payload-log.ts.
 *
 * <p>
 * Logs Anthropic API request payloads and usage to a JSONL file
 * when the OPENCLAW_ANTHROPIC_PAYLOAD_LOG environment variable is enabled.
 * </p>
 */
@Slf4j
public final class AnthropicPayloadLog {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private AnthropicPayloadLog() {
    }

    /**
     * Payload log configuration.
     */
    public record PayloadLogConfig(boolean enabled, String filePath) {
    }

    /**
     * Resolve payload log config from environment.
     */
    public static PayloadLogConfig resolveConfig() {
        String envVal = System.getenv("OPENCLAW_ANTHROPIC_PAYLOAD_LOG");
        boolean enabled = "true".equalsIgnoreCase(envVal) || "1".equals(envVal);
        String fileOverride = System.getenv("OPENCLAW_ANTHROPIC_PAYLOAD_LOG_FILE");
        String filePath = (fileOverride != null && !fileOverride.isBlank())
                ? fileOverride.trim()
                : Path.of(System.getProperty("user.home"), ".openclaw", "logs", "anthropic-payload.jsonl").toString();
        return new PayloadLogConfig(enabled, filePath);
    }

    /**
     * Log a payload event to the JSONL file.
     */
    public static void logEvent(PayloadLogConfig config, Map<String, Object> event) {
        if (!config.enabled)
            return;
        try {
            String line = MAPPER.writeValueAsString(event) + "\n";
            Path path = Path.of(config.filePath);
            Files.createDirectories(path.getParent());
            Files.writeString(path, line, StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE, StandardOpenOption.APPEND);
        } catch (IOException e) {
            log.debug("Failed to write anthropic payload log: {}", e.getMessage());
        }
    }

    /**
     * Compute SHA-256 digest of a JSON payload.
     */
    public static String digest(Object payload) {
        try {
            byte[] bytes = MAPPER.writeValueAsBytes(payload);
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] hash = md.digest(bytes);
            return HexFormat.of().formatHex(hash);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Find the last assistant usage block in a message history.
     */
    public static JsonNode findLastAssistantUsage(JsonNode messages) {
        if (messages == null || !messages.isArray())
            return null;
        for (int i = messages.size() - 1; i >= 0; i--) {
            JsonNode msg = messages.get(i);
            if (msg != null && "assistant".equals(msg.path("role").asText(""))
                    && msg.has("usage") && msg.get("usage").isObject()) {
                return msg.get("usage");
            }
        }
        return null;
    }

    /**
     * Record a request payload event.
     */
    public static void recordRequest(PayloadLogConfig config,
            String runId, String sessionId, String sessionKey,
            String provider, String modelId,
            Object payload) {
        if (!config.enabled)
            return;
        logEvent(config, Map.of(
                "ts", Instant.now().toString(),
                "stage", "request",
                "runId", safeStr(runId),
                "sessionId", safeStr(sessionId),
                "sessionKey", safeStr(sessionKey),
                "provider", safeStr(provider),
                "modelId", safeStr(modelId),
                "payloadDigest", safeStr(digest(payload))));
    }

    /**
     * Record a usage event.
     */
    public static void recordUsage(PayloadLogConfig config,
            String runId, String sessionId, String sessionKey,
            String provider, String modelId,
            JsonNode usage, String error) {
        if (!config.enabled)
            return;
        var event = new java.util.HashMap<String, Object>();
        event.put("ts", Instant.now().toString());
        event.put("stage", "usage");
        event.put("runId", safeStr(runId));
        event.put("sessionId", safeStr(sessionId));
        if (usage != null)
            event.put("usage", usage);
        if (error != null)
            event.put("error", error);
        logEvent(config, event);
    }

    private static String safeStr(String s) {
        return s != null ? s : "";
    }
}
