package com.openclaw.common.infra;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.io.*;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.*;

/**
 * Communicates with an external exec host via Unix domain socket or TCP.
 * <p>
 * Sends HMAC-authenticated command execution requests and receives results.
 * Used for delegating command execution to a supervised host process.
 * <p>
 * Port of: infra/exec-host.ts
 */
@Slf4j
public class ExecHost {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ExecRequest {
        private String[] command;
        private String rawCommand;
        private String cwd;
        private Map<String, String> env;
        private Long timeoutMs;
        private Boolean needsScreenRecording;
        private String agentId;
        private String sessionKey;
        private String approvalDecision; // "allow-once" | "allow-always"
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ExecRunResult {
        private Integer exitCode;
        private boolean timedOut;
        private boolean success;
        private String stdout;
        private String stderr;
        private String error;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ExecError {
        private String code;
        private String message;
        private String reason;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ExecResponse {
        private boolean ok;
        private ExecRunResult payload;
        private ExecError error;
    }

    /**
     * Send a command execution request via TCP socket.
     *
     * @param host      TCP hostname (e.g. "127.0.0.1")
     * @param port      TCP port
     * @param token     HMAC authentication token
     * @param request   the exec request
     * @param timeoutMs overall timeout in ms (default 20_000)
     * @return the response, or null on failure/timeout
     */
    public static ExecResponse requestViaSocket(
            String host, int port, String token,
            ExecRequest request, long timeoutMs) {
        if (host == null || host.isEmpty() || token == null || token.isEmpty()) {
            return null;
        }
        if (timeoutMs <= 0) {
            timeoutMs = 20_000;
        }

        ExecutorService executor = Executors.newSingleThreadExecutor();
        long finalTimeoutMs = timeoutMs;
        Future<ExecResponse> future = executor.submit(() -> {
            try (Socket socket = new Socket(host, port)) {
                socket.setSoTimeout((int) finalTimeoutMs);

                String requestJson = MAPPER.writeValueAsString(request);
                String nonce = UUID.randomUUID().toString().replace("-", "").substring(0, 32);
                long ts = System.currentTimeMillis();
                String hmac = computeHmac(token, nonce + ":" + ts + ":" + requestJson);

                Map<String, Object> payload = Map.of(
                        "type", "exec",
                        "id", UUID.randomUUID().toString(),
                        "nonce", nonce,
                        "ts", ts,
                        "hmac", hmac,
                        "requestJson", requestJson);
                String payloadJson = MAPPER.writeValueAsString(payload) + "\n";

                OutputStream out = socket.getOutputStream();
                out.write(payloadJson.getBytes(StandardCharsets.UTF_8));
                out.flush();

                BufferedReader reader = new BufferedReader(
                        new InputStreamReader(socket.getInputStream(), StandardCharsets.UTF_8));
                String line;
                while ((line = reader.readLine()) != null) {
                    String trimmed = line.trim();
                    if (trimmed.isEmpty())
                        continue;
                    try {
                        @SuppressWarnings("unchecked")
                        Map<String, Object> msg = MAPPER.readValue(trimmed, Map.class);
                        if ("exec-res".equals(msg.get("type"))) {
                            Boolean ok = (Boolean) msg.get("ok");
                            if (Boolean.TRUE.equals(ok) && msg.get("payload") != null) {
                                ExecRunResult result = MAPPER.convertValue(
                                        msg.get("payload"), ExecRunResult.class);
                                return ExecResponse.builder().ok(true).payload(result).build();
                            }
                            if (Boolean.FALSE.equals(ok) && msg.get("error") != null) {
                                ExecError error = MAPPER.convertValue(
                                        msg.get("error"), ExecError.class);
                                return ExecResponse.builder().ok(false).error(error).build();
                            }
                            return null;
                        }
                    } catch (Exception e) {
                        // ignore malformed lines
                    }
                }
            }
            return null;
        });

        try {
            return future.get(timeoutMs, TimeUnit.MILLISECONDS);
        } catch (Exception e) {
            log.debug("exec host request failed: {}", e.getMessage());
            return null;
        } finally {
            executor.shutdownNow();
        }
    }

    private static String computeHmac(String key, String data) {
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            mac.init(new SecretKeySpec(key.getBytes(StandardCharsets.UTF_8), "HmacSHA256"));
            byte[] hash = mac.doFinal(data.getBytes(StandardCharsets.UTF_8));
            StringBuilder hex = new StringBuilder();
            for (byte b : hash) {
                hex.append(String.format("%02x", b));
            }
            return hex.toString();
        } catch (Exception e) {
            throw new RuntimeException("HMAC computation failed", e);
        }
    }
}
