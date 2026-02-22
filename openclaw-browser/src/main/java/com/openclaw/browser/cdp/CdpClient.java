package com.openclaw.browser.cdp;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

/**
 * CDP WebSocket JSON-RPC client.
 * Corresponds to TypeScript's {@code withCdpSocket()} + {@code createCdpSender()} from cdp.helpers.ts.
 *
 * <p>Usage:
 * <pre>
 *   byte[] screenshot = CdpClient.withCdpSocket(wsUrl, send -> {
 *       send.apply("Page.enable", null);
 *       JsonNode result = send.apply("Page.captureScreenshot", params);
 *       return Base64.getDecoder().decode(result.get("data").asText());
 *   });
 * </pre>
 */
@Slf4j
public final class CdpClient {

    private static final ObjectMapper mapper = new ObjectMapper();
    private static final long DEFAULT_COMMAND_TIMEOUT_MS = 30_000;
    private static final long HANDSHAKE_TIMEOUT_MS = 5_000;

    private CdpClient() {
    }

    /**
     * Functional interface for CDP send operations.
     * Each call sends a JSON-RPC command and returns the result.
     */
    @FunctionalInterface
    public interface CdpSendFn {
        /**
         * Send a CDP command and wait for the result.
         *
         * @param method CDP method name (e.g. "Page.enable")
         * @param params Optional parameters (can be null)
         * @return Result JSON node
         * @throws CdpException if the command fails or times out
         */
        JsonNode send(String method, JsonNode params) throws CdpException;
    }

    /**
     * Open a one-shot CDP WebSocket connection, execute the callback, then close.
     *
     * @param wsUrl  WebSocket URL (e.g. ws://127.0.0.1:9222/devtools/browser/...)
     * @param action Callback receiving a CdpSendFn; return value is passed through
     * @param <T>    Return type
     * @return The value returned by the action
     */
    public static <T> T withCdpSocket(String wsUrl, CdpAction<T> action) throws CdpException {
        return withCdpSocket(wsUrl, action, null);
    }

    /**
     * Open a one-shot CDP WebSocket connection with custom headers.
     */
    public static <T> T withCdpSocket(String wsUrl, CdpAction<T> action,
                                       Map<String, String> extraHeaders) throws CdpException {
        Map<String, String> headers = CdpHelpers.getAuthHeaders(wsUrl);
        if (extraHeaders != null) {
            headers.putAll(extraHeaders);
        }

        OkHttpClient client = new OkHttpClient.Builder()
                .readTimeout(0, TimeUnit.MILLISECONDS) // WebSocket: no read timeout
                .build();

        Request.Builder reqBuilder = new Request.Builder().url(wsUrl);
        headers.forEach(reqBuilder::addHeader);

        CountDownLatch openLatch = new CountDownLatch(1);
        AtomicInteger nextId = new AtomicInteger(1);
        ConcurrentHashMap<Integer, CompletableFuture<JsonNode>> pending = new ConcurrentHashMap<>();
        CompletableFuture<Void> errorFuture = new CompletableFuture<>();

        WebSocket[] wsHolder = new WebSocket[1];

        WebSocketListener listener = new WebSocketListener() {
            @Override
            public void onOpen(WebSocket ws, Response response) {
                wsHolder[0] = ws;
                openLatch.countDown();
            }

            @Override
            public void onMessage(WebSocket ws, String text) {
                try {
                    JsonNode msg = mapper.readTree(text);
                    if (msg.has("id")) {
                        int id = msg.get("id").asInt();
                        CompletableFuture<JsonNode> future = pending.remove(id);
                        if (future == null) return;

                        if (msg.has("error") && msg.get("error").has("message")) {
                            future.completeExceptionally(
                                    new CdpException(msg.get("error").get("message").asText()));
                        } else {
                            future.complete(msg.has("result") ? msg.get("result") : mapper.createObjectNode());
                        }
                    }
                    // Events (no "id") are ignored in one-shot mode
                } catch (Exception e) {
                    log.debug("CDP parse error: {}", e.getMessage());
                }
            }

            @Override
            public void onFailure(WebSocket ws, Throwable t, Response response) {
                openLatch.countDown();
                errorFuture.completeExceptionally(t);
                // Reject all pending
                for (CompletableFuture<JsonNode> f : pending.values()) {
                    f.completeExceptionally(new CdpException("CDP socket failed: " + t.getMessage()));
                }
                pending.clear();
            }

            @Override
            public void onClosed(WebSocket ws, int code, String reason) {
                for (CompletableFuture<JsonNode> f : pending.values()) {
                    f.completeExceptionally(new CdpException("CDP socket closed"));
                }
                pending.clear();
            }
        };

        WebSocket ws = client.newWebSocket(reqBuilder.build(), listener);

        try {
            // Wait for WebSocket open
            if (!openLatch.await(HANDSHAKE_TIMEOUT_MS, TimeUnit.MILLISECONDS)) {
                ws.cancel();
                throw new CdpException("CDP WebSocket handshake timeout");
            }

            // Check if connection failed during handshake
            if (errorFuture.isCompletedExceptionally()) {
                try {
                    errorFuture.get();
                } catch (ExecutionException e) {
                    throw new CdpException("CDP connection failed: " + e.getCause().getMessage());
                }
            }

            if (wsHolder[0] == null) {
                throw new CdpException("CDP WebSocket not connected");
            }

            // Create the send function
            CdpSendFn sendFn = (method, params) -> {
                int id = nextId.getAndIncrement();
                ObjectNode msg = mapper.createObjectNode();
                msg.put("id", id);
                msg.put("method", method);
                if (params != null) {
                    msg.set("params", params);
                }

                CompletableFuture<JsonNode> future = new CompletableFuture<>();
                pending.put(id, future);

                boolean sent = wsHolder[0].send(msg.toString());
                if (!sent) {
                    pending.remove(id);
                    throw new CdpException("Failed to send CDP command: " + method);
                }

                try {
                    return future.get(DEFAULT_COMMAND_TIMEOUT_MS, TimeUnit.MILLISECONDS);
                } catch (TimeoutException e) {
                    pending.remove(id);
                    throw new CdpException("CDP command timeout: " + method);
                } catch (ExecutionException e) {
                    Throwable cause = e.getCause();
                    if (cause instanceof CdpException ce) throw ce;
                    throw new CdpException("CDP command failed: " + cause.getMessage());
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw new CdpException("CDP command interrupted: " + method);
                }
            };

            return action.execute(sendFn);

        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new CdpException("CDP connection interrupted");
        } finally {
            try {
                ws.close(1000, "done");
            } catch (Exception e) {
                // ignore
            }
            client.dispatcher().executorService().shutdown();
            client.connectionPool().evictAll();
        }
    }

    /**
     * Action to execute within a CDP socket connection.
     */
    @FunctionalInterface
    public interface CdpAction<T> {
        T execute(CdpSendFn send) throws CdpException;
    }

    /**
     * CDP operation exception.
     */
    public static class CdpException extends Exception {
        public CdpException(String message) {
            super(message);
        }

        public CdpException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}
