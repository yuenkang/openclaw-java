package com.openclaw.browser.cdp;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

import java.io.IOException;
import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * CDP helper utilities.
 * Corresponds to TypeScript's cdp.helpers.ts.
 */
@Slf4j
public final class CdpHelpers {

    private static final ObjectMapper mapper = new ObjectMapper();
    private static final OkHttpClient httpClient = new OkHttpClient.Builder()
            .connectTimeout(3, TimeUnit.SECONDS)
            .readTimeout(5, TimeUnit.SECONDS)
            .build();

    private CdpHelpers() {
    }

    /**
     * Append a path to a CDP URL, preserving existing base path.
     * e.g. appendCdpPath("http://127.0.0.1:9222/devtools", "/json/version")
     *      → "http://127.0.0.1:9222/devtools/json/version"
     */
    public static String appendCdpPath(String cdpUrl, String path) {
        try {
            URI uri = URI.create(cdpUrl.trim());
            String basePath = uri.getPath();
            if (basePath == null) basePath = "";
            basePath = basePath.replaceAll("/$", "");
            String suffix = path.startsWith("/") ? path : "/" + path;
            return new URI(uri.getScheme(), uri.getUserInfo(), uri.getHost(),
                    uri.getPort(), basePath + suffix, uri.getQuery(), null).toString();
        } catch (Exception e) {
            // Fallback: simple concat
            return cdpUrl.replaceAll("/$", "") + (path.startsWith("/") ? path : "/" + path);
        }
    }

    /**
     * Fetch JSON from a CDP HTTP endpoint.
     */
    public static <T> T fetchJson(String url, int timeoutMs, Class<T> type) throws IOException {
        Request request = new Request.Builder()
                .url(url)
                .headers(okhttp3.Headers.of(getAuthHeaders(url)))
                .get()
                .build();
        OkHttpClient client = timeoutMs > 0
                ? httpClient.newBuilder()
                .readTimeout(timeoutMs, TimeUnit.MILLISECONDS)
                .connectTimeout(Math.min(timeoutMs, 3000), TimeUnit.MILLISECONDS)
                .build()
                : httpClient;
        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new IOException("CDP HTTP " + response.code());
            }
            String body = response.body() != null ? response.body().string() : "";
            return mapper.readValue(body, type);
        }
    }

    /**
     * Fetch JSON as JsonNode.
     */
    public static JsonNode fetchJsonNode(String url, int timeoutMs) throws IOException {
        return fetchJson(url, timeoutMs, JsonNode.class);
    }

    /**
     * Fetch JSON and deserialize to a type reference.
     */
    public static <T> T fetchJson(String url, int timeoutMs, TypeReference<T> type) throws IOException {
        Request request = new Request.Builder()
                .url(url)
                .headers(okhttp3.Headers.of(getAuthHeaders(url)))
                .get()
                .build();
        OkHttpClient client = timeoutMs > 0
                ? httpClient.newBuilder()
                .readTimeout(timeoutMs, TimeUnit.MILLISECONDS)
                .connectTimeout(Math.min(timeoutMs, 3000), TimeUnit.MILLISECONDS)
                .build()
                : httpClient;
        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new IOException("CDP HTTP " + response.code());
            }
            String body = response.body() != null ? response.body().string() : "";
            return mapper.readValue(body, type);
        }
    }

    /**
     * Check if a host is a loopback address.
     */
    public static boolean isLoopbackHost(String host) {
        if (host == null) return false;
        String h = host.trim().toLowerCase();
        return h.equals("localhost")
                || h.equals("127.0.0.1")
                || h.equals("0.0.0.0")
                || h.equals("[::1]")
                || h.equals("::1")
                || h.equals("[::]")
                || h.equals("::");
    }

    /**
     * Normalize a WebSocket URL against a CDP base URL.
     * Handles protocol/host upgrades for remote connections.
     */
    public static String normalizeCdpWsUrl(String wsUrl, String cdpUrl) {
        try {
            URI ws = URI.create(wsUrl);
            URI cdp = URI.create(cdpUrl);

            String wsHost = ws.getHost() != null ? ws.getHost() : "localhost";
            String cdpHost = cdp.getHost() != null ? cdp.getHost() : "localhost";

            String scheme = ws.getScheme();
            String host = wsHost;
            int port = ws.getPort();

            // If WS points to loopback but CDP is remote, use CDP's host
            if (isLoopbackHost(wsHost) && !isLoopbackHost(cdpHost)) {
                host = cdpHost;
                String cdpScheme = cdp.getScheme() != null ? cdp.getScheme() : "http";
                int cdpPort = cdp.getPort();
                if (cdpPort <= 0) {
                    cdpPort = cdpScheme.equals("https") ? 443 : 80;
                }
                port = cdpPort;
                scheme = cdpScheme.equals("https") ? "wss" : "ws";
            }

            // Upgrade to wss if CDP uses https
            String cdpScheme = cdp.getScheme() != null ? cdp.getScheme() : "http";
            if (cdpScheme.equals("https") && "ws".equals(scheme)) {
                scheme = "wss";
            }

            // Carry over auth from CDP URL if WS lacks it
            String userInfo = ws.getUserInfo();
            if ((userInfo == null || userInfo.isEmpty()) && cdp.getUserInfo() != null) {
                userInfo = cdp.getUserInfo();
            }

            return new URI(scheme, userInfo, host, port, ws.getPath(),
                    ws.getQuery(), null).toString();
        } catch (Exception e) {
            log.warn("Failed to normalize CDP WS URL: {}", e.getMessage());
            return wsUrl;
        }
    }

    /**
     * Get auth headers for relay authentication.
     * Includes Extension Relay token if a relay server is running on this port,
     * and Basic auth from URL credentials.
     */
    public static Map<String, String> getAuthHeaders(String url) {
        Map<String, String> headers = new LinkedHashMap<>();
        // Relay auth headers
        try {
            Map<String, String> relayHeaders =
                    com.openclaw.browser.relay.ExtensionRelayManager.getRelayAuthHeaders(url);
            headers.putAll(relayHeaders);
        } catch (Exception e) {
            // ignore — relay module may not be initialized
        }
        // Basic auth from URL credentials
        try {
            URI uri = URI.create(url);
            String userInfo = uri.getUserInfo();
            if (userInfo != null && !userInfo.isEmpty()
                    && !headers.containsKey("Authorization")) {
                String encoded = java.util.Base64.getEncoder()
                        .encodeToString(userInfo.getBytes(java.nio.charset.StandardCharsets.UTF_8));
                headers.put("Authorization", "Basic " + encoded);
            }
        } catch (Exception e) {
            // ignore
        }
        return headers;
    }
}
