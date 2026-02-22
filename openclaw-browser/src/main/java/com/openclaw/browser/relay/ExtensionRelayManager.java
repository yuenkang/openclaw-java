package com.openclaw.browser.relay;

import com.openclaw.browser.cdp.CdpHelpers;
import lombok.extern.slf4j.Slf4j;

import java.net.URI;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Manages Extension Relay Server instances.
 * Singleton per port, mirrors TypeScript's serversByPort/relayAuthByPort pattern.
 */
@Slf4j
public final class ExtensionRelayManager {

    private static final ConcurrentHashMap<Integer, ExtensionRelayServer> serversByPort
            = new ConcurrentHashMap<>();

    private ExtensionRelayManager() {
    }

    /**
     * Ensure a relay server is running for the given CDP URL.
     * Returns existing instance if one is already running on the same port.
     */
    public static ExtensionRelayServer ensureRelayServer(String cdpUrl)
            throws Exception {
        URI uri = URI.create(cdpUrl.trim().replaceAll("/$", ""));
        String host = uri.getHost();
        if (host == null) host = "127.0.0.1";

        if (!CdpHelpers.isLoopbackHost(host)) {
            throw new IllegalArgumentException(
                    "Extension relay requires loopback cdpUrl host (got " + host + ")");
        }

        int port = uri.getPort();
        if (port <= 0) {
            String scheme = uri.getScheme();
            port = "https".equals(scheme) ? 443 : 80;
        }

        ExtensionRelayServer existing = serversByPort.get(port);
        if (existing != null) {
            return existing;
        }

        ExtensionRelayServer server = new ExtensionRelayServer(host, port);
        ExtensionRelayServer prev = serversByPort.putIfAbsent(port, server);
        if (prev != null) {
            // Another thread beat us; use theirs
            return prev;
        }

        try {
            server.start();
            log.info("Extension Relay started for {}", cdpUrl);
            return server;
        } catch (Exception e) {
            serversByPort.remove(port);
            throw e;
        }
    }

    /**
     * Stop the relay server for the given CDP URL.
     */
    public static boolean stopRelayServer(String cdpUrl) {
        try {
            URI uri = URI.create(cdpUrl.trim().replaceAll("/$", ""));
            int port = uri.getPort();
            if (port <= 0) {
                String scheme = uri.getScheme();
                port = "https".equals(scheme) ? 443 : 80;
            }

            ExtensionRelayServer server = serversByPort.remove(port);
            if (server == null) return false;

            server.stop();
            return true;
        } catch (Exception e) {
            log.warn("Failed to stop relay server: {}", e.getMessage());
            return false;
        }
    }

    /**
     * Get relay auth headers for a URL if a relay server is running on that port.
     * Used by CdpHelpers.getAuthHeaders() in Batch 1.
     */
    public static Map<String, String> getRelayAuthHeaders(String url) {
        try {
            URI uri = URI.create(url);
            String host = uri.getHost();
            if (host == null || !CdpHelpers.isLoopbackHost(host)) {
                return Map.of();
            }
            int port = uri.getPort();
            if (port <= 0) {
                String scheme = uri.getScheme();
                if ("wss".equals(scheme) || "https".equals(scheme)) port = 443;
                else port = 80;
            }

            ExtensionRelayServer server = serversByPort.get(port);
            if (server == null) return Map.of();

            return Map.of(ExtensionRelayTypes.RELAY_AUTH_HEADER, server.getRelayAuthToken());
        } catch (Exception e) {
            return Map.of();
        }
    }

    /**
     * Stop all running relay servers.
     */
    public static void stopAll() {
        for (Map.Entry<Integer, ExtensionRelayServer> entry : serversByPort.entrySet()) {
            try {
                entry.getValue().stop();
            } catch (Exception e) {
                log.warn("Failed to stop relay on port {}: {}", entry.getKey(), e.getMessage());
            }
        }
        serversByPort.clear();
    }
}
