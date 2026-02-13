package com.openclaw.gateway.net;

import java.net.URI;

/**
 * Browser origin validation for WebSocket and HTTP requests.
 * Corresponds to TypeScript's origin-check.ts.
 */
public final class OriginChecker {

    private OriginChecker() {
    }

    public sealed interface OriginCheckResult permits OriginOk, OriginRejected {
    }

    public record OriginOk() implements OriginCheckResult {
    }

    public record OriginRejected(String reason) implements OriginCheckResult {
    }

    /**
     * Check whether a browser Origin header is allowed.
     *
     * @param requestHost    the Host header from the request
     * @param origin         the Origin header from the request
     * @param allowedOrigins configured allowed origins whitelist
     */
    public static OriginCheckResult checkBrowserOrigin(
            String requestHost, String origin, java.util.List<String> allowedOrigins) {

        ParsedOrigin parsed = parseOrigin(origin);
        if (parsed == null) {
            return new OriginRejected("origin missing or invalid");
        }

        // Check allowlist
        if (allowedOrigins != null) {
            for (String allowed : allowedOrigins) {
                String normalized = allowed.trim().toLowerCase();
                if (!normalized.isEmpty() && normalized.equals(parsed.origin)) {
                    return new OriginOk();
                }
            }
        }

        // Same host check
        String normalizedRequestHost = normalizeHostHeader(requestHost);
        if (!normalizedRequestHost.isEmpty() && parsed.host.equals(normalizedRequestHost)) {
            return new OriginOk();
        }

        // Loopback check
        String requestHostname = resolveHostName(requestHost);
        if (isLoopbackHost(parsed.hostname) && isLoopbackHost(requestHostname)) {
            return new OriginOk();
        }

        return new OriginRejected("origin not allowed");
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    static String normalizeHostHeader(String hostHeader) {
        return hostHeader == null ? "" : hostHeader.trim().toLowerCase();
    }

    static String resolveHostName(String hostHeader) {
        String host = normalizeHostHeader(hostHeader);
        if (host.isEmpty())
            return "";
        if (host.startsWith("[")) {
            int end = host.indexOf(']');
            if (end != -1)
                return host.substring(1, end);
        }
        int colon = host.indexOf(':');
        return colon >= 0 ? host.substring(0, colon) : host;
    }

    record ParsedOrigin(String origin, String host, String hostname) {
    }

    static ParsedOrigin parseOrigin(String originRaw) {
        String trimmed = originRaw == null ? "" : originRaw.trim();
        if (trimmed.isEmpty() || "null".equals(trimmed))
            return null;
        try {
            URI uri = URI.create(trimmed);
            String host = uri.getHost();
            if (host == null)
                return null;
            int port = uri.getPort();
            String scheme = uri.getScheme();
            String originStr = (scheme + "://" + host + (port > 0 ? ":" + port : "")).toLowerCase();
            String hostStr = (host + (port > 0 ? ":" + port : "")).toLowerCase();
            return new ParsedOrigin(originStr, hostStr, host.toLowerCase());
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Check if a hostname is a loopback address.
     */
    public static boolean isLoopbackHost(String hostname) {
        if (hostname == null || hostname.isEmpty())
            return false;
        if ("localhost".equals(hostname))
            return true;
        if ("::1".equals(hostname))
            return true;
        if ("127.0.0.1".equals(hostname) || hostname.startsWith("127."))
            return true;
        return false;
    }
}
