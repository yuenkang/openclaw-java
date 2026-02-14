package com.openclaw.gateway.net;

import java.util.List;

/**
 * Network utility functions for Gateway authentication.
 * Corresponds to TypeScript's gateway/net.ts.
 */
public final class NetUtils {

    private NetUtils() {
    }

    /**
     * Check if an IP is a loopback address.
     * Matches: 127.x.x.x, ::1, ::ffff:127.x
     */
    public static boolean isLoopbackAddress(String ip) {
        if (ip == null || ip.isBlank()) {
            return false;
        }
        if ("127.0.0.1".equals(ip))
            return true;
        if (ip.startsWith("127."))
            return true;
        if ("::1".equals(ip))
            return true;
        // Java InetAddress.getHostAddress() returns full-form IPv6 for loopback
        if ("0:0:0:0:0:0:0:1".equals(ip))
            return true;
        if (ip.startsWith("::ffff:127."))
            return true;
        return false;
    }

    /**
     * Normalize IPv4-mapped IPv6 addresses to plain IPv4.
     * e.g. "::ffff:127.0.0.1" → "127.0.0.1"
     */
    public static String normalizeIPv4Mapped(String ip) {
        if (ip == null)
            return null;
        if (ip.startsWith("::ffff:")) {
            return ip.substring("::ffff:".length());
        }
        return ip;
    }

    /**
     * Normalize an IP address (trim, lowercase, strip IPv4-mapped prefix).
     */
    public static String normalizeIp(String ip) {
        if (ip == null || ip.isBlank())
            return null;
        return normalizeIPv4Mapped(ip.trim().toLowerCase());
    }

    /**
     * Strip optional port suffix from an IP string.
     * e.g. "127.0.0.1:8080" → "127.0.0.1", "[::1]:8080" → "::1"
     */
    public static String stripOptionalPort(String ip) {
        if (ip == null)
            return null;
        // IPv6 bracket notation [::1]:port
        if (ip.startsWith("[")) {
            int end = ip.indexOf(']');
            if (end != -1)
                return ip.substring(1, end);
        }
        // IPv4 with port: only strip if there's exactly one colon
        int lastColon = ip.lastIndexOf(':');
        if (lastColon > -1 && ip.contains(".") && ip.indexOf(':') == lastColon) {
            return ip.substring(0, lastColon);
        }
        return ip;
    }

    /**
     * Parse the first client IP from an X-Forwarded-For header value.
     * Corresponds to TS parseForwardedForClientIp().
     */
    public static String parseForwardedForClientIp(String forwardedFor) {
        if (forwardedFor == null || forwardedFor.isBlank())
            return null;
        String first = forwardedFor.split(",")[0].trim();
        if (first.isEmpty())
            return null;
        return normalizeIp(stripOptionalPort(first));
    }

    /**
     * Parse X-Real-IP header value.
     */
    public static String parseRealIp(String realIp) {
        if (realIp == null || realIp.isBlank())
            return null;
        return normalizeIp(stripOptionalPort(realIp.trim()));
    }

    /**
     * Check if an IP is a trusted proxy address.
     */
    public static boolean isTrustedProxyAddress(String ip, List<String> trustedProxies) {
        String normalized = normalizeIp(ip);
        if (normalized == null || trustedProxies == null || trustedProxies.isEmpty()) {
            return false;
        }
        return trustedProxies.stream()
                .map(NetUtils::normalizeIp)
                .anyMatch(normalized::equals);
    }

    /**
     * Resolve the real client IP address, considering trusted proxies.
     * Corresponds to TS resolveGatewayClientIp().
     */
    public static String resolveGatewayClientIp(
            String remoteAddr, String forwardedFor, String realIp, List<String> trustedProxies) {
        String remote = normalizeIp(remoteAddr);
        if (remote == null)
            return null;
        if (!isTrustedProxyAddress(remote, trustedProxies)) {
            return remote;
        }
        String fromForwarded = parseForwardedForClientIp(forwardedFor);
        if (fromForwarded != null)
            return fromForwarded;
        String fromRealIp = parseRealIp(realIp);
        if (fromRealIp != null)
            return fromRealIp;
        return remote;
    }

    /**
     * Check if a request is a local direct request (not through proxy).
     * A request is local+direct when:
     * 1. The client IP is loopback
     * 2. The Host header is localhost/127.0.0.1/::1
     * 3. There are no proxy headers, OR they come from a trusted proxy
     *
     * Corresponds to TS isLocalDirectRequest().
     */
    public static boolean isLocalDirectRequest(
            String clientIp, String hostHeader,
            String forwardedFor, String realIp, String forwardedHost,
            String remoteAddr, List<String> trustedProxies) {
        if (!isLoopbackAddress(clientIp)) {
            return false;
        }
        String host = getHostName(hostHeader);
        boolean hostIsLocal = "localhost".equals(host) || "127.0.0.1".equals(host) || "::1".equals(host);
        boolean hasForwarded = isNotBlank(forwardedFor) || isNotBlank(realIp) || isNotBlank(forwardedHost);
        boolean remoteIsTrusted = isTrustedProxyAddress(remoteAddr, trustedProxies);

        return hostIsLocal && (!hasForwarded || remoteIsTrusted);
    }

    /**
     * Extract hostname from a Host header, stripping port and brackets.
     */
    public static String getHostName(String hostHeader) {
        String host = (hostHeader != null ? hostHeader : "").trim().toLowerCase();
        if (host.isEmpty())
            return "";
        if (host.startsWith("[")) {
            int end = host.indexOf(']');
            if (end != -1)
                return host.substring(1, end);
        }
        String[] parts = host.split(":");
        return parts.length > 0 ? parts[0] : "";
    }

    private static boolean isNotBlank(String s) {
        return s != null && !s.isBlank();
    }
}
