package com.openclaw.common.infra;

import lombok.extern.slf4j.Slf4j;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.List;
import java.util.Set;

/**
 * SSRF (Server-Side Request Forgery) protection.
 * Validates hostnames and IP addresses to block requests to private/internal
 * networks.
 * <p>
 * Corresponds to TypeScript's {@code infra/net/ssrf.ts}.
 */
@Slf4j
public class SsrfGuard {

    private static final Set<String> BLOCKED_HOSTNAMES = Set.of(
            "localhost", "metadata.google.internal");

    private static final List<String> PRIVATE_IPV6_PREFIXES = List.of(
            "fe80:", "fec0:", "fc", "fd");

    /**
     * Policy for SSRF checks.
     */
    public record SsrfPolicy(
            boolean allowPrivateNetwork,
            List<String> allowedHostnames) {
        public static final SsrfPolicy DEFAULT = new SsrfPolicy(false, List.of());
    }

    /**
     * Thrown when a request is blocked by SSRF policy.
     */
    public static class SsrfBlockedError extends RuntimeException {
        public SsrfBlockedError(String message) {
            super(message);
        }
    }

    /**
     * Assert that a hostname resolves to a public IP address.
     * Throws {@link SsrfBlockedError} if blocked.
     */
    public static void assertPublicHostname(String hostname) {
        assertPublicHostname(hostname, SsrfPolicy.DEFAULT);
    }

    /**
     * Assert with a custom policy.
     */
    public static void assertPublicHostname(String hostname, SsrfPolicy policy) {
        String normalized = normalizeHostname(hostname);

        // Check allowed hostnames
        if (policy.allowedHostnames() != null && !policy.allowedHostnames().isEmpty()) {
            for (String allowed : policy.allowedHostnames()) {
                if (normalizeHostname(allowed).equals(normalized)) {
                    return; // Explicitly allowed
                }
            }
        }

        // Block known internal hostnames
        if (isBlockedHostname(normalized)) {
            if (!policy.allowPrivateNetwork()) {
                throw new SsrfBlockedError("Blocked hostname: " + hostname);
            }
        }

        // Resolve and check IP addresses
        try {
            InetAddress[] addresses = InetAddress.getAllByName(hostname);
            for (InetAddress addr : addresses) {
                if (!policy.allowPrivateNetwork() && isPrivateIpAddress(addr)) {
                    throw new SsrfBlockedError(
                            "Blocked private IP: " + addr.getHostAddress() + " for hostname: " + hostname);
                }
            }
        } catch (UnknownHostException e) {
            throw new SsrfBlockedError("Cannot resolve hostname: " + hostname);
        }
    }

    /**
     * Check if a hostname is in the blocked list.
     */
    public static boolean isBlockedHostname(String hostname) {
        String normalized = normalizeHostname(hostname);
        return BLOCKED_HOSTNAMES.contains(normalized);
    }

    /**
     * Check if an IP address is private/internal.
     */
    public static boolean isPrivateIpAddress(InetAddress address) {
        return address.isLoopbackAddress()
                || address.isSiteLocalAddress()
                || address.isLinkLocalAddress()
                || address.isAnyLocalAddress()
                || isPrivateIpv6(address.getHostAddress());
    }

    /**
     * Check if an IP string represents a private address.
     */
    public static boolean isPrivateIpAddress(String addressStr) {
        try {
            InetAddress addr = InetAddress.getByName(addressStr);
            return isPrivateIpAddress(addr);
        } catch (UnknownHostException e) {
            return false;
        }
    }

    private static boolean isPrivateIpv6(String address) {
        if (address == null)
            return false;
        String lower = address.toLowerCase();
        return PRIVATE_IPV6_PREFIXES.stream().anyMatch(lower::startsWith);
    }

    static String normalizeHostname(String hostname) {
        if (hostname == null)
            return "";
        String h = hostname.trim().toLowerCase();
        // Strip brackets from IPv6
        if (h.startsWith("[") && h.endsWith("]")) {
            h = h.substring(1, h.length() - 1);
        }
        // Strip trailing dot
        if (h.endsWith(".")) {
            h = h.substring(0, h.length() - 1);
        }
        return h;
    }
}
