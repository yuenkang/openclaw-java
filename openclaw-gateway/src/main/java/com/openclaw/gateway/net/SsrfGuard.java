package com.openclaw.gateway.net;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

/**
 * Server-Side Request Forgery (SSRF) protection.
 * Validates hostnames and IP addresses to prevent access to private/internal
 * networks.
 * Corresponds to TypeScript's infra/net/ssrf.ts.
 */
public final class SsrfGuard {

    private SsrfGuard() {
    }

    // ── Exception ───────────────────────────────────────────────────────

    /**
     * Thrown when a request is blocked by SSRF policy.
     */
    public static class SsrfBlockedError extends RuntimeException {
        public SsrfBlockedError(String message) {
            super(message);
        }
    }

    // ── Policy ──────────────────────────────────────────────────────────

    /**
     * SSRF policy configuration.
     *
     * @param allowPrivateNetwork whether to allow access to private/internal IPs
     * @param allowedHostnames    set of explicitly allowed hostnames (normalized)
     */
    public record Policy(boolean allowPrivateNetwork, Set<String> allowedHostnames) {

        /** Default: block private networks, no custom allowlist. */
        public static final Policy DEFAULT = new Policy(false, Set.of());

        public Policy {
            allowedHostnames = allowedHostnames != null
                    ? Collections.unmodifiableSet(new HashSet<>(allowedHostnames))
                    : Set.of();
        }
    }

    // ── Blocked hostnames ───────────────────────────────────────────────

    private static final Set<String> BLOCKED_HOSTNAMES = Set.of(
            "localhost",
            "metadata.google.internal");

    private static final String[] PRIVATE_IPV6_PREFIXES = { "fe80:", "fec0:", "fc", "fd" };

    // ── Hostname normalization ──────────────────────────────────────────

    /**
     * Normalize a hostname (lowercase, strip trailing dot, unwrap brackets).
     */
    public static String normalizeHostname(String hostname) {
        if (hostname == null) {
            return "";
        }
        String normalized = hostname.trim().toLowerCase(Locale.ROOT).replaceAll("\\.$", "");
        if (normalized.startsWith("[") && normalized.endsWith("]")) {
            normalized = normalized.substring(1, normalized.length() - 1);
        }
        return normalized;
    }

    /**
     * Normalize a list of hostnames into a set.
     */
    public static Set<String> normalizeHostnameSet(String... hostnames) {
        if (hostnames == null || hostnames.length == 0) {
            return Set.of();
        }
        Set<String> result = new HashSet<>();
        for (String h : hostnames) {
            String n = normalizeHostname(h);
            if (!n.isEmpty()) {
                result.add(n);
            }
        }
        return Collections.unmodifiableSet(result);
    }

    // ── IPv4 parsing ────────────────────────────────────────────────────

    /**
     * Parse a dotted-decimal IPv4 address into 4 octets.
     *
     * @return octet array, or {@code null} if invalid
     */
    static int[] parseIpv4(String address) {
        String[] parts = address.split("\\.");
        if (parts.length != 4) {
            return null;
        }
        int[] octets = new int[4];
        for (int i = 0; i < 4; i++) {
            try {
                int v = Integer.parseInt(parts[i]);
                if (v < 0 || v > 255) {
                    return null;
                }
                octets[i] = v;
            } catch (NumberFormatException e) {
                return null;
            }
        }
        return octets;
    }

    /**
     * Parse an IPv4 address from an IPv4-mapped IPv6 representation (e.g.
     * "::ffff:10.0.0.1").
     */
    static int[] parseIpv4FromMappedIpv6(String mapped) {
        if (mapped.contains(".")) {
            return parseIpv4(mapped);
        }
        String[] parts = Arrays.stream(mapped.split(":"))
                .filter(s -> !s.isEmpty())
                .toArray(String[]::new);
        if (parts.length == 1) {
            try {
                long value = Long.parseLong(parts[0], 16);
                if (value < 0 || value > 0xFFFFFFFFL) {
                    return null;
                }
                return new int[] {
                        (int) ((value >>> 24) & 0xFF),
                        (int) ((value >>> 16) & 0xFF),
                        (int) ((value >>> 8) & 0xFF),
                        (int) (value & 0xFF)
                };
            } catch (NumberFormatException e) {
                return null;
            }
        }
        if (parts.length != 2) {
            return null;
        }
        try {
            int high = Integer.parseInt(parts[0], 16);
            int low = Integer.parseInt(parts[1], 16);
            if (high < 0 || low < 0 || high > 0xFFFF || low > 0xFFFF) {
                return null;
            }
            int value = (high << 16) + low;
            return new int[] {
                    (value >>> 24) & 0xFF,
                    (value >>> 16) & 0xFF,
                    (value >>> 8) & 0xFF,
                    value & 0xFF
            };
        } catch (NumberFormatException e) {
            return null;
        }
    }

    // ── Private IP detection ────────────────────────────────────────────

    /**
     * Check if an IPv4 address (as 4 octets) is in a private/reserved range.
     */
    static boolean isPrivateIpv4(int[] octets) {
        int o1 = octets[0];
        int o2 = octets[1];
        if (o1 == 0)
            return true; // 0.0.0.0/8
        if (o1 == 10)
            return true; // 10.0.0.0/8
        if (o1 == 127)
            return true; // 127.0.0.0/8 loopback
        if (o1 == 169 && o2 == 254)
            return true; // 169.254.0.0/16 link-local
        if (o1 == 172 && o2 >= 16 && o2 <= 31)
            return true; // 172.16.0.0/12
        if (o1 == 192 && o2 == 168)
            return true; // 192.168.0.0/16
        if (o1 == 100 && o2 >= 64 && o2 <= 127)
            return true; // 100.64.0.0/10 CGNAT
        return false;
    }

    /**
     * Check if an IP address string is a private/internal address.
     *
     * @param address IP address (IPv4, IPv6, or IPv4-mapped IPv6)
     * @return {@code true} if the address is private/internal
     */
    public static boolean isPrivateIpAddress(String address) {
        String normalized = address.trim().toLowerCase(Locale.ROOT);
        if (normalized.startsWith("[") && normalized.endsWith("]")) {
            normalized = normalized.substring(1, normalized.length() - 1);
        }
        if (normalized.isEmpty()) {
            return false;
        }

        // IPv4-mapped IPv6 (::ffff:x.x.x.x)
        if (normalized.startsWith("::ffff:")) {
            String mapped = normalized.substring("::ffff:".length());
            int[] ipv4 = parseIpv4FromMappedIpv6(mapped);
            if (ipv4 != null) {
                return isPrivateIpv4(ipv4);
            }
        }

        // IPv6
        if (normalized.contains(":")) {
            if ("::".equals(normalized) || "::1".equals(normalized)) {
                return true;
            }
            for (String prefix : PRIVATE_IPV6_PREFIXES) {
                if (normalized.startsWith(prefix)) {
                    return true;
                }
            }
            return false;
        }

        // IPv4
        int[] ipv4 = parseIpv4(normalized);
        if (ipv4 == null) {
            return false;
        }
        return isPrivateIpv4(ipv4);
    }

    /**
     * Check if a hostname should be blocked (localhost, .local, .internal, etc.).
     */
    public static boolean isBlockedHostname(String hostname) {
        String normalized = normalizeHostname(hostname);
        if (normalized.isEmpty()) {
            return false;
        }
        if (BLOCKED_HOSTNAMES.contains(normalized)) {
            return true;
        }
        return normalized.endsWith(".localhost")
                || normalized.endsWith(".local")
                || normalized.endsWith(".internal");
    }

    // ── Validation ──────────────────────────────────────────────────────

    /**
     * Validate a hostname against SSRF policy. Throws {@link SsrfBlockedError} if
     * blocked.
     *
     * @param hostname the hostname to validate
     * @param policy   SSRF policy (may be {@code null} for defaults)
     * @throws SsrfBlockedError if the hostname is blocked
     */
    public static void validateHostname(String hostname, Policy policy) {
        if (policy == null) {
            policy = Policy.DEFAULT;
        }
        String normalized = normalizeHostname(hostname);
        if (normalized.isEmpty()) {
            throw new SsrfBlockedError("Invalid hostname");
        }

        boolean isExplicitAllowed = policy.allowedHostnames().contains(normalized);
        if (policy.allowPrivateNetwork() || isExplicitAllowed) {
            return;
        }

        if (isBlockedHostname(normalized)) {
            throw new SsrfBlockedError("Blocked hostname: " + hostname);
        }
        if (isPrivateIpAddress(normalized)) {
            throw new SsrfBlockedError("Blocked: private/internal IP address");
        }
    }

    /**
     * Validate a hostname and its resolved IP addresses against SSRF policy.
     * Performs DNS resolution and checks all returned addresses.
     *
     * @param hostname the hostname to validate and resolve
     * @param policy   SSRF policy
     * @throws SsrfBlockedError if blocked
     */
    public static void validateHostnameWithDns(String hostname, Policy policy) {
        validateHostname(hostname, policy);

        if (policy == null) {
            policy = Policy.DEFAULT;
        }
        String normalized = normalizeHostname(hostname);
        boolean isExplicitAllowed = policy.allowedHostnames().contains(normalized);

        if (policy.allowPrivateNetwork() || isExplicitAllowed) {
            return;
        }

        // DNS resolution check
        try {
            InetAddress[] addresses = InetAddress.getAllByName(hostname);
            for (InetAddress addr : addresses) {
                if (isPrivateIpAddress(addr.getHostAddress())) {
                    throw new SsrfBlockedError("Blocked: resolves to private/internal IP address");
                }
            }
        } catch (UnknownHostException e) {
            throw new SsrfBlockedError("Unable to resolve hostname: " + hostname);
        }
    }

    /**
     * Assert that a hostname resolves to public (non-private) addresses.
     *
     * @param hostname the hostname to check
     * @throws SsrfBlockedError if blocked or resolves to private IP
     */
    public static void assertPublicHostname(String hostname) {
        validateHostnameWithDns(hostname, Policy.DEFAULT);
    }
}
