package com.openclaw.common.config;

/**
 * Port derivation for gateway sub-services.
 * Corresponds to TypeScript's port-defaults.ts.
 */
public final class PortDefaults {

    private PortDefaults() {
    }

    public static final int DEFAULT_BRIDGE_PORT = 18790;
    public static final int DEFAULT_BROWSER_CONTROL_PORT = 18791;
    public static final int DEFAULT_CANVAS_HOST_PORT = 18793;
    public static final int DEFAULT_BROWSER_CDP_PORT_RANGE_START = 18800;
    public static final int DEFAULT_BROWSER_CDP_PORT_RANGE_END = 18899;

    // =========================================================================
    // Types
    // =========================================================================

    public record PortRange(int start, int end) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    public static int deriveDefaultBridgePort(int gatewayPort) {
        return derivePort(gatewayPort, 1, DEFAULT_BRIDGE_PORT);
    }

    public static int deriveDefaultBrowserControlPort(int gatewayPort) {
        return derivePort(gatewayPort, 2, DEFAULT_BROWSER_CONTROL_PORT);
    }

    public static int deriveDefaultCanvasHostPort(int gatewayPort) {
        return derivePort(gatewayPort, 4, DEFAULT_CANVAS_HOST_PORT);
    }

    public static PortRange deriveDefaultBrowserCdpPortRange(int browserControlPort) {
        int start = derivePort(browserControlPort, 9, DEFAULT_BROWSER_CDP_PORT_RANGE_START);
        int end = clampPort(
                start + (DEFAULT_BROWSER_CDP_PORT_RANGE_END - DEFAULT_BROWSER_CDP_PORT_RANGE_START),
                DEFAULT_BROWSER_CDP_PORT_RANGE_END);
        if (end < start) {
            return new PortRange(start, start);
        }
        return new PortRange(start, end);
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    public static boolean isValidPort(int port) {
        return port > 0 && port <= 65535;
    }

    private static int clampPort(int port, int fallback) {
        return isValidPort(port) ? port : fallback;
    }

    private static int derivePort(int base, int offset, int fallback) {
        return clampPort(base + offset, fallback);
    }
}
