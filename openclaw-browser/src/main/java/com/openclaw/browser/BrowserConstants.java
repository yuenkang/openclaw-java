package com.openclaw.browser;

/**
 * Browser module constants — default ports, timeouts, and feature flags.
 * Corresponds to TypeScript's browser/constants.ts.
 */
public final class BrowserConstants {

    private BrowserConstants() {}

    // ==================== Ports ====================

    /** Default port for the browser control HTTP server. */
    public static final int DEFAULT_CONTROL_PORT = 18791;

    /** Default starting port for CDP debugging connections. */
    public static final int DEFAULT_CDP_PORT_RANGE_START = 9222;

    /** Default CDP port range size. */
    public static final int DEFAULT_CDP_PORT_RANGE_SIZE = 10;

    // ==================== Feature Flags ====================

    /** Whether the browser subsystem is enabled by default. */
    public static final boolean DEFAULT_BROWSER_ENABLED = true;

    /** Whether JavaScript evaluation via CDP is enabled. */
    public static final boolean DEFAULT_EVALUATE_ENABLED = true;

    // ==================== Profile Names ====================

    /** The built-in default profile name. */
    public static final String DEFAULT_PROFILE_NAME = "openclaw";

    /** Default profile color (hex). */
    public static final String DEFAULT_PROFILE_COLOR = "#4A90D9";

    // ==================== Timeouts ====================

    /** Default remote CDP connection timeout in milliseconds. */
    public static final int DEFAULT_REMOTE_CDP_TIMEOUT_MS = 10_000;

    /** Default remote CDP handshake timeout in milliseconds. */
    public static final int DEFAULT_REMOTE_CDP_HANDSHAKE_TIMEOUT_MS = 5_000;

    // ==================== Protocols ====================

    /** Default CDP protocol. */
    public static final String DEFAULT_CDP_PROTOCOL = "http";

    /** Default CDP host (loopback). */
    public static final String DEFAULT_CDP_HOST = "127.0.0.1";
}
