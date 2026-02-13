package com.openclaw.gateway.protocol;

import java.util.Locale;
import java.util.Set;

/**
 * Gateway client identifiers, modes, and capabilities.
 * Mirrors {@code protocol/client-info.ts}.
 */
public final class ClientInfo {

    private ClientInfo() {
    }

    // ── Client IDs ──────────────────────────────────────────────

    public static final String WEBCHAT_UI = "webchat-ui";
    public static final String CONTROL_UI = "openclaw-control-ui";
    public static final String WEBCHAT = "webchat";
    public static final String CLI = "cli";
    public static final String GATEWAY_CLIENT = "gateway-client";
    public static final String MACOS_APP = "openclaw-macos";
    public static final String IOS_APP = "openclaw-ios";
    public static final String ANDROID_APP = "openclaw-android";
    public static final String NODE_HOST = "node-host";
    public static final String TEST = "test";
    public static final String FINGERPRINT = "fingerprint";
    public static final String PROBE = "openclaw-probe";

    private static final Set<String> CLIENT_ID_SET = Set.of(
            WEBCHAT_UI, CONTROL_UI, WEBCHAT, CLI, GATEWAY_CLIENT,
            MACOS_APP, IOS_APP, ANDROID_APP, NODE_HOST, TEST,
            FINGERPRINT, PROBE);

    // ── Client Modes ────────────────────────────────────────────

    public static final String MODE_WEBCHAT = "webchat";
    public static final String MODE_CLI = "cli";
    public static final String MODE_UI = "ui";
    public static final String MODE_BACKEND = "backend";
    public static final String MODE_NODE = "node";
    public static final String MODE_PROBE = "probe";
    public static final String MODE_TEST = "test";

    private static final Set<String> CLIENT_MODE_SET = Set.of(
            MODE_WEBCHAT, MODE_CLI, MODE_UI, MODE_BACKEND,
            MODE_NODE, MODE_PROBE, MODE_TEST);

    // ── Client Caps ─────────────────────────────────────────────

    public static final String CAP_TOOL_EVENTS = "tool-events";

    // ── Normalize helpers ───────────────────────────────────────

    /**
     * Normalize a raw client-id string to a known {@code GatewayClientId},
     * or {@code null} if unknown or blank.
     */
    public static String normalizeClientId(String raw) {
        if (raw == null || raw.isBlank()) {
            return null;
        }
        String normalized = raw.trim().toLowerCase(Locale.ROOT);
        return CLIENT_ID_SET.contains(normalized) ? normalized : null;
    }

    /**
     * Normalize a raw client-mode string, or {@code null} if unknown.
     */
    public static String normalizeClientMode(String raw) {
        if (raw == null || raw.isBlank()) {
            return null;
        }
        String normalized = raw.trim().toLowerCase(Locale.ROOT);
        return CLIENT_MODE_SET.contains(normalized) ? normalized : null;
    }

    /**
     * Check whether the given caps list contains a specific capability.
     */
    public static boolean hasClientCap(java.util.List<String> caps, String cap) {
        if (caps == null || caps.isEmpty()) {
            return false;
        }
        return caps.contains(cap);
    }
}
