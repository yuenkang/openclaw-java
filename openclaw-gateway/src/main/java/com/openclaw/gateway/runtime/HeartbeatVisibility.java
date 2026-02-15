package com.openclaw.gateway.runtime;

/**
 * Resolve heartbeat visibility settings per channel and account.
 * Controls whether heartbeat messages are shown, suppressed, or indicator-only.
 * Corresponds to TypeScript's infra/heartbeat-visibility.ts.
 */
public final class HeartbeatVisibility {

    private HeartbeatVisibility() {
    }

    /**
     * Resolved visibility flags.
     *
     * @param showOk       whether to send "all OK" heartbeat messages
     * @param showAlerts   whether to send content/alert messages
     * @param useIndicator whether to emit indicator events (for UI status display)
     */
    public record Resolved(boolean showOk, boolean showAlerts, boolean useIndicator) {
    }

    /** Default: silent OK, show alerts, emit indicators. */
    public static final Resolved DEFAULT = new Resolved(false, true, true);

    /**
     * Resolve heartbeat visibility for a channel with layered config precedence:
     * per-account > per-channel > channel-defaults > global defaults.
     *
     * @param channelShowOk       per-channel showOk override (nullable)
     * @param channelShowAlerts   per-channel showAlerts override (nullable)
     * @param channelUseIndicator per-channel useIndicator override (nullable)
     * @param accountShowOk       per-account showOk override (nullable)
     * @param accountShowAlerts   per-account showAlerts override (nullable)
     * @param accountUseIndicator per-account useIndicator override (nullable)
     * @param defaultShowOk       channel-defaults showOk override (nullable)
     * @param defaultShowAlerts   channel-defaults showAlerts override (nullable)
     * @param defaultUseIndicator channel-defaults useIndicator override (nullable)
     * @return resolved visibility
     */
    public static Resolved resolve(
            Boolean channelShowOk, Boolean channelShowAlerts, Boolean channelUseIndicator,
            Boolean accountShowOk, Boolean accountShowAlerts, Boolean accountUseIndicator,
            Boolean defaultShowOk, Boolean defaultShowAlerts, Boolean defaultUseIndicator) {

        boolean showOk = coalesce(accountShowOk, channelShowOk, defaultShowOk, DEFAULT.showOk());
        boolean showAlerts = coalesce(accountShowAlerts, channelShowAlerts, defaultShowAlerts, DEFAULT.showAlerts());
        boolean useIndicator = coalesce(accountUseIndicator, channelUseIndicator, defaultUseIndicator,
                DEFAULT.useIndicator());

        return new Resolved(showOk, showAlerts, useIndicator);
    }

    /**
     * Simplified resolve with only channel-level overrides.
     */
    public static Resolved resolve(Boolean showOk, Boolean showAlerts, Boolean useIndicator) {
        return new Resolved(
                showOk != null ? showOk : DEFAULT.showOk(),
                showAlerts != null ? showAlerts : DEFAULT.showAlerts(),
                useIndicator != null ? useIndicator : DEFAULT.useIndicator());
    }

    private static boolean coalesce(Boolean... values) {
        for (Boolean v : values) {
            if (v != null) {
                return v;
            }
        }
        return false;
    }
}
