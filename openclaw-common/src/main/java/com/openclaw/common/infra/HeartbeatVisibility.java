package com.openclaw.common.infra;

/**
 * Resolves heartbeat visibility settings for a channel.
 * <p>
 * Controls whether heartbeat results are shown (OK/alerts), or just
 * emit indicator events. Supports layered configuration:
 * per-account → per-channel → defaults → global defaults.
 * <p>
 * Port of: infra/heartbeat-visibility.ts
 */
public class HeartbeatVisibility {

    /**
     * Resolved heartbeat visibility flags.
     */
    public record ResolvedVisibility(
            boolean showOk,
            boolean showAlerts,
            boolean useIndicator) {
    }

    /** Default visibility: silent OK, show alerts, use indicator. */
    public static final ResolvedVisibility DEFAULT = new ResolvedVisibility(false, true, true);

    /**
     * Resolve visibility from a layered config.
     *
     * @param defaultShowOk       channel defaults for showOk
     * @param defaultShowAlerts   channel defaults for showAlerts
     * @param defaultUseIndicator channel defaults for useIndicator
     * @param channelShowOk       per-channel config
     * @param channelShowAlerts   per-channel config
     * @param channelUseIndicator per-channel config
     * @param accountShowOk       per-account config (most specific)
     * @param accountShowAlerts   per-account config
     * @param accountUseIndicator per-account config
     */
    public static ResolvedVisibility resolve(
            Boolean defaultShowOk, Boolean defaultShowAlerts, Boolean defaultUseIndicator,
            Boolean channelShowOk, Boolean channelShowAlerts, Boolean channelUseIndicator,
            Boolean accountShowOk, Boolean accountShowAlerts, Boolean accountUseIndicator) {
        return new ResolvedVisibility(
                coalesce(accountShowOk, channelShowOk, defaultShowOk, DEFAULT.showOk()),
                coalesce(accountShowAlerts, channelShowAlerts, defaultShowAlerts, DEFAULT.showAlerts()),
                coalesce(accountUseIndicator, channelUseIndicator, defaultUseIndicator, DEFAULT.useIndicator()));
    }

    /**
     * Simple resolve with just channel defaults — no per-account overrides.
     */
    public static ResolvedVisibility resolveDefaults(
            Boolean showOk, Boolean showAlerts, Boolean useIndicator) {
        return new ResolvedVisibility(
                showOk != null ? showOk : DEFAULT.showOk(),
                showAlerts != null ? showAlerts : DEFAULT.showAlerts(),
                useIndicator != null ? useIndicator : DEFAULT.useIndicator());
    }

    private static boolean coalesce(Boolean a, Boolean b, Boolean c, boolean fallback) {
        if (a != null)
            return a;
        if (b != null)
            return b;
        if (c != null)
            return c;
        return fallback;
    }
}
