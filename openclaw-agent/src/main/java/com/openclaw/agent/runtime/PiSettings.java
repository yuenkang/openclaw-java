package com.openclaw.agent.runtime;

/**
 * PI-agent compaction reserve-token settings.
 * Mirrors {@code agents/pi-settings.ts}.
 */
public final class PiSettings {

    private PiSettings() {
    }

    public static final int DEFAULT_PI_COMPACTION_RESERVE_TOKENS_FLOOR = 20_000;

    // --- Settings-manager abstraction ---

    public interface PiSettingsManager {
        int getCompactionReserveTokens();

        void applyOverrides(int reserveTokens);
    }

    public record EnsureResult(boolean didOverride, int reserveTokens) {
    }

    /**
     * Ensure the compaction reserve-tokens value is at least
     * {@code minReserveTokens}.
     */
    public static EnsureResult ensurePiCompactionReserveTokens(
            PiSettingsManager settingsManager,
            Integer minReserveTokens) {
        int floor = minReserveTokens != null ? minReserveTokens
                : DEFAULT_PI_COMPACTION_RESERVE_TOKENS_FLOOR;
        int current = settingsManager.getCompactionReserveTokens();
        if (current >= floor) {
            return new EnsureResult(false, current);
        }
        settingsManager.applyOverrides(floor);
        return new EnsureResult(true, floor);
    }

    /**
     * Resolve the compaction reserve-tokens floor.
     * When a proper config type with compaction fields is added,
     * this can be extended to read from it.
     */
    public static int resolveCompactionReserveTokensFloor(int configuredFloor) {
        return configuredFloor >= 0 ? configuredFloor
                : DEFAULT_PI_COMPACTION_RESERVE_TOKENS_FLOOR;
    }
}
