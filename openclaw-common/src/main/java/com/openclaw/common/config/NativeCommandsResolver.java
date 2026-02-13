package com.openclaw.common.config;

import java.util.Set;

/**
 * Native commands and skills resolution for channel providers.
 * Corresponds to TypeScript's commands.ts.
 */
public final class NativeCommandsResolver {

    private NativeCommandsResolver() {
    }

    /** Channels that enable native commands by default. */
    private static final Set<String> AUTO_ENABLED_CHANNELS = Set.of("discord", "telegram");

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Resolve whether native skills are enabled for the given provider.
     *
     * @param providerId      channel provider ID (e.g. "discord", "telegram",
     *                        "slack")
     * @param providerSetting per-provider setting (true/false/null for "auto")
     * @param globalSetting   global setting (true/false/null for "auto")
     */
    public static boolean resolveNativeSkillsEnabled(
            String providerId, Boolean providerSetting, Boolean globalSetting) {
        Boolean setting = providerSetting != null ? providerSetting : globalSetting;
        if (setting != null)
            return setting;
        return resolveAutoDefault(providerId);
    }

    /**
     * Resolve whether native commands are enabled for the given provider.
     */
    public static boolean resolveNativeCommandsEnabled(
            String providerId, Boolean providerSetting, Boolean globalSetting) {
        Boolean setting = providerSetting != null ? providerSetting : globalSetting;
        if (setting != null)
            return setting;
        return resolveAutoDefault(providerId);
    }

    /**
     * Check if native commands are explicitly disabled by configuration.
     */
    public static boolean isNativeCommandsExplicitlyDisabled(
            Boolean providerSetting, Boolean globalSetting) {
        if (Boolean.FALSE.equals(providerSetting))
            return true;
        if (providerSetting == null)
            return Boolean.FALSE.equals(globalSetting);
        return false;
    }

    // =========================================================================
    // Internal
    // =========================================================================

    private static boolean resolveAutoDefault(String providerId) {
        if (providerId == null)
            return false;
        return AUTO_ENABLED_CHANNELS.contains(providerId.trim().toLowerCase());
    }
}
