package com.openclaw.channel;

import java.util.Map;

/**
 * Resolve whether config writes are allowed for a channel/account.
 * Corresponds to TypeScript's channels/plugins/config-writes.ts.
 */
public final class ChannelConfigWrites {

    private ChannelConfigWrites() {
    }

    /**
     * Check if config writes are enabled for a given channel and account.
     * Returns true by default if not explicitly disabled.
     *
     * @param channelsConfig the "channels" section of config
     * @param channelId      channel ID (e.g. "telegram", "discord")
     * @param accountId      account ID
     * @return true if config writes are allowed
     */
    @SuppressWarnings("unchecked")
    public static boolean isEnabled(Map<String, Object> channelsConfig,
            String channelId,
            String accountId) {
        if (channelId == null || channelId.isBlank()) {
            return true;
        }
        if (channelsConfig == null) {
            return true;
        }
        Object channelObj = channelsConfig.get(channelId);
        if (!(channelObj instanceof Map)) {
            return true;
        }
        Map<String, Object> channelConfig = (Map<String, Object>) channelObj;

        String effectiveAccountId = (accountId != null && !accountId.isBlank())
                ? accountId
                : "default";

        // Check account-level config
        Map<String, Object> accounts = channelConfig.get("accounts") instanceof Map
                ? (Map<String, Object>) channelConfig.get("accounts")
                : null;
        if (accounts != null) {
            Map<String, Object> accountConfig = resolveAccountConfig(accounts, effectiveAccountId);
            if (accountConfig != null) {
                Object val = accountConfig.get("configWrites");
                if (val instanceof Boolean) {
                    return (Boolean) val;
                }
            }
        }

        // Check channel-level config
        Object channelLevel = channelConfig.get("configWrites");
        if (channelLevel instanceof Boolean) {
            return (Boolean) channelLevel;
        }

        return true;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> resolveAccountConfig(Map<String, Object> accounts,
            String accountId) {
        if (accounts.containsKey(accountId)) {
            Object val = accounts.get(accountId);
            return val instanceof Map ? (Map<String, Object>) val : null;
        }
        // Case-insensitive fallback
        for (Map.Entry<String, Object> entry : accounts.entrySet()) {
            if (entry.getKey().equalsIgnoreCase(accountId) && entry.getValue() instanceof Map) {
                return (Map<String, Object>) entry.getValue();
            }
        }
        return null;
    }
}
