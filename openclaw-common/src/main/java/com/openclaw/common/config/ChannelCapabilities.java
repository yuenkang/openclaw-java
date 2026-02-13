package com.openclaw.common.config;

import java.util.List;
import java.util.Map;

/**
 * Channel capabilities resolution — resolves per-account and per-channel
 * capability lists.
 * Corresponds to TypeScript's channel-capabilities.ts.
 */
public final class ChannelCapabilities {

    private ChannelCapabilities() {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Resolve capabilities for a specific channel and account.
     */
    @SuppressWarnings("unchecked")
    public static List<String> resolveChannelCapabilities(
            OpenClawConfig cfg, String channel, String accountId) {
        if (cfg == null || channel == null || channel.isBlank())
            return null;
        String normalizedChannel = channel.trim().toLowerCase();

        var channels = cfg.getChannels();
        if (channels == null)
            return null;

        Map<String, Object> ext = channels.getProviders();
        if (ext == null)
            return null;

        Object channelObj = ext.get(normalizedChannel);
        if (!(channelObj instanceof Map))
            return null;

        Map<String, Object> channelConfig = (Map<String, Object>) channelObj;
        return resolveAccountCapabilities(channelConfig, accountId);
    }

    // =========================================================================
    // Internal
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static List<String> resolveAccountCapabilities(
            Map<String, Object> channelConfig, String accountId) {
        if (channelConfig == null)
            return null;
        String normalizedAccountId = accountId != null ? accountId.trim().toLowerCase() : "";

        // Try account-specific capabilities
        Object accountsObj = channelConfig.get("accounts");
        if (accountsObj instanceof Map) {
            Map<String, Object> accounts = (Map<String, Object>) accountsObj;
            Map<String, Object> acctConfig = findAccountConfig(accounts, normalizedAccountId);
            if (acctConfig != null) {
                List<String> caps = normalizeCapabilities(acctConfig.get("capabilities"));
                if (caps != null)
                    return caps;
            }
        }

        // Fall back to channel-level capabilities
        return normalizeCapabilities(channelConfig.get("capabilities"));
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> findAccountConfig(
            Map<String, Object> accounts, String normalizedAccountId) {
        // Direct match
        Object direct = accounts.get(normalizedAccountId);
        if (direct instanceof Map)
            return (Map<String, Object>) direct;
        // Case-insensitive fallback
        for (var entry : accounts.entrySet()) {
            if (entry.getKey().equalsIgnoreCase(normalizedAccountId) && entry.getValue() instanceof Map) {
                return (Map<String, Object>) entry.getValue();
            }
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    private static List<String> normalizeCapabilities(Object capabilities) {
        if (!(capabilities instanceof List<?> list))
            return null;
        // Only accept string arrays
        if (!list.stream().allMatch(e -> e instanceof String))
            return null;
        List<String> normalized = ((List<String>) capabilities).stream()
                .map(String::trim)
                .filter(s -> !s.isEmpty())
                .toList();
        return normalized.isEmpty() ? null : normalized;
    }
}
