package com.openclaw.common.config;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Group/channel policy resolution — allowlist, requireMention, per-sender tool
 * policies.
 * Corresponds to TypeScript's group-policy.ts.
 */
public final class GroupPolicyResolver {

    private GroupPolicyResolver() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    public record ChannelGroupPolicy(
            boolean allowlistEnabled,
            boolean allowed,
            Map<String, Object> groupConfig,
            Map<String, Object> defaultConfig) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Resolve group policy for a channel + group + account combo.
     */
    @SuppressWarnings("unchecked")
    public static ChannelGroupPolicy resolveChannelGroupPolicy(
            OpenClawConfig cfg, String channel, String groupId, String accountId) {

        Map<String, Object> groups = resolveChannelGroups(cfg, channel, accountId);
        boolean allowlistEnabled = groups != null && !groups.isEmpty();
        String normalizedId = groupId != null ? groupId.trim() : null;
        Map<String, Object> groupConfig = (normalizedId != null && !normalizedId.isEmpty() && groups != null)
                ? (Map<String, Object>) groups.get(normalizedId)
                : null;
        Map<String, Object> defaultConfig = groups != null ? (Map<String, Object>) groups.get("*") : null;
        boolean allowAll = allowlistEnabled && groups != null && groups.containsKey("*");
        boolean allowed = !allowlistEnabled || allowAll ||
                (normalizedId != null && !normalizedId.isEmpty() && groups != null && groups.containsKey(normalizedId));

        return new ChannelGroupPolicy(allowlistEnabled, allowed, groupConfig, defaultConfig);
    }

    /**
     * Resolve whether mention is required for a group chat context.
     */
    public static boolean resolveChannelGroupRequireMention(
            OpenClawConfig cfg, String channel, String groupId, String accountId,
            Boolean requireMentionOverride, String overrideOrder) {

        if (overrideOrder == null)
            overrideOrder = "after-config";
        ChannelGroupPolicy policy = resolveChannelGroupPolicy(cfg, channel, groupId, accountId);

        Boolean configMention = extractBoolean(policy.groupConfig(), "requireMention");
        if (configMention == null) {
            configMention = extractBoolean(policy.defaultConfig(), "requireMention");
        }

        if ("before-config".equals(overrideOrder) && requireMentionOverride != null) {
            return requireMentionOverride;
        }
        if (configMention != null) {
            return configMention;
        }
        if (!"before-config".equals(overrideOrder) && requireMentionOverride != null) {
            return requireMentionOverride;
        }
        return true; // default: require mention in groups
    }

    /**
     * Resolve tool policy for a group chat, considering per-sender overrides.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> resolveChannelGroupToolsPolicy(
            OpenClawConfig cfg, String channel, String groupId, String accountId,
            String senderId, String senderName, String senderUsername, String senderE164) {

        ChannelGroupPolicy policy = resolveChannelGroupPolicy(cfg, channel, groupId, accountId);

        // Try group-level sender-specific policy
        Map<String, Object> groupSenderPolicy = resolveToolsBySender(
                extractMap(policy.groupConfig(), "toolsBySender"),
                senderId, senderName, senderUsername, senderE164);
        if (groupSenderPolicy != null)
            return groupSenderPolicy;

        // Try group-level tools
        Map<String, Object> groupTools = extractMap(policy.groupConfig(), "tools");
        if (groupTools != null)
            return groupTools;

        // Try default sender-specific policy
        Map<String, Object> defaultSenderPolicy = resolveToolsBySender(
                extractMap(policy.defaultConfig(), "toolsBySender"),
                senderId, senderName, senderUsername, senderE164);
        if (defaultSenderPolicy != null)
            return defaultSenderPolicy;

        // Try default tools
        return extractMap(policy.defaultConfig(), "tools");
    }

    // =========================================================================
    // Sender-based tool policy resolution
    // =========================================================================

    @SuppressWarnings("unchecked")
    public static Map<String, Object> resolveToolsBySender(
            Map<String, Object> toolsBySender,
            String senderId, String senderName, String senderUsername, String senderE164) {

        if (toolsBySender == null || toolsBySender.isEmpty())
            return null;

        Map<String, Map<String, Object>> normalized = new LinkedHashMap<>();
        Map<String, Object> wildcard = null;

        for (var entry : toolsBySender.entrySet()) {
            if (entry.getValue() == null)
                continue;
            String key = normalizeSenderKey(entry.getKey());
            if (key.isEmpty())
                continue;
            if ("*".equals(key)) {
                wildcard = (Map<String, Object>) entry.getValue();
                continue;
            }
            normalized.putIfAbsent(key, (Map<String, Object>) entry.getValue());
        }

        List<String> candidates = new ArrayList<>();
        addCandidate(candidates, senderId);
        addCandidate(candidates, senderE164);
        addCandidate(candidates, senderUsername);
        addCandidate(candidates, senderName);

        for (String candidate : candidates) {
            String key = normalizeSenderKey(candidate);
            if (key.isEmpty())
                continue;
            Map<String, Object> match = normalized.get(key);
            if (match != null)
                return match;
        }
        return wildcard;
    }

    // =========================================================================
    // Internal helpers
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static Map<String, Object> resolveChannelGroups(
            OpenClawConfig cfg, String channel, String accountId) {
        if (cfg.getChannels() == null || channel == null)
            return null;

        Map<String, Object> channelsMap = cfg.getChannels().getProviders();
        if (channelsMap == null)
            return null;
        Object channelObj = channelsMap.get(channel.toLowerCase());
        if (!(channelObj instanceof Map))
            return null;

        Map<String, Object> channelConfig = (Map<String, Object>) channelObj;
        String normalizedAccountId = accountId != null ? accountId.trim().toLowerCase() : "";

        // Try account-specific groups first
        Object accountsObj = channelConfig.get("accounts");
        if (accountsObj instanceof Map) {
            Map<String, Object> accounts = (Map<String, Object>) accountsObj;
            Object acctObj = accounts.get(normalizedAccountId);
            if (acctObj == null) {
                // case-insensitive fallback
                for (var e : accounts.entrySet()) {
                    if (e.getKey().equalsIgnoreCase(normalizedAccountId)) {
                        acctObj = e.getValue();
                        break;
                    }
                }
            }
            if (acctObj instanceof Map) {
                Object groups = ((Map<String, Object>) acctObj).get("groups");
                if (groups instanceof Map)
                    return (Map<String, Object>) groups;
            }
        }

        // Fall back to channel-level groups
        Object groups = channelConfig.get("groups");
        return groups instanceof Map ? (Map<String, Object>) groups : null;
    }

    static String normalizeSenderKey(String value) {
        if (value == null)
            return "";
        String trimmed = value.trim();
        if (trimmed.isEmpty())
            return "";
        String withoutAt = trimmed.startsWith("@") ? trimmed.substring(1) : trimmed;
        return withoutAt.toLowerCase();
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> extractMap(Map<String, Object> map, String key) {
        if (map == null)
            return null;
        Object v = map.get(key);
        return v instanceof Map ? (Map<String, Object>) v : null;
    }

    private static Boolean extractBoolean(Map<String, Object> map, String key) {
        if (map == null)
            return null;
        Object v = map.get(key);
        return v instanceof Boolean b ? b : null;
    }

    private static void addCandidate(List<String> candidates, String value) {
        if (value != null && !value.isBlank())
            candidates.add(value.trim());
    }
}
