package com.openclaw.agent.runtime;

import com.openclaw.common.config.OpenClawConfig;

import java.util.List;

/**
 * Resolves agent identity configuration — name, emoji, message prefixes.
 * Corresponds to TypeScript's identity.ts.
 */
public class AgentIdentity {

    private static final String DEFAULT_ACK_REACTION = "👀";
    private static final String DEFAULT_MESSAGE_PREFIX = "[openclaw]";

    /**
     * Identity configuration for an agent.
     */
    public record IdentityConfig(
            String name,
            String emoji,
            String description) {
    }

    /**
     * Resolved message prefix configuration.
     */
    public record MessagesConfig(
            String messagePrefix,
            String responsePrefix) {
    }

    /**
     * Resolve the identity config for an agent from OpenClawConfig.
     */
    public static IdentityConfig resolveAgentIdentity(OpenClawConfig cfg, String agentId) {
        if (cfg == null || cfg.getAgents() == null)
            return null;
        List<OpenClawConfig.AgentEntry> entries = cfg.getAgents().getEntries();
        for (OpenClawConfig.AgentEntry entry : entries) {
            if (agentId != null && agentId.equals(entry.getId())) {
                return new IdentityConfig(
                        entry.getName(),
                        null, // emoji not in AgentEntry yet
                        entry.getDescription());
            }
        }
        return null;
    }

    /**
     * Resolve the acknowledgement reaction emoji.
     */
    public static String resolveAckReaction(OpenClawConfig cfg, String agentId) {
        IdentityConfig identity = resolveAgentIdentity(cfg, agentId);
        if (identity != null && identity.emoji() != null && !identity.emoji().isBlank()) {
            return identity.emoji().trim();
        }
        return DEFAULT_ACK_REACTION;
    }

    /**
     * Resolve the identity name prefix (e.g. "[MyBot]").
     */
    public static String resolveIdentityNamePrefix(OpenClawConfig cfg, String agentId) {
        IdentityConfig identity = resolveAgentIdentity(cfg, agentId);
        if (identity == null || identity.name() == null || identity.name().isBlank()) {
            return null;
        }
        return "[" + identity.name().trim() + "]";
    }

    /**
     * Resolve just the identity name (without brackets).
     */
    public static String resolveIdentityName(OpenClawConfig cfg, String agentId) {
        IdentityConfig identity = resolveAgentIdentity(cfg, agentId);
        if (identity == null || identity.name() == null || identity.name().isBlank()) {
            return null;
        }
        return identity.name().trim();
    }

    /**
     * Resolve the message prefix for outgoing messages.
     *
     * @param cfg          config
     * @param agentId      agent id
     * @param hasAllowFrom whether the channel has allowFrom configured
     * @param fallback     fallback prefix if nothing configured
     */
    public static String resolveMessagePrefix(
            OpenClawConfig cfg, String agentId, boolean hasAllowFrom, String fallback) {
        // If allowFrom is configured, no prefix needed
        if (hasAllowFrom) {
            return "";
        }
        // Try identity name
        String namePrefix = resolveIdentityNamePrefix(cfg, agentId);
        if (namePrefix != null) {
            return namePrefix;
        }
        return fallback != null ? fallback : DEFAULT_MESSAGE_PREFIX;
    }

    /**
     * Resolve response prefix for a specific channel/account context.
     */
    public static String resolveResponsePrefix(
            OpenClawConfig cfg, String agentId, String channel, String accountId) {
        // For now, just resolve from identity name
        // Full channel-level resolution can be added later
        return resolveIdentityNamePrefix(cfg, agentId);
    }

    /**
     * Resolve effective messages config (message prefix + response prefix).
     */
    public static MessagesConfig resolveEffectiveMessagesConfig(
            OpenClawConfig cfg, String agentId, boolean hasAllowFrom,
            String channel, String accountId) {
        return new MessagesConfig(
                resolveMessagePrefix(cfg, agentId, hasAllowFrom, null),
                resolveResponsePrefix(cfg, agentId, channel, accountId));
    }
}
