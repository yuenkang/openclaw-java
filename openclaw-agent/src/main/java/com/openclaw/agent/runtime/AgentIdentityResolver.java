package com.openclaw.agent.runtime;

import com.openclaw.common.config.OpenClawConfig;

/**
 * Resolve agent identity-related configuration: name, emoji, message
 * prefix, response prefix, ack reaction.
 * Mirrors {@code agents/identity.ts}.
 */
public final class AgentIdentityResolver {

    private AgentIdentityResolver() {
    }

    private static final String DEFAULT_ACK_REACTION = "👀";

    // --- Public API ---

    /** Resolve the ack reaction emoji for an agent. */
    public static String resolveAckReaction(String configuredAckReaction, String agentId, OpenClawConfig cfg) {
        if (configuredAckReaction != null && !configuredAckReaction.isBlank()) {
            return configuredAckReaction.trim();
        }
        String emoji = resolveIdentityEmoji(cfg, agentId);
        return (emoji != null && !emoji.isEmpty()) ? emoji : DEFAULT_ACK_REACTION;
    }

    /** Return the identity name wrapped in brackets, e.g. {@code [MyAgent]}. */
    public static String resolveIdentityNamePrefix(OpenClawConfig cfg, String agentId) {
        String name = resolveIdentityName(cfg, agentId);
        return name != null ? "[" + name + "]" : null;
    }

    /** Return just the identity name (no brackets). */
    public static String resolveIdentityName(OpenClawConfig cfg, String agentId) {
        if (cfg == null || cfg.getAgents() == null)
            return null;
        // Try agent-specific config first
        for (var entry : cfg.getAgents().getEntries()) {
            if (entry.getId() != null && entry.getId().equals(agentId)) {
                String name = entry.getName();
                if (name != null && !name.isBlank())
                    return name.trim();
            }
        }
        return null;
    }

    /** Resolve the message prefix with fallback chain. */
    public static String resolveMessagePrefix(
            String configured, String agentId, OpenClawConfig cfg,
            boolean hasAllowFrom, String fallback) {
        if (configured != null)
            return configured;
        if (hasAllowFrom)
            return "";
        String prefix = resolveIdentityNamePrefix(cfg, agentId);
        return prefix != null ? prefix : (fallback != null ? fallback : "[openclaw]");
    }

    // --- Internal helpers ---

    private static String resolveIdentityEmoji(OpenClawConfig cfg, String agentId) {
        // AgentEntry doesn't have an emoji field yet — return null
        // until OpenClawConfig is extended with identity sub-config.
        return null;
    }
}
