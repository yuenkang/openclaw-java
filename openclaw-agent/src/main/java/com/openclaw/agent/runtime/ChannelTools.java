package com.openclaw.agent.runtime;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Channel tool injection and supported actions resolution.
 * Corresponds to TypeScript agents/channel-tools.ts.
 *
 * <p>
 * Aggregates channel plugin tools and resolves supported message actions
 * for the current channel or across all configured channels.
 * </p>
 */
@Slf4j
public final class ChannelTools {

    private ChannelTools() {
    }

    /** Standard channel message action names. */
    public static final List<String> STANDARD_ACTIONS = List.of(
            "send", "reply", "react", "delete", "edit",
            "poll", "pin", "threads", "forward");

    /**
     * Channel agent tool definition.
     */
    public record ChannelAgentTool(
            String name,
            String description,
            String channel) {
    }

    /**
     * Get supported message actions for a specific channel.
     *
     * @param cfg     OpenClaw config
     * @param channel Channel ID
     * @return List of supported action names
     */
    public static List<String> listChannelSupportedActions(
            OpenClawConfig cfg, String channel) {
        if (channel == null || channel.isBlank())
            return List.of();

        // TODO: look up channel plugin and call listActions
        // For now, return standard actions for known channels
        String normalized = normalizeChannelId(channel);
        if (normalized == null)
            return List.of();

        log.debug("listChannelSupportedActions: stub for channel={}", normalized);
        return STANDARD_ACTIONS;
    }

    /**
     * Get all supported message actions across all configured channels.
     */
    public static List<String> listAllChannelSupportedActions(OpenClawConfig cfg) {
        // TODO: iterate channel plugins
        log.debug("listAllChannelSupportedActions: stub");
        return STANDARD_ACTIONS;
    }

    /**
     * Get channel-specific agent tools (e.g. login, auth).
     */
    public static List<ChannelAgentTool> listChannelAgentTools(OpenClawConfig cfg) {
        // TODO: iterate channel plugins and aggregate agentTools
        log.debug("listChannelAgentTools: stub");
        return List.of();
    }

    /**
     * Resolve message tool hints for a specific channel.
     */
    public static List<String> resolveChannelMessageToolHints(
            OpenClawConfig cfg, String channel, String accountId) {
        String normalized = normalizeChannelId(channel);
        if (normalized == null)
            return List.of();

        // TODO: look up channel dock and resolve agent prompt hints
        log.debug("resolveChannelMessageToolHints: stub for channel={}", normalized);
        return List.of();
    }

    /**
     * Normalize a channel identifier.
     */
    static String normalizeChannelId(String channel) {
        if (channel == null || channel.isBlank())
            return null;
        return channel.trim().toLowerCase();
    }
}
