package com.openclaw.gateway.outbound;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Channel selection and resolution logic.
 * Corresponds to TypeScript's channel-selection.ts.
 */
@Slf4j
public final class ChannelSelection {

    private ChannelSelection() {
    }

    /** All known deliverable channels. */
    private static final List<String> KNOWN_CHANNELS = List.of(
            "telegram", "whatsapp", "discord", "slack", "signal", "msteams", "matrix");

    /**
     * Check if a channel name is known/deliverable.
     */
    public static boolean isKnownChannel(String value) {
        return value != null && KNOWN_CHANNELS.contains(value.toLowerCase());
    }

    /**
     * Normalize a message channel name. Returns null for unrecognized channels.
     */
    public static String normalizeChannel(String value) {
        if (value == null || value.isBlank())
            return null;
        String lower = value.trim().toLowerCase();
        if (KNOWN_CHANNELS.contains(lower))
            return lower;
        // Support aliases
        return switch (lower) {
            case "tg" -> "telegram";
            case "wa" -> "whatsapp";
            case "dc" -> "discord";
            case "teams" -> "msteams";
            default -> null;
        };
    }

    /**
     * List configured/enabled channels based on current config.
     * Inspects channels.providers map for configured channel entries.
     */
    public static List<String> listConfiguredChannels(OpenClawConfig cfg) {
        List<String> configured = new ArrayList<>();
        if (cfg == null)
            return configured;

        // Check the channels.providers map for configured channels
        var channels = cfg.getChannels();
        if (channels != null && channels.getProviders() != null) {
            Map<String, Object> providers = channels.getProviders();
            for (String key : providers.keySet()) {
                String normalized = normalizeChannel(key);
                if (normalized != null && !configured.contains(normalized)) {
                    configured.add(normalized);
                }
            }
        }

        return configured;
    }

    /**
     * Resolve the channel to use for outbound delivery.
     * Auto-selects if only one channel is configured.
     *
     * @throws IllegalArgumentException if channel is required but not specified
     */
    public static ChannelResolution resolveChannelSelection(
            OpenClawConfig cfg, String requestedChannel) {

        String normalized = normalizeChannel(requestedChannel);
        if (normalized != null) {
            if (!isKnownChannel(normalized)) {
                throw new IllegalArgumentException("Unknown channel: " + normalized);
            }
            return new ChannelResolution(
                    normalized,
                    listConfiguredChannels(cfg));
        }

        List<String> configured = listConfiguredChannels(cfg);
        if (configured.size() == 1) {
            return new ChannelResolution(configured.get(0), configured);
        }
        if (configured.isEmpty()) {
            throw new IllegalArgumentException("Channel is required (no configured channels detected).");
        }
        throw new IllegalArgumentException(
                "Channel is required when multiple channels are configured: "
                        + String.join(", ", configured));
    }

    /**
     * Result of channel resolution.
     */
    public record ChannelResolution(String channel, List<String> configured) {
    }
}
