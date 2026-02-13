package com.openclaw.channel;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Channel pairing utilities — list/resolve/validate pairing adapters.
 * Corresponds to TypeScript's channels/plugins/pairing.ts.
 */
public final class ChannelPairing {

    private ChannelPairing() {
    }

    /**
     * List channel IDs that have pairing support.
     */
    public static List<String> listPairingChannels(List<ChannelPluginDef> allPlugins) {
        return allPlugins.stream()
                .filter(p -> p.getMeta() != null) // pairing would be a non-null adapter
                .map(ChannelPluginDef::getId)
                .collect(Collectors.toList());
    }

    /**
     * Resolve and validate a raw channel ID for pairing,
     * throwing if it's not a valid pairing channel.
     */
    public static String resolvePairingChannel(String raw, List<String> pairingChannels) {
        if (raw == null) {
            throw new IllegalArgumentException(
                    "Invalid channel: (empty) (expected one of: " + String.join(", ", pairingChannels) + ")");
        }
        String value = raw.trim().toLowerCase();
        if (value.isEmpty() || !pairingChannels.contains(value)) {
            throw new IllegalArgumentException(
                    "Invalid channel: " + (value.isEmpty() ? "(empty)" : value)
                            + " (expected one of: " + String.join(", ", pairingChannels) + ")");
        }
        return value;
    }

    /** Pairing message constant. */
    public static final String PAIRING_MESSAGE_TEMPLATE = "A new device is requesting to pair with your %s account. "
            + "To approve, run: `openclaw pairing approve %s %s`";

    /**
     * Format the default pairing message.
     */
    public static String formatPairingMessage(String channelLabel, String channelId, String code) {
        return String.format(PAIRING_MESSAGE_TEMPLATE, channelLabel, channelId, code);
    }
}
