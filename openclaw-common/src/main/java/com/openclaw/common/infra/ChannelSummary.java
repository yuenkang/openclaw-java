package com.openclaw.common.infra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Builds a summary of channel statuses (Telegram, WeChat, etc.) with account
 * details, enabled state, and configuration issues.
 * <p>
 * Corresponds to TypeScript's infra/channel-summary.ts.
 */
public final class ChannelSummary {

    private ChannelSummary() {
    }

    private static final Logger log = LoggerFactory.getLogger(ChannelSummary.class);

    // =========================================================================
    // Data model
    // =========================================================================

    /**
     * Summary for a single channel account.
     */
    public record AccountSummary(
            String label,
            boolean enabled,
            boolean configured,
            String status,
            List<String> allowFrom,
            Map<String, String> extra) {
    }

    /**
     * Summary for a channel type (e.g. "telegram").
     */
    public record ChannelInfo(
            String channelId,
            String displayName,
            boolean enabled,
            List<AccountSummary> accounts,
            List<String> warnings) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Build a text summary of all channel statuses suitable for CLI / log output.
     *
     * @param channels the resolved channel info list
     * @return formatted text lines
     */
    public static List<String> buildSummaryLines(List<ChannelInfo> channels) {
        List<String> lines = new ArrayList<>();
        if (channels == null || channels.isEmpty()) {
            lines.add("No channels configured.");
            return lines;
        }

        for (ChannelInfo channel : channels) {
            String header = String.format("  %s: %s",
                    channel.displayName(),
                    channel.enabled() ? "enabled" : "disabled");
            lines.add(header);

            for (AccountSummary account : channel.accounts()) {
                String accountLine = String.format("    • %s — %s%s",
                        account.label(),
                        account.configured() ? "configured" : "not configured",
                        account.enabled() ? "" : " (disabled)");
                lines.add(accountLine);

                if (account.status() != null && !account.status().isBlank()) {
                    lines.add("      status: " + account.status());
                }
                if (account.allowFrom() != null && !account.allowFrom().isEmpty()) {
                    lines.add("      allow-from: " + String.join(", ", account.allowFrom()));
                }
            }

            if (channel.warnings() != null) {
                for (String warning : channel.warnings()) {
                    lines.add("    ⚠ " + warning);
                }
            }
        }
        return lines;
    }

    /**
     * Build a text summary as a single multi-line string.
     */
    public static String buildSummary(List<ChannelInfo> channels) {
        return String.join("\n", buildSummaryLines(channels));
    }

    /**
     * Format an age duration in human-readable form (e.g. "2h 15m", "3d 4h").
     *
     * @param ageMs age in milliseconds
     * @return formatted age string
     */
    public static String formatAge(long ageMs) {
        if (ageMs < 0) {
            return "?";
        }
        long seconds = ageMs / 1000;
        long minutes = seconds / 60;
        long hours = minutes / 60;
        long days = hours / 24;

        if (days > 0) {
            long remainingHours = hours % 24;
            return remainingHours > 0
                    ? String.format("%dd %dh", days, remainingHours)
                    : String.format("%dd", days);
        }
        if (hours > 0) {
            long remainingMinutes = minutes % 60;
            return remainingMinutes > 0
                    ? String.format("%dh %dm", hours, remainingMinutes)
                    : String.format("%dh", hours);
        }
        if (minutes > 0) {
            return String.format("%dm", minutes);
        }
        return String.format("%ds", Math.max(seconds, 1));
    }
}
