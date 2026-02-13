package com.openclaw.channel;

import java.util.function.BiFunction;

/**
 * Channel media upload size limits.
 * Corresponds to TypeScript's channels/plugins/media-limits.ts.
 */
public final class ChannelMediaLimits {

    private ChannelMediaLimits() {
    }

    private static final int MB = 1024 * 1024;

    /**
     * Resolve the maximum media size in bytes for a channel.
     * Checks channel-specific limit first, then falls back to global agent config.
     *
     * @param channelLimitMbResolver function(accountId) → channel-specific limit in
     *                               MB, or null
     * @param accountId              account ID
     * @param globalMediaMaxMb       global agents.defaults.mediaMaxMb from config,
     *                               or null
     * @return max bytes, or null if no limit configured
     */
    public static Long resolveMaxBytes(BiFunction<String, String, Double> channelLimitMbResolver,
            String channelId,
            String accountId,
            Double globalMediaMaxMb) {
        String effectiveAccountId = (accountId != null && !accountId.isBlank())
                ? accountId
                : "default";
        Double channelLimit = channelLimitMbResolver.apply(channelId, effectiveAccountId);
        if (channelLimit != null && channelLimit > 0) {
            return (long) (channelLimit * MB);
        }
        if (globalMediaMaxMb != null && globalMediaMaxMb > 0) {
            return (long) (globalMediaMaxMb * MB);
        }
        return null;
    }
}
