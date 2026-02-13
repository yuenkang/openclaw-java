package com.openclaw.channel;

/**
 * Channel plugin helpers.
 * Corresponds to TypeScript's channels/plugins/helpers.ts.
 */
public final class ChannelPluginHelpers {

    private ChannelPluginHelpers() {
    }

    public static final String DEFAULT_ACCOUNT_ID = "default";

    /**
     * Resolve the default account ID for a channel plugin.
     * Falls back to the first listed account or the DEFAULT_ACCOUNT_ID.
     *
     * @param plugin     the channel plugin definition
     * @param accountIds available account IDs (may be null to let plugin resolve)
     * @return the resolved default account ID
     */
    public static String resolveDefaultAccountId(ChannelPluginDef plugin,
            java.util.List<String> accountIds) {
        if (accountIds != null && !accountIds.isEmpty()) {
            return accountIds.get(0);
        }
        return DEFAULT_ACCOUNT_ID;
    }

    /**
     * Format a pairing approval hint in CLI command style.
     */
    public static String formatPairingApproveHint(String channelId) {
        return "Approve via: `openclaw pairing list " + channelId
                + "` / `openclaw pairing approve " + channelId + " <code>`";
    }
}
