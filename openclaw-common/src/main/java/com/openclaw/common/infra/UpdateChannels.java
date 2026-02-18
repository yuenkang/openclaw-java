package com.openclaw.common.infra;

/**
 * Update channel definitions (stable / beta / dev) and channel resolution
 * logic.
 * <p>
 * Corresponds to TypeScript's infra/update-channels.ts.
 */
public final class UpdateChannels {

    private UpdateChannels() {
    }

    public enum Channel {
        STABLE, BETA, DEV;

        public String label() {
            return name().toLowerCase();
        }
    }

    public enum ChannelSource {
        CONFIG, GIT_TAG, GIT_BRANCH, DEFAULT;

        public String label() {
            return name().toLowerCase().replace('_', '-');
        }
    }

    public static final Channel DEFAULT_PACKAGE_CHANNEL = Channel.STABLE;
    public static final Channel DEFAULT_GIT_CHANNEL = Channel.DEV;
    public static final String DEV_BRANCH = "main";

    /**
     * Normalize a string to a valid {@link Channel}, or null if invalid.
     */
    public static Channel normalize(String value) {
        if (value == null || value.isBlank()) {
            return null;
        }
        String lower = value.trim().toLowerCase();
        return switch (lower) {
            case "stable" -> Channel.STABLE;
            case "beta" -> Channel.BETA;
            case "dev" -> Channel.DEV;
            default -> null;
        };
    }

    /**
     * Map a channel to the corresponding npm dist-tag (or Maven qualifier).
     */
    public static String channelToTag(Channel channel) {
        return switch (channel) {
            case BETA -> "beta";
            case DEV -> "dev";
            case STABLE -> "latest";
        };
    }

    /**
     * Check if a git tag represents a beta release (contains "-beta").
     */
    public static boolean isBetaTag(String tag) {
        return tag != null && tag.toLowerCase().contains("-beta");
    }

    /**
     * Check if a git tag represents a stable release.
     */
    public static boolean isStableTag(String tag) {
        return !isBetaTag(tag);
    }

    /**
     * Resolved channel together with its source.
     */
    public record ResolvedChannel(Channel channel, ChannelSource source) {
    }

    /**
     * Resolve the effective update channel based on config, install kind, and git
     * info.
     *
     * @param configChannel user-configured channel override (may be null)
     * @param installKind   "git", "package", or "unknown"
     * @param gitTag        current git tag (may be null)
     * @param gitBranch     current git branch (may be null)
     */
    public static ResolvedChannel resolveEffective(
            Channel configChannel,
            String installKind,
            String gitTag,
            String gitBranch) {
        if (configChannel != null) {
            return new ResolvedChannel(configChannel, ChannelSource.CONFIG);
        }

        if ("git".equals(installKind)) {
            if (gitTag != null && !gitTag.isBlank()) {
                Channel ch = isBetaTag(gitTag) ? Channel.BETA : Channel.STABLE;
                return new ResolvedChannel(ch, ChannelSource.GIT_TAG);
            }
            if (gitBranch != null && !gitBranch.isBlank() && !"HEAD".equals(gitBranch)) {
                return new ResolvedChannel(Channel.DEV, ChannelSource.GIT_BRANCH);
            }
            return new ResolvedChannel(DEFAULT_GIT_CHANNEL, ChannelSource.DEFAULT);
        }

        return new ResolvedChannel(DEFAULT_PACKAGE_CHANNEL, ChannelSource.DEFAULT);
    }

    /**
     * Format a human-readable label for a resolved channel.
     */
    public static String formatLabel(Channel channel, ChannelSource source,
            String gitTag, String gitBranch) {
        return switch (source) {
            case CONFIG -> channel.label() + " (config)";
            case GIT_TAG -> gitTag != null
                    ? channel.label() + " (" + gitTag + ")"
                    : channel.label() + " (tag)";
            case GIT_BRANCH -> gitBranch != null
                    ? channel.label() + " (" + gitBranch + ")"
                    : channel.label() + " (branch)";
            case DEFAULT -> channel.label() + " (default)";
        };
    }
}
