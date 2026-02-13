package com.openclaw.channel;

/**
 * Mention gating logic — determines whether to skip messages based on @mention
 * requirements.
 * Corresponds to TypeScript's channels/mention-gating.ts.
 */
public final class MentionGating {

    private MentionGating() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    /** Result of a basic mention gate check. */
    public record MentionGateResult(boolean effectiveWasMentioned, boolean shouldSkip) {
    }

    /** Extended result including bypass information. */
    public record MentionGateWithBypassResult(
            boolean effectiveWasMentioned,
            boolean shouldSkip,
            boolean shouldBypassMention) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Resolve basic mention gating.
     */
    public static MentionGateResult resolve(boolean requireMention,
            boolean canDetectMention,
            boolean wasMentioned,
            boolean implicitMention,
            boolean shouldBypassMention) {
        boolean effectiveWasMentioned = wasMentioned || implicitMention || shouldBypassMention;
        boolean shouldSkip = requireMention && canDetectMention && !effectiveWasMentioned;
        return new MentionGateResult(effectiveWasMentioned, shouldSkip);
    }

    /**
     * Resolve mention gating with automatic bypass detection.
     * Bypass is enabled when: in a group, mention required, not mentioned,
     * no other mentions, text commands allowed, command authorized, and has control
     * command.
     */
    public static MentionGateWithBypassResult resolveWithBypass(
            boolean isGroup,
            boolean requireMention,
            boolean canDetectMention,
            boolean wasMentioned,
            boolean implicitMention,
            boolean hasAnyMention,
            boolean allowTextCommands,
            boolean hasControlCommand,
            boolean commandAuthorized) {

        boolean shouldBypassMention = isGroup
                && requireMention
                && !wasMentioned
                && !hasAnyMention
                && allowTextCommands
                && commandAuthorized
                && hasControlCommand;

        MentionGateResult base = resolve(
                requireMention, canDetectMention, wasMentioned,
                implicitMention, shouldBypassMention);

        return new MentionGateWithBypassResult(
                base.effectiveWasMentioned(),
                base.shouldSkip(),
                shouldBypassMention);
    }
}
