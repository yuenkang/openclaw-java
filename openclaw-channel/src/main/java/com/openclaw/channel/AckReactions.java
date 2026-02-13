package com.openclaw.channel;

import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;

/**
 * ACK reaction gating logic — determines whether to send ack reactions.
 * Corresponds to TypeScript's channels/ack-reactions.ts.
 */
public final class AckReactions {

    private AckReactions() {
    }

    // =========================================================================
    // Scope constants
    // =========================================================================

    public static final String SCOPE_ALL = "all";
    public static final String SCOPE_DIRECT = "direct";
    public static final String SCOPE_GROUP_ALL = "group-all";
    public static final String SCOPE_GROUP_MENTIONS = "group-mentions";
    public static final String SCOPE_OFF = "off";
    public static final String SCOPE_NONE = "none";

    /** WhatsApp ack reaction mode. */
    public static final String WA_MODE_ALWAYS = "always";
    public static final String WA_MODE_MENTIONS = "mentions";
    public static final String WA_MODE_NEVER = "never";

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Determine whether to send an ack reaction based on scope and context.
     */
    public static boolean shouldAckReaction(String scope,
            boolean isDirect,
            boolean isGroup,
            boolean isMentionableGroup,
            boolean requireMention,
            boolean canDetectMention,
            boolean effectiveWasMentioned,
            boolean shouldBypassMention) {
        String effectiveScope = scope != null ? scope : SCOPE_GROUP_MENTIONS;
        return switch (effectiveScope) {
            case SCOPE_OFF, SCOPE_NONE -> false;
            case SCOPE_ALL -> true;
            case SCOPE_DIRECT -> isDirect;
            case SCOPE_GROUP_ALL -> isGroup;
            case SCOPE_GROUP_MENTIONS -> isMentionableGroup
                    && requireMention
                    && canDetectMention
                    && (effectiveWasMentioned || shouldBypassMention);
            default -> false;
        };
    }

    /**
     * WhatsApp-specific ack reaction check.
     */
    public static boolean shouldAckReactionForWhatsApp(String emoji,
            boolean isDirect,
            boolean isGroup,
            boolean directEnabled,
            String groupMode,
            boolean wasMentioned,
            boolean groupActivated) {
        if (emoji == null || emoji.isEmpty()) {
            return false;
        }
        if (isDirect) {
            return directEnabled;
        }
        if (!isGroup) {
            return false;
        }
        if (WA_MODE_NEVER.equals(groupMode)) {
            return false;
        }
        if (WA_MODE_ALWAYS.equals(groupMode)) {
            return true;
        }
        // "mentions" mode — use standard group-mentions gating
        return shouldAckReaction(SCOPE_GROUP_MENTIONS, false, true, true,
                true, true, wasMentioned, groupActivated);
    }

    /**
     * Remove ack reaction after reply completes.
     */
    public static void removeAckReactionAfterReply(boolean removeAfterReply,
            CompletableFuture<Boolean> ackPromise,
            String ackReactionValue,
            Runnable removeAction,
            Consumer<Throwable> onError) {
        if (!removeAfterReply || ackPromise == null || ackReactionValue == null) {
            return;
        }
        ackPromise.thenAccept(didAck -> {
            if (didAck) {
                try {
                    removeAction.run();
                } catch (Exception err) {
                    if (onError != null) {
                        onError.accept(err);
                    }
                }
            }
        });
    }
}
