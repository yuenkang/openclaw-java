package com.openclaw.autoreply.reply;

/**
 * Reply reference (threading) planner — controls when to attach reply/thread
 * IDs.
 * Mirrors {@code auto-reply/reply/reply-reference.ts}.
 */
public final class ReplyReference {

    private ReplyReference() {
    }

    /** Planner that decides whether to attach a reply reference to each message. */
    public interface ReplyReferencePlanner {
        /**
         * Returns the effective reply/thread id for the next send and updates state.
         */
        String use();

        /** Mark that a reply was sent. */
        void markSent();

        /** Whether a reply has been sent in this flow. */
        boolean hasReplied();
    }

    /**
     * Create a planner for reply reference IDs.
     *
     * @param replyToMode    "off", "first", or "all"
     * @param existingId     existing thread/reference id (always used when present)
     * @param startId        id to start a new thread/reference when allowed
     * @param allowReference whether to allow references at all (default true)
     * @param hasRepliedInit seed the planner with prior reply state
     */
    public static ReplyReferencePlanner createReplyReferencePlanner(
            String replyToMode,
            String existingId,
            String startId,
            Boolean allowReference,
            Boolean hasRepliedInit) {

        return new ReplyReferencePlanner() {
            private boolean replied = hasRepliedInit != null ? hasRepliedInit : false;
            private final boolean allowed = allowReference == null || allowReference;
            private final String existing = existingId != null ? existingId.trim() : null;
            private final String start = startId != null ? startId.trim() : null;
            private final String mode = replyToMode != null ? replyToMode : "first";

            @Override
            public String use() {
                if (!allowed)
                    return null;
                if (existing != null && !existing.isEmpty()) {
                    replied = true;
                    return existing;
                }
                if (start == null || start.isEmpty())
                    return null;
                if ("off".equals(mode))
                    return null;
                if ("all".equals(mode)) {
                    replied = true;
                    return start;
                }
                // "first" mode
                if (!replied) {
                    replied = true;
                    return start;
                }
                return null;
            }

            @Override
            public void markSent() {
                replied = true;
            }

            @Override
            public boolean hasReplied() {
                return replied;
            }
        };
    }
}
