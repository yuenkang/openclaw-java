package com.openclaw.agent.autoreply.reply;

import com.openclaw.agent.autoreply.ReplyTokens;

/**
 * Typing mode resolution and signaler factory.
 * Mirrors {@code auto-reply/reply/typing-mode.ts}.
 */
public final class TypingMode {

    private TypingMode() {
    }

    /** Default typing mode for group chats. */
    public static final String DEFAULT_GROUP_TYPING_MODE = "message";

    /**
     * Resolve the effective typing mode.
     *
     * @param configured   configured mode (instant|message|thinking|never|null)
     * @param isGroupChat  whether this is a group chat
     * @param wasMentioned whether the bot was mentioned
     * @param isHeartbeat  whether this is a heartbeat message
     * @return resolved mode: "instant", "message", "thinking", or "never"
     */
    public static String resolveTypingMode(String configured, boolean isGroupChat, boolean wasMentioned,
            boolean isHeartbeat) {
        if (isHeartbeat)
            return "never";
        if (configured != null && !configured.isEmpty())
            return configured;
        if (!isGroupChat || wasMentioned)
            return "instant";
        return DEFAULT_GROUP_TYPING_MODE;
    }

    /** Typing signaler interface. */
    public interface TypingSignaler {
        String mode();

        boolean shouldStartImmediately();

        boolean shouldStartOnMessageStart();

        boolean shouldStartOnText();

        boolean shouldStartOnReasoning();

        void signalRunStart();

        void signalMessageStart();

        void signalTextDelta(String text);

        void signalReasoningDelta();

        void signalToolStart();
    }

    /**
     * Create a typing signaler that delegates to a TypingController.
     */
    public static TypingSignaler createTypingSignaler(Typing.TypingController typing, String mode,
            boolean isHeartbeat) {
        boolean immediate = "instant".equals(mode);
        boolean onMessageStart = "message".equals(mode);
        boolean onText = "message".equals(mode) || "instant".equals(mode);
        boolean onReasoning = "thinking".equals(mode);
        boolean disabled = isHeartbeat || "never".equals(mode);

        return new TypingSignaler() {
            private boolean hasRenderableText = false;

            private boolean isRenderable(String text) {
                String trimmed = text != null ? text.trim() : "";
                if (trimmed.isEmpty())
                    return false;
                return !ReplyTokens.isSilentReplyText(trimmed, ReplyTokens.SILENT_REPLY_TOKEN);
            }

            @Override
            public String mode() {
                return mode;
            }

            @Override
            public boolean shouldStartImmediately() {
                return immediate;
            }

            @Override
            public boolean shouldStartOnMessageStart() {
                return onMessageStart;
            }

            @Override
            public boolean shouldStartOnText() {
                return onText;
            }

            @Override
            public boolean shouldStartOnReasoning() {
                return onReasoning;
            }

            @Override
            public void signalRunStart() {
                if (disabled || !immediate)
                    return;
                typing.startTypingLoop();
            }

            @Override
            public void signalMessageStart() {
                if (disabled || !onMessageStart || !hasRenderableText)
                    return;
                typing.startTypingLoop();
            }

            @Override
            public void signalTextDelta(String text) {
                if (disabled)
                    return;
                if (isRenderable(text))
                    hasRenderableText = true;
                else if (text != null && !text.trim().isEmpty())
                    return;
                if (onText) {
                    typing.startTypingOnText(text);
                    return;
                }
                if (onReasoning) {
                    if (!typing.isActive())
                        typing.startTypingLoop();
                    typing.refreshTypingTtl();
                }
            }

            @Override
            public void signalReasoningDelta() {
                if (disabled || !onReasoning || !hasRenderableText)
                    return;
                typing.startTypingLoop();
                typing.refreshTypingTtl();
            }

            @Override
            public void signalToolStart() {
                if (disabled)
                    return;
                if (!typing.isActive()) {
                    typing.startTypingLoop();
                    typing.refreshTypingTtl();
                    return;
                }
                typing.refreshTypingTtl();
            }
        };
    }
}
