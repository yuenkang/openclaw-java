package com.openclaw.channel.telegram;

/**
 * Telegram caption splitting.
 * Corresponds to TypeScript's telegram/caption.ts.
 */
public final class TelegramCaption {

    private TelegramCaption() {
    }

    public static final int MAX_CAPTION_LENGTH = 1024;

    /**
     * Result of splitting a caption: either a short caption or follow-up text.
     */
    public record SplitResult(String caption, String followUpText) {
    }

    /**
     * Split text into a caption (if within limits) or follow-up text.
     * Telegram captions are limited to {@value MAX_CAPTION_LENGTH} characters.
     */
    public static SplitResult split(String text) {
        String trimmed = text != null ? text.trim() : "";
        if (trimmed.isEmpty()) {
            return new SplitResult(null, null);
        }
        if (trimmed.length() > MAX_CAPTION_LENGTH) {
            return new SplitResult(null, trimmed);
        }
        return new SplitResult(trimmed, null);
    }
}
