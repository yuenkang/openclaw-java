package com.openclaw.channel.telegram;

/**
 * Telegram voice message compatibility checks.
 * Corresponds to TypeScript's telegram/voice.ts.
 */
public final class TelegramVoice {

    private TelegramVoice() {
    }

    /**
     * Check if media is compatible with Telegram voice message format.
     * Voice notes must be audio (OGG/opus preferred).
     */
    public static boolean isVoiceCompatible(String contentType, String fileName) {
        if (contentType == null || contentType.isBlank())
            return false;
        String lower = contentType.toLowerCase();
        return lower.startsWith("audio/ogg") || lower.equals("audio/opus");
    }

    public record VoiceDecision(boolean useVoice, String reason) {
    }

    /**
     * Decide whether to send media as a voice note.
     */
    public static VoiceDecision resolveVoiceSend(boolean wantsVoice, String contentType,
            String fileName) {
        if (!wantsVoice)
            return new VoiceDecision(false, null);
        if (isVoiceCompatible(contentType, fileName))
            return new VoiceDecision(true, null);
        String ct = contentType != null ? contentType : "unknown";
        String fn = fileName != null ? fileName : "unknown";
        return new VoiceDecision(false, "media is " + ct + " (" + fn + ")");
    }
}
