package com.openclaw.channel.telegram;

/**
 * Telegram bot context types.
 * Corresponds to TypeScript's telegram/bot/types.ts.
 */
public final class TelegramBotTypes {

    private TelegramBotTypes() {
    }

    /** Stream mode for Telegram draft streaming. */
    public enum StreamMode {
        OFF, PARTIAL, BLOCK
    }

    /** Sticker metadata for context enrichment and caching. */
    public record StickerMetadata(
            String emoji,
            String setName,
            String fileId,
            String fileUniqueId,
            String cachedDescription) {
    }
}
