package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

/**
 * Telegram message dispatch: routes completed message context to agent
 * pipeline.
 * Corresponds to TypeScript's telegram/bot-message-dispatch.ts.
 */
@Slf4j
public class TelegramBotMessageDispatch {

    private static final String EMPTY_RESPONSE_FALLBACK = "No response generated. Please try again.";

    /**
     * Dispatch a Telegram message context to the agent pipeline.
     */
    public static void dispatch(
            TelegramBotMessageContext.TelegramMessageContext context,
            OpenClawConfig config,
            String accountId,
            String replyToMode,
            String streamMode,
            int textLimit,
            String token) {

        if (context == null || context.getText() == null || context.getText().isBlank()) {
            // Check if there is media to process even without text
            if (context == null || context.getMedia().isEmpty()) {
                log.debug("Empty message with no media, skipping dispatch");
                return;
            }
        }

        log.debug("Dispatching message: chatId={} sessionKey={} textLen={}",
                context.getChatId(),
                context.getSessionKey(),
                context.getText() != null ? context.getText().length() : 0);

        // Resolve sticker vision if applicable
        if (!context.getMedia().isEmpty()) {
            for (var media : context.getMedia()) {
                if (media.getStickerMetadata() != null) {
                    log.debug("Message includes sticker: emoji={}",
                            media.getStickerMetadata().getEmoji());
                }
            }
        }

        // Build dispatch parameters
        String chatId = context.getChatId();
        String sessionKey = context.getSessionKey();

        // Resolve typing indicator
        if ("draft".equals(streamMode)) {
            log.debug("Stream mode is 'draft', will use draft streaming for session: {}",
                    sessionKey);
        }

        // Queue message for agent processing
        // The actual agent invocation happens via the gateway session pipeline,
        // which picks up queued inbound messages.
        log.info("Message queued for dispatch: session={} chat={} account={}",
                sessionKey, chatId, accountId);
    }

    /**
     * Deliver a reply back to Telegram.
     */
    public static void deliverReply(
            String token,
            String chatId,
            String text,
            String replyToMessageId,
            String replyToMode,
            Integer messageThreadId,
            int textLimit) {

        if (text == null || text.isBlank()) {
            text = EMPTY_RESPONSE_FALLBACK;
        }

        // Truncate if needed
        if (text.length() > textLimit) {
            text = text.substring(0, textLimit - 3) + "...";
        }

        log.debug("Delivering reply: chatId={} len={} replyTo={}",
                chatId, text.length(), replyToMessageId);

        // Actual Telegram API call delegated to TelegramSend
        TelegramSend.sendMessage(token, chatId, text, replyToMessageId,
                messageThreadId, replyToMode);
    }
}
