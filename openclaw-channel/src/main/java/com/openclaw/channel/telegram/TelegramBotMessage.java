package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Telegram message processing: builds context and dispatches to agent.
 * Corresponds to TypeScript's telegram/bot-message.ts.
 */
@Slf4j
public class TelegramBotMessage {

    // =========================================================================
    // Message processor dependencies
    // =========================================================================

    @Data
    @Builder
    public static class TelegramMessageProcessorDeps {
        private OpenClawConfig config;
        private String accountId;
        private String token;
        @Builder.Default
        private String replyToMode = "reply";
        @Builder.Default
        private String streamMode = "draft";
        @Builder.Default
        private int textLimit = 4096;
        @Builder.Default
        private int historyLimit = 50;
        @Builder.Default
        private String dmPolicy = "open";
    }

    /**
     * Create a Telegram message processor function.
     * Returns a processor that, for each incoming message, builds context
     * and dispatches to the agent pipeline.
     */
    public static TelegramMessageProcessor createMessageProcessor(
            TelegramMessageProcessorDeps deps) {
        return new TelegramMessageProcessor(deps);
    }

    /**
     * The message processor: wraps context building and dispatch.
     */
    public static class TelegramMessageProcessor {
        private final TelegramMessageProcessorDeps deps;

        public TelegramMessageProcessor(TelegramMessageProcessorDeps deps) {
            this.deps = deps;
        }

        /**
         * Process a Telegram message with optional media attachments.
         */
        public void process(
                Map<String, Object> message,
                List<TelegramBotMessageContext.TelegramMediaRef> allMedia,
                List<String> storeAllowFrom,
                Map<String, Object> options) {

            // Build message context
            TelegramBotMessageContext.TelegramMessageContext context = TelegramBotMessageContext.buildMessageContext(
                    message, allMedia, storeAllowFrom, options,
                    deps.getConfig(), deps.getAccountId());

            if (context == null) {
                log.debug("Message context was null, skipping dispatch");
                return;
            }

            // Dispatch the message
            TelegramBotMessageDispatch.dispatch(
                    context, deps.getConfig(), deps.getAccountId(),
                    deps.getReplyToMode(), deps.getStreamMode(),
                    deps.getTextLimit(), deps.getToken());
        }
    }
}
