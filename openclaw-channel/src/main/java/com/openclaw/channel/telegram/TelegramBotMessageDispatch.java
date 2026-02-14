package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Telegram message dispatch: builds a MsgContext from the Telegram message,
 * calls the auto-reply pipeline (via ReplyPipeline), and delivers the
 * replies back to Telegram.
 * Corresponds to TypeScript's telegram/bot-message-dispatch.ts.
 */
@Slf4j
public class TelegramBotMessageDispatch {

    private static final String EMPTY_RESPONSE_FALLBACK = "No response generated. Please try again.";

    // =========================================================================
    // ReplyPipeline — callback injected from the app module
    // =========================================================================

    /**
     * Functional interface for the auto-reply pipeline.
     * Implemented by the app layer to bridge channel → getReplyFromConfig.
     */
    @FunctionalInterface
    public interface ReplyPipeline {
        /**
         * Run the full auto-reply pipeline for an inbound message.
         *
         * @param msgCtx full MsgContext (Map<String,Object>)
         * @param opts   reply options (nullable)
         * @param config current OpenClawConfig
         * @return CompletableFuture with list of ReplyPayload-shaped maps
         */
        CompletableFuture<List<Map<String, Object>>> getReply(
                Map<String, Object> msgCtx,
                Map<String, Object> opts,
                OpenClawConfig config);
    }

    /**
     * Legacy AgentInvoker — kept for backward compatibility.
     * If only an AgentInvoker is set (no ReplyPipeline), we fall back to it.
     */
    @FunctionalInterface
    public interface AgentInvoker {
        CompletableFuture<String> invoke(String sessionKey, String userText, OpenClawConfig config);
    }

    private static volatile ReplyPipeline replyPipeline;
    private static volatile AgentInvoker agentInvoker;

    /** Set the full reply pipeline. Preferred over setAgentInvoker. */
    public static void setReplyPipeline(ReplyPipeline pipeline) {
        replyPipeline = pipeline;
    }

    /** Legacy: set a simple agent invoker (bypasses getReplyFromConfig). */
    public static void setAgentInvoker(AgentInvoker invoker) {
        agentInvoker = invoker;
    }

    // =========================================================================
    // MsgContext builder — mirrors TS MsgContext (auto-reply/templating.ts)
    // =========================================================================

    /**
     * Build a full MsgContext from a Telegram message context.
     * This maps Telegram-specific fields to the canonical MsgContext fields.
     */
    static Map<String, Object> buildMsgContext(
            TelegramBotMessageContext.TelegramMessageContext ctx,
            String accountId) {

        Map<String, Object> mc = new LinkedHashMap<>();

        // Core body
        String text = ctx.getText() != null ? ctx.getText() : "";
        mc.put("Body", text);
        mc.put("BodyForAgent", text);
        mc.put("RawBody", text);
        mc.put("CommandBody", text);
        mc.put("BodyForCommands", text);

        // Session / routing
        mc.put("SessionKey", ctx.getSessionKey());
        mc.put("AccountId", accountId);
        mc.put("Provider", "telegram");
        mc.put("Surface", "telegram");

        // Sender
        mc.put("SenderId", ctx.getSenderId());
        mc.put("SenderUsername", ctx.getSenderUsername());
        String senderName = buildSenderName(
                ctx.getSenderFirstName(), ctx.getSenderLastName());
        mc.put("SenderName", senderName);
        mc.put("From", senderName);

        // Chat info
        mc.put("To", ctx.getChatId());
        mc.put("ChatType", ctx.getChatType());
        mc.put("MessageSid", ctx.getMessageId());
        mc.put("WasMentioned", ctx.isWasMentioned());

        // Thread / forum
        if (ctx.getMessageThreadId() != null) {
            mc.put("MessageThreadId", ctx.getMessageThreadId());
        }
        mc.put("IsForum", ctx.isForum());

        // Reply context
        if (ctx.getReplyToMessageId() != null) {
            mc.put("ReplyToId", ctx.getReplyToMessageId());
        }

        // Forwarded context
        if (ctx.getForwardedFromName() != null) {
            mc.put("ForwardedFrom", ctx.getForwardedFromName());
        }

        // Media
        if (ctx.getMedia() != null && !ctx.getMedia().isEmpty()) {
            var first = ctx.getMedia().get(0);
            mc.put("MediaType", first.getContentType());
            mc.put("MediaPath", first.getFileId());
            if (ctx.getMedia().size() > 1) {
                mc.put("MediaPaths", ctx.getMedia().stream()
                        .map(TelegramBotMessageContext.TelegramMediaRef::getFileId)
                        .toList());
                mc.put("MediaTypes", ctx.getMedia().stream()
                        .map(TelegramBotMessageContext.TelegramMediaRef::getContentType)
                        .toList());
            }
        }

        return mc;
    }

    private static String buildSenderName(String firstName, String lastName) {
        if (firstName == null && lastName == null)
            return "User";
        if (lastName == null)
            return firstName;
        if (firstName == null)
            return lastName;
        return firstName + " " + lastName;
    }

    // =========================================================================
    // Dispatch — main entry point
    // =========================================================================

    /**
     * Dispatch a Telegram message context to the auto-reply pipeline.
     */
    public static void dispatch(
            TelegramBotMessageContext.TelegramMessageContext context,
            OpenClawConfig config,
            String accountId,
            String replyToMode,
            String streamMode,
            int textLimit,
            String token) {

        if (context == null || (context.getText() == null || context.getText().isBlank())
                && (context.getMedia() == null || context.getMedia().isEmpty())) {
            log.debug("Empty message with no media, skipping dispatch");
            return;
        }

        String chatId = context.getChatId();
        String sessionKey = context.getSessionKey();
        String messageId = context.getMessageId();
        Integer messageThreadId = context.getMessageThreadId();

        log.info("Dispatching message: chatId={} sessionKey={} textLen={}",
                chatId, sessionKey,
                context.getText() != null ? context.getText().length() : 0);

        // Send typing action while processing
        sendTyping(token, chatId);

        // Prefer full ReplyPipeline over legacy AgentInvoker
        if (replyPipeline != null) {
            dispatchViaReplyPipeline(
                    context, config, accountId, token,
                    chatId, messageId, replyToMode, messageThreadId, textLimit);
        } else if (agentInvoker != null) {
            dispatchViaLegacyInvoker(
                    context, config, token,
                    chatId, sessionKey, messageId, replyToMode, messageThreadId, textLimit);
        } else {
            log.warn("No reply pipeline or agent invoker configured");
            deliverReply(token, chatId,
                    "⚠️ Agent not configured. Please check server setup.",
                    messageId, replyToMode, messageThreadId, textLimit);
        }
    }

    // =========================================================================
    // Full pipeline dispatch
    // =========================================================================

    private static void dispatchViaReplyPipeline(
            TelegramBotMessageContext.TelegramMessageContext context,
            OpenClawConfig config,
            String accountId,
            String token,
            String chatId,
            String messageId,
            String replyToMode,
            Integer messageThreadId,
            int textLimit) {

        Map<String, Object> msgCtx = buildMsgContext(context, accountId);

        // Reply options
        Map<String, Object> opts = new LinkedHashMap<>();
        opts.put("textLimit", textLimit);
        opts.put("streamMode", null); // streaming handled at delivery layer

        replyPipeline.getReply(msgCtx, opts, config)
                .thenAccept(payloads -> {
                    if (payloads == null || payloads.isEmpty()) {
                        deliverReply(token, chatId, EMPTY_RESPONSE_FALLBACK,
                                messageId, replyToMode, messageThreadId, textLimit);
                        return;
                    }
                    // Deliver each reply payload
                    for (Map<String, Object> payload : payloads) {
                        String text = payload.get("text") instanceof String s ? s : null;
                        if (text != null && !text.isBlank()) {
                            deliverReply(token, chatId, text,
                                    messageId, replyToMode, messageThreadId, textLimit);
                        }
                    }
                })
                .exceptionally(ex -> {
                    log.error("Reply pipeline failed for session {}: {}",
                            msgCtx.get("SessionKey"), ex.getMessage(), ex);
                    deliverReply(token, chatId,
                            "❌ Error: " + ex.getMessage(),
                            messageId, replyToMode, messageThreadId, textLimit);
                    return null;
                });
    }

    // =========================================================================
    // Legacy fallback (AgentInvoker — bypasses getReplyFromConfig)
    // =========================================================================

    private static void dispatchViaLegacyInvoker(
            TelegramBotMessageContext.TelegramMessageContext context,
            OpenClawConfig config,
            String token,
            String chatId,
            String sessionKey,
            String messageId,
            String replyToMode,
            Integer messageThreadId,
            int textLimit) {

        String userText = context.getText() != null ? context.getText() : "";

        agentInvoker.invoke(sessionKey, userText, config)
                .thenAccept(reply -> {
                    deliverReply(token, chatId, reply, messageId,
                            replyToMode, messageThreadId, textLimit);
                })
                .exceptionally(ex -> {
                    log.error("Agent invocation failed for session {}: {}",
                            sessionKey, ex.getMessage(), ex);
                    deliverReply(token, chatId,
                            "❌ Error: " + ex.getMessage(),
                            messageId, replyToMode, messageThreadId, textLimit);
                    return null;
                });
    }

    // =========================================================================
    // Typing + delivery
    // =========================================================================

    private static void sendTyping(String token, String chatId) {
        try {
            TelegramFetch.callApi(token, "sendChatAction",
                    "{\"chat_id\":\"" + chatId + "\",\"action\":\"typing\"}");
        } catch (Exception e) {
            log.debug("Failed to send typing action: {}", e.getMessage());
        }
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

        TelegramSend.sendMessage(token, chatId, text, replyToMessageId,
                messageThreadId, replyToMode);
    }
}
