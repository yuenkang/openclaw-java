package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
    private static volatile CommandHandler commandHandler;
    private static volatile CallbackQueryHandler callbackQueryHandler;

    /**
     * Result of a command handler — text + optional inline keyboard buttons.
     */
    public record CommandHandlerResult(
            String text,
            List<List<TelegramSend.InlineButton>> buttons) {

        public boolean hasButtons() {
            return buttons != null && !buttons.isEmpty();
        }
    }

    /**
     * Command handler — intercepts slash commands before the reply pipeline.
     * Returns a CommandHandlerResult if the command was handled, null otherwise.
     */
    @FunctionalInterface
    public interface CommandHandler {
        CommandHandlerResult handle(String command, String sessionKey, String senderId, OpenClawConfig config);
    }

    /**
     * Callback query handler — handles inline button presses (e.g. pagination).
     */
    @FunctionalInterface
    public interface CallbackQueryHandler {
        CommandHandlerResult handle(String callbackData, OpenClawConfig config);
    }

    /** Set the full reply pipeline. Preferred over setAgentInvoker. */
    public static void setReplyPipeline(ReplyPipeline pipeline) {
        replyPipeline = pipeline;
    }

    /** Legacy: set a simple agent invoker (bypasses getReplyFromConfig). */
    public static void setAgentInvoker(AgentInvoker invoker) {
        agentInvoker = invoker;
    }

    /** Set the command handler for slash commands (e.g. /clear, /usage). */
    public static void setCommandHandler(CommandHandler handler) {
        commandHandler = handler;
    }

    /** Set the callback query handler for inline button presses. */
    public static void setCallbackQueryHandler(CallbackQueryHandler handler) {
        callbackQueryHandler = handler;
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

        // Intercept Telegram-specific /start command
        String text = context.getText() != null ? context.getText().trim() : "";
        if (text.equals("/start") || text.startsWith("/start ")) {
            String welcomeText = """
                    🤖 *欢迎使用 OpenClaw！*

                    我是你的 AI 助手，可以直接对话，也可以使用斜杠命令。

                    *快速开始*
                    • 直接发送消息与 AI 对话
                    • /help — 查看帮助信息
                    • /commands — 列出所有可用命令
                    • /status — 查看当前状态
                    • /model — 查看或切换模型

                    发送任意消息，开始对话吧！ 💬""";
            deliverReply(token, chatId, welcomeText,
                    messageId, replyToMode, messageThreadId, textLimit);
            return;
        }

        // DM access check (matches TS buildTelegramMessageContext L224-303)
        // For DMs: check allowFrom + ownerAllowFrom. If not allowed, block.
        // /start is exempt (handled above).
        // Group messages bypass this check (group policy logic is separate).
        if (!context.isGroup()) {
            var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, accountId);
            List<String> allowFrom = tgConfig != null
                    ? TelegramBotHelpers.resolveAllowFrom(tgConfig)
                    : List.of();
            List<String> ownerAllowFrom = List.of();
            if (config.getCommands() != null && config.getCommands().getOwnerAllowFrom() != null) {
                ownerAllowFrom = config.getCommands().getOwnerAllowFrom().stream()
                        .map(String::valueOf).toList();
            }
            var normalized = TelegramBotAccess.normalizeAllowFromWithStore(allowFrom, ownerAllowFrom);
            boolean allowed = TelegramBotAccess.isSenderAllowed(
                    normalized, context.getSenderId(), context.getSenderUsername());
            if (!allowed) {
                log.info("DM from non-allowlisted sender {} — blocked", context.getSenderId());
                return;
            }
        }

        // Intercept slash commands before the reply pipeline
        if (commandHandler != null && text.startsWith("/")) {
            CommandHandlerResult cmdResult = null;
            try {
                cmdResult = commandHandler.handle(text, sessionKey, context.getSenderId(), config);
            } catch (Exception e) {
                log.error("Command handler failed: {}", e.getMessage(), e);
            }
            if (cmdResult != null) {
                if (cmdResult.hasButtons()) {
                    // Send with inline keyboard
                    sendMessageWithButtons(token, chatId, cmdResult.text(),
                            cmdResult.buttons(), messageId, messageThreadId, replyToMode);
                } else {
                    deliverReply(token, chatId, cmdResult.text(),
                            messageId, replyToMode, messageThreadId, textLimit);
                }
                return;
            }
        }

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
        // Include bot token for media downloads (e.g. image messages)
        msgCtx.put("BotToken", token);

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
    // Callback query handling (inline button presses)
    // =========================================================================

    /**
     * Handle a Telegram callback query (inline button press).
     * Returns true if the callback was handled.
     */
    public static boolean handleCallbackQuery(
            String token, String chatId, Integer messageId,
            String callbackData, OpenClawConfig config) {

        if (callbackQueryHandler == null || callbackData == null) {
            return false;
        }

        try {
            CommandHandlerResult result = callbackQueryHandler.handle(callbackData, config);
            if (result == null) {
                return false;
            }

            // Convert markdown to Telegram HTML
            String html = com.openclaw.channel.telegram.TelegramFormat
                    .markdownToTelegramHtml(result.text(), null);

            // Serialize inline keyboard if present
            String replyMarkup = result.hasButtons()
                    ? serializeInlineKeyboard(result.buttons())
                    : null;

            TelegramSend.editMessage(token, chatId, messageId, html, replyMarkup);
            return true;
        } catch (Exception e) {
            log.error("Callback query handling failed: {}", e.getMessage(), e);
            return false;
        }
    }

    // =========================================================================
    // Send with inline keyboard
    // =========================================================================

    /**
     * Send a message with inline keyboard buttons.
     */
    private static void sendMessageWithButtons(
            String token, String chatId, String text,
            List<List<TelegramSend.InlineButton>> buttons,
            String replyToMessageId, Integer messageThreadId,
            String replyToMode) {

        // Convert markdown to Telegram HTML
        String html = TelegramFormat.markdownToTelegramHtml(text, null);

        String replyMarkup = serializeInlineKeyboard(buttons);
        TelegramSend.sendMessage(token, chatId, html, replyToMessageId,
                messageThreadId, replyToMode, replyMarkup);
    }

    /**
     * Serialize inline keyboard buttons to JSON for Telegram API reply_markup.
     */
    static String serializeInlineKeyboard(List<List<TelegramSend.InlineButton>> buttons) {
        if (buttons == null || buttons.isEmpty()) {
            return null;
        }
        StringBuilder sb = new StringBuilder();
        sb.append("{\"inline_keyboard\":[");
        for (int r = 0; r < buttons.size(); r++) {
            if (r > 0)
                sb.append(",");
            sb.append("[");
            List<TelegramSend.InlineButton> row = buttons.get(r);
            for (int c = 0; c < row.size(); c++) {
                if (c > 0)
                    sb.append(",");
                TelegramSend.InlineButton btn = row.get(c);
                sb.append("{\"text\":\"").append(escapeJson(btn.text()))
                        .append("\",\"callback_data\":\"")
                        .append(escapeJson(btn.callbackData()))
                        .append("\"}");
            }
            sb.append("]");
        }
        sb.append("]}");
        return sb.toString();
    }

    private static String escapeJson(String value) {
        if (value == null)
            return "";
        return value.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r");
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

    // Regex to match markdown images with base64 data URIs:
    // ![caption](data:image/png;base64,...)
    private static final Pattern BASE64_IMAGE_PATTERN = Pattern.compile(
            "!\\[([^\\]]*)\\]\\(data:image/([a-zA-Z]+);base64,([A-Za-z0-9+/=\\s]+)\\)");

    // Regex to match markdown images with local file paths:
    // ![caption](/path/to/image.png)
    private static final Pattern FILE_IMAGE_PATTERN = Pattern.compile(
            "!\\[([^\\]]*)\\]\\((/[^)]+\\.(png|jpg|jpeg|gif|webp))\\)");

    /**
     * Deliver a reply back to Telegram.
     * Automatically detects embedded images (base64 data URIs or local file paths)
     * and sends them as Telegram photos.
     * Text delivery is delegated to {@link TelegramBotDelivery} for markdown→HTML
     * conversion and IR-level chunking.
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

        // Extract and send embedded images
        String remaining = extractAndSendImages(token, chatId, text,
                replyToMessageId, messageThreadId);

        // Delegate text delivery to TelegramBotDelivery (markdown → HTML + chunking)
        remaining = remaining.trim();
        if (!remaining.isBlank()) {
            log.debug("Delivering reply: chatId={} len={} replyTo={}",
                    chatId, remaining.length(), replyToMessageId);

            TelegramBotDelivery.deliverReply(token, chatId, remaining,
                    replyToMessageId, messageThreadId, replyToMode, null);
        }
    }

    /**
     * Extract embedded images from text, send them as Telegram photos,
     * and return the text with images removed.
     */
    private static String extractAndSendImages(
            String token, String chatId, String text,
            String replyToMessageId, Integer messageThreadId) {

        String remaining = text;
        int imageCount = 0;

        // 1. Handle base64 data URI images
        Matcher base64Matcher = BASE64_IMAGE_PATTERN.matcher(remaining);
        while (base64Matcher.find()) {
            String caption = base64Matcher.group(1);
            String format = base64Matcher.group(2);
            String base64Data = base64Matcher.group(3).replaceAll("\\s+", "");

            try {
                byte[] imageBytes = Base64.getDecoder().decode(base64Data);
                String fileName = "image_" + (++imageCount) + "." + format;

                log.info("Sending embedded base64 image: {} ({} bytes)", fileName, imageBytes.length);
                TelegramSend.sendPhoto(token, chatId, imageBytes, fileName,
                        caption.isBlank() ? null : caption, replyToMessageId, messageThreadId);
            } catch (Exception e) {
                log.warn("Failed to decode/send base64 image: {}", e.getMessage());
            }
        }
        remaining = BASE64_IMAGE_PATTERN.matcher(remaining).replaceAll("");

        // 2. Handle local file path images
        Matcher fileMatcher = FILE_IMAGE_PATTERN.matcher(remaining);
        while (fileMatcher.find()) {
            String caption = fileMatcher.group(1);
            String filePath = fileMatcher.group(2);

            try {
                Path path = Path.of(filePath);
                if (Files.exists(path) && Files.isRegularFile(path)) {
                    byte[] imageBytes = Files.readAllBytes(path);
                    String fileName = path.getFileName().toString();

                    log.info("Sending local file image: {} ({} bytes)", fileName, imageBytes.length);
                    TelegramSend.sendPhoto(token, chatId, imageBytes, fileName,
                            caption.isBlank() ? null : caption, replyToMessageId, messageThreadId);
                } else {
                    log.debug("Image file not found: {}", filePath);
                }
            } catch (Exception e) {
                log.warn("Failed to read/send file image {}: {}", filePath, e.getMessage());
            }
        }
        remaining = FILE_IMAGE_PATTERN.matcher(remaining).replaceAll("");

        return remaining;
    }
}
