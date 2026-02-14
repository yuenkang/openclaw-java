package com.openclaw.channel.wechat;

import lombok.extern.slf4j.Slf4j;

/**
 * Handles incoming WeChat messages and produces replies.
 * <p>
 * Flow:
 * 1. WeChatWebhookController receives XML, calls this handler
 * 2. Handler parses the message and decides how to reply
 * 3. For quick replies (< 5s), returns a passive XML reply
 * 4. For long-running replies, returns "thinking..." and later uses
 * WeChatOutboundAdapter
 */
@Slf4j
public class WeChatMessageHandler {

    private final WeChatOutboundAdapter outboundAdapter;

    public WeChatMessageHandler(WeChatOutboundAdapter outboundAdapter) {
        this.outboundAdapter = outboundAdapter;
    }

    /**
     * Handle an incoming message and return a passive reply XML.
     * <p>
     * If the reply would take too long, this method returns a quick "thinking"
     * message
     * and dispatches the real reply asynchronously via the outbound adapter.
     *
     * @param message parsed incoming message
     * @return XML reply string, or {@code null} / empty to return "success" (no
     *         reply)
     */
    public String handleMessage(WeChatTypes.WeChatIncomingMessage message) {
        if (message == null) {
            return null;
        }

        String msgType = message.getMsgType();
        if (msgType == null) {
            return null;
        }

        return switch (msgType) {
            case "text" -> handleTextMessage(message);
            case "image" -> handleImageMessage(message);
            case "voice" -> handleVoiceMessage(message);
            case "event" -> handleEvent(message);
            default -> {
                log.debug("Unsupported WeChat message type: {}", msgType);
                yield null;
            }
        };
    }

    private String handleTextMessage(WeChatTypes.WeChatIncomingMessage message) {
        String userText = message.getContent();
        if (userText == null || userText.isBlank()) {
            return null;
        }

        log.info("WeChat text message from {}: {}", message.getFromUserName(), userText);

        // TODO: Integrate with Agent/AutoReply engine.
        // For now, return a passive reply acknowledging receipt.
        // In production, this should:
        // 1. Return a quick "正在思考中..." passive reply
        // 2. Asynchronously call the Agent for a real answer
        // 3. Send the real answer via outboundAdapter.sendText()

        String quickReply = "收到你的消息：" + userText + "\n正在处理中，请稍候...";

        // Dispatch async reply via outbound adapter (placeholder for Agent integration)
        // dispatchAgentReply(message.getFromUserName(), userText);

        return WeChatXmlUtils.buildTextReply(
                message.getFromUserName(),
                message.getToUserName(),
                quickReply);
    }

    private String handleImageMessage(WeChatTypes.WeChatIncomingMessage message) {
        log.info("WeChat image message from {}, mediaId: {}",
                message.getFromUserName(), message.getMediaId());

        return WeChatXmlUtils.buildTextReply(
                message.getFromUserName(),
                message.getToUserName(),
                "收到你的图片，暂不支持图片处理。");
    }

    private String handleVoiceMessage(WeChatTypes.WeChatIncomingMessage message) {
        // If speech recognition is enabled, use the recognized text
        String recognition = message.getRecognition();
        if (recognition != null && !recognition.isBlank()) {
            log.info("WeChat voice message (recognized) from {}: {}",
                    message.getFromUserName(), recognition);
            // Treat as text message
            WeChatTypes.WeChatIncomingMessage textMsg = WeChatTypes.WeChatIncomingMessage.builder()
                    .toUserName(message.getToUserName())
                    .fromUserName(message.getFromUserName())
                    .createTime(message.getCreateTime())
                    .msgType("text")
                    .content(recognition)
                    .build();
            return handleTextMessage(textMsg);
        }

        log.info("WeChat voice message from {}, format: {}",
                message.getFromUserName(), message.getFormat());

        return WeChatXmlUtils.buildTextReply(
                message.getFromUserName(),
                message.getToUserName(),
                "收到你的语音消息，请开启语音识别功能或发送文字消息。");
    }

    private String handleEvent(WeChatTypes.WeChatIncomingMessage message) {
        String event = message.getEvent();
        if (event == null) {
            return null;
        }

        return switch (event.toLowerCase()) {
            case "subscribe" -> {
                log.info("New WeChat subscriber: {}", message.getFromUserName());
                yield WeChatXmlUtils.buildTextReply(
                        message.getFromUserName(),
                        message.getToUserName(),
                        "欢迎关注！发送任何消息即可开始对话。");
            }
            case "unsubscribe" -> {
                log.info("WeChat unsubscribe: {}", message.getFromUserName());
                yield null; // No reply needed
            }
            default -> {
                log.debug("Unhandled WeChat event: {}", event);
                yield null;
            }
        };
    }
}
