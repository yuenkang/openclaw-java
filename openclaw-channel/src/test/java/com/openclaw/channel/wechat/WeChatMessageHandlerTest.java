package com.openclaw.channel.wechat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link WeChatMessageHandler}.
 */
class WeChatMessageHandlerTest {

    private WeChatMessageHandler handler;

    @BeforeEach
    void setUp() {
        // Use null outbound adapter since it's not invoked in passive reply mode
        handler = new WeChatMessageHandler(null);
    }

    @Test
    void handleMessage_textMessage_returnsReply() {
        WeChatTypes.WeChatIncomingMessage msg = WeChatTypes.WeChatIncomingMessage.builder()
                .toUserName("bot")
                .fromUserName("user")
                .msgType("text")
                .content("你好")
                .build();

        String reply = handler.handleMessage(msg);

        assertNotNull(reply);
        assertTrue(reply.contains("<ToUserName><![CDATA[user]]></ToUserName>"));
        assertTrue(reply.contains("<FromUserName><![CDATA[bot]]></FromUserName>"));
        assertTrue(reply.contains("你好"));
    }

    @Test
    void handleMessage_emptyTextContent_returnsNull() {
        WeChatTypes.WeChatIncomingMessage msg = WeChatTypes.WeChatIncomingMessage.builder()
                .toUserName("bot")
                .fromUserName("user")
                .msgType("text")
                .content("")
                .build();

        assertNull(handler.handleMessage(msg));
    }

    @Test
    void handleMessage_imageMessage_returnsAcknowledgement() {
        WeChatTypes.WeChatIncomingMessage msg = WeChatTypes.WeChatIncomingMessage.builder()
                .toUserName("bot")
                .fromUserName("user")
                .msgType("image")
                .mediaId("media123")
                .build();

        String reply = handler.handleMessage(msg);

        assertNotNull(reply);
        assertTrue(reply.contains("图片"));
    }

    @Test
    void handleMessage_voiceWithRecognition_treatsAsText() {
        WeChatTypes.WeChatIncomingMessage msg = WeChatTypes.WeChatIncomingMessage.builder()
                .toUserName("bot")
                .fromUserName("user")
                .msgType("voice")
                .recognition("语音识别结果")
                .build();

        String reply = handler.handleMessage(msg);

        assertNotNull(reply);
        assertTrue(reply.contains("语音识别结果"));
    }

    @Test
    void handleMessage_subscribeEvent_returnsWelcome() {
        WeChatTypes.WeChatIncomingMessage msg = WeChatTypes.WeChatIncomingMessage.builder()
                .toUserName("bot")
                .fromUserName("user")
                .msgType("event")
                .event("subscribe")
                .build();

        String reply = handler.handleMessage(msg);

        assertNotNull(reply);
        assertTrue(reply.contains("欢迎"));
    }

    @Test
    void handleMessage_unsubscribeEvent_returnsNull() {
        WeChatTypes.WeChatIncomingMessage msg = WeChatTypes.WeChatIncomingMessage.builder()
                .toUserName("bot")
                .fromUserName("user")
                .msgType("event")
                .event("unsubscribe")
                .build();

        assertNull(handler.handleMessage(msg));
    }

    @Test
    void handleMessage_unknownType_returnsNull() {
        WeChatTypes.WeChatIncomingMessage msg = WeChatTypes.WeChatIncomingMessage.builder()
                .toUserName("bot")
                .fromUserName("user")
                .msgType("unknown_type")
                .build();

        assertNull(handler.handleMessage(msg));
    }

    @Test
    void handleMessage_null_returnsNull() {
        assertNull(handler.handleMessage(null));
    }
}
