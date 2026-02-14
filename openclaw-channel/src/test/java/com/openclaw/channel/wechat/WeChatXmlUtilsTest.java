package com.openclaw.channel.wechat;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link WeChatXmlUtils}.
 */
class WeChatXmlUtilsTest {

    // =========================================================================
    // Signature validation
    // =========================================================================

    @Test
    void validateSignature_validSignature_returnsTrue() {
        // SHA1(sort("testToken", "1234567890", "nonce123")) is a known hash
        String token = "testToken";
        String timestamp = "1234567890";
        String nonce = "nonce123";

        // Compute expected: sort → ["1234567890", "nonce123", "testToken"]
        // join → "1234567890nonce123testToken"
        // SHA1 of "1234567890nonce123testToken"
        String expectedSha1 = sha1("1234567890nonce123testToken");

        assertTrue(WeChatXmlUtils.validateSignature(token, timestamp, nonce, expectedSha1));
    }

    @Test
    void validateSignature_invalidSignature_returnsFalse() {
        assertFalse(WeChatXmlUtils.validateSignature("token", "ts", "nonce", "badsig"));
    }

    @Test
    void validateSignature_nullInputs_returnsFalse() {
        assertFalse(WeChatXmlUtils.validateSignature(null, "ts", "nonce", "sig"));
        assertFalse(WeChatXmlUtils.validateSignature("token", null, "nonce", "sig"));
        assertFalse(WeChatXmlUtils.validateSignature("token", "ts", null, "sig"));
        assertFalse(WeChatXmlUtils.validateSignature("token", "ts", "nonce", null));
    }

    // =========================================================================
    // XML parsing
    // =========================================================================

    @Test
    void parseIncomingMessage_textMessage_parsesCorrectly() {
        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[user_openid]]></FromUserName>
                  <CreateTime>1348831860</CreateTime>
                  <MsgType><![CDATA[text]]></MsgType>
                  <Content><![CDATA[Hello World]]></Content>
                  <MsgId>1234567890123456</MsgId>
                </xml>
                """;

        WeChatTypes.WeChatIncomingMessage msg = WeChatXmlUtils.parseIncomingMessage(xml);

        assertNotNull(msg);
        assertEquals("gh_bot", msg.getToUserName());
        assertEquals("user_openid", msg.getFromUserName());
        assertEquals(1348831860L, msg.getCreateTime());
        assertEquals("text", msg.getMsgType());
        assertEquals("Hello World", msg.getContent());
        assertEquals("1234567890123456", msg.getMsgId());
    }

    @Test
    void parseIncomingMessage_imageMessage_parsesCorrectly() {
        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[user_openid]]></FromUserName>
                  <CreateTime>1348831860</CreateTime>
                  <MsgType><![CDATA[image]]></MsgType>
                  <PicUrl><![CDATA[https://example.com/pic.jpg]]></PicUrl>
                  <MediaId><![CDATA[media_id_123]]></MediaId>
                  <MsgId>1234567890123456</MsgId>
                </xml>
                """;

        WeChatTypes.WeChatIncomingMessage msg = WeChatXmlUtils.parseIncomingMessage(xml);

        assertNotNull(msg);
        assertEquals("image", msg.getMsgType());
        assertEquals("https://example.com/pic.jpg", msg.getPicUrl());
        assertEquals("media_id_123", msg.getMediaId());
    }

    @Test
    void parseIncomingMessage_eventSubscribe_parsesCorrectly() {
        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[user_openid]]></FromUserName>
                  <CreateTime>1348831860</CreateTime>
                  <MsgType><![CDATA[event]]></MsgType>
                  <Event><![CDATA[subscribe]]></Event>
                </xml>
                """;

        WeChatTypes.WeChatIncomingMessage msg = WeChatXmlUtils.parseIncomingMessage(xml);

        assertNotNull(msg);
        assertEquals("event", msg.getMsgType());
        assertEquals("subscribe", msg.getEvent());
    }

    @Test
    void parseIncomingMessage_locationMessage_parsesCorrectly() {
        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[user_openid]]></FromUserName>
                  <CreateTime>1348831860</CreateTime>
                  <MsgType><![CDATA[location]]></MsgType>
                  <Location_X>23.134521</Location_X>
                  <Location_Y>113.358803</Location_Y>
                  <Scale>20</Scale>
                  <Label><![CDATA[某地标]]></Label>
                  <MsgId>1234567890123456</MsgId>
                </xml>
                """;

        WeChatTypes.WeChatIncomingMessage msg = WeChatXmlUtils.parseIncomingMessage(xml);

        assertNotNull(msg);
        assertEquals("location", msg.getMsgType());
        assertEquals(23.134521, msg.getLocationX(), 0.0001);
        assertEquals(113.358803, msg.getLocationY(), 0.0001);
        assertEquals(20, msg.getScale());
        assertEquals("某地标", msg.getLabel());
    }

    @Test
    void parseIncomingMessage_invalidXml_returnsNull() {
        assertNull(WeChatXmlUtils.parseIncomingMessage("not xml at all"));
    }

    // =========================================================================
    // XML reply building
    // =========================================================================

    @Test
    void buildTextReply_producesValidXml() {
        String reply = WeChatXmlUtils.buildTextReply("user123", "bot456", "Hello!");

        assertTrue(reply.contains("<ToUserName><![CDATA[user123]]></ToUserName>"));
        assertTrue(reply.contains("<FromUserName><![CDATA[bot456]]></FromUserName>"));
        assertTrue(reply.contains("<MsgType><![CDATA[text]]></MsgType>"));
        assertTrue(reply.contains("<Content><![CDATA[Hello!]]></Content>"));
        assertTrue(reply.contains("<CreateTime>"));
    }

    @Test
    void buildImageReply_producesValidXml() {
        String reply = WeChatXmlUtils.buildImageReply("user123", "bot456", "media_id_abc");

        assertTrue(reply.contains("<ToUserName><![CDATA[user123]]></ToUserName>"));
        assertTrue(reply.contains("<MsgType><![CDATA[image]]></MsgType>"));
        assertTrue(reply.contains("<MediaId><![CDATA[media_id_abc]]></MediaId>"));
    }

    // =========================================================================
    // Helper: compute SHA1 for test verification
    // =========================================================================

    private static String sha1(String input) {
        try {
            java.security.MessageDigest md = java.security.MessageDigest.getInstance("SHA-1");
            byte[] digest = md.digest(input.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder(digest.length * 2);
            for (byte b : digest) {
                sb.append(String.format("%02x", b));
            }
            return sb.toString();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
