package com.openclaw.channel.wechat;

import lombok.extern.slf4j.Slf4j;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.Arrays;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * WeChat XML parsing and signature verification utilities.
 */
@Slf4j
public final class WeChatXmlUtils {

    private WeChatXmlUtils() {
    }

    /**
     * Validate a WeChat webhook signature.
     * <p>
     * WeChat sends: signature = SHA1(sort(token, timestamp, nonce)).
     */
    public static boolean validateSignature(String token, String timestamp, String nonce, String signature) {
        if (token == null || timestamp == null || nonce == null || signature == null) {
            return false;
        }
        try {
            String[] arr = { token, timestamp, nonce };
            Arrays.sort(arr);
            String joined = String.join("", arr);
            MessageDigest md = MessageDigest.getInstance("SHA-1");
            byte[] digest = md.digest(joined.getBytes(StandardCharsets.UTF_8));
            String computed = bytesToHex(digest);
            return computed.equalsIgnoreCase(signature);
        } catch (Exception e) {
            log.error("Signature validation error: {}", e.getMessage());
            return false;
        }
    }

    /**
     * Parse an incoming WeChat XML message into a
     * {@link WeChatTypes.WeChatIncomingMessage}.
     */
    public static WeChatTypes.WeChatIncomingMessage parseIncomingMessage(String xml) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            // Prevent XXE
            factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
            factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);

            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse(new ByteArrayInputStream(xml.getBytes(StandardCharsets.UTF_8)));
            Element root = doc.getDocumentElement();

            WeChatTypes.WeChatIncomingMessage msg = WeChatTypes.WeChatIncomingMessage.builder()
                    .toUserName(getElementText(root, "ToUserName"))
                    .fromUserName(getElementText(root, "FromUserName"))
                    .createTime(parseLong(getElementText(root, "CreateTime")))
                    .msgType(getElementText(root, "MsgType"))
                    .msgId(getElementText(root, "MsgId"))
                    .content(getElementText(root, "Content"))
                    .picUrl(getElementText(root, "PicUrl"))
                    .mediaId(getElementText(root, "MediaId"))
                    .format(getElementText(root, "Format"))
                    .recognition(getElementText(root, "Recognition"))
                    .thumbMediaId(getElementText(root, "ThumbMediaId"))
                    .locationX(parseDouble(getElementText(root, "Location_X")))
                    .locationY(parseDouble(getElementText(root, "Location_Y")))
                    .scale(parseInt(getElementText(root, "Scale")))
                    .label(getElementText(root, "Label"))
                    .title(getElementText(root, "Title"))
                    .description(getElementText(root, "Description"))
                    .url(getElementText(root, "Url"))
                    .event(getElementText(root, "Event"))
                    .eventKey(getElementText(root, "EventKey"))
                    .build();

            return msg;
        } catch (Exception e) {
            log.error("Failed to parse WeChat XML message: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Build a passive text reply XML for WeChat.
     */
    public static String buildTextReply(String toUser, String fromUser, String content) {
        long timestamp = System.currentTimeMillis() / 1000;
        return "<xml>"
                + "<ToUserName><![CDATA[" + toUser + "]]></ToUserName>"
                + "<FromUserName><![CDATA[" + fromUser + "]]></FromUserName>"
                + "<CreateTime>" + timestamp + "</CreateTime>"
                + "<MsgType><![CDATA[text]]></MsgType>"
                + "<Content><![CDATA[" + content + "]]></Content>"
                + "</xml>";
    }

    /**
     * Build a passive image reply XML for WeChat.
     */
    public static String buildImageReply(String toUser, String fromUser, String mediaId) {
        long timestamp = System.currentTimeMillis() / 1000;
        return "<xml>"
                + "<ToUserName><![CDATA[" + toUser + "]]></ToUserName>"
                + "<FromUserName><![CDATA[" + fromUser + "]]></FromUserName>"
                + "<CreateTime>" + timestamp + "</CreateTime>"
                + "<MsgType><![CDATA[image]]></MsgType>"
                + "<Image>"
                + "<MediaId><![CDATA[" + mediaId + "]]></MediaId>"
                + "</Image>"
                + "</xml>";
    }

    // --- Private helpers ---

    private static String getElementText(Element root, String tagName) {
        NodeList nodes = root.getElementsByTagName(tagName);
        if (nodes.getLength() == 0) {
            return null;
        }
        String text = nodes.item(0).getTextContent();
        return (text != null && !text.isEmpty()) ? text : null;
    }

    private static long parseLong(String s) {
        if (s == null)
            return 0;
        try {
            return Long.parseLong(s);
        } catch (NumberFormatException e) {
            return 0;
        }
    }

    private static Double parseDouble(String s) {
        if (s == null)
            return null;
        try {
            return Double.parseDouble(s);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private static Integer parseInt(String s) {
        if (s == null)
            return null;
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private static String bytesToHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder(bytes.length * 2);
        for (byte b : bytes) {
            sb.append(String.format("%02x", b));
        }
        return sb.toString();
    }
}
