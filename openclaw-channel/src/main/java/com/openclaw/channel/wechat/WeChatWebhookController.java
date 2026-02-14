package com.openclaw.channel.wechat;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

/**
 * Handler for WeChat webhook callbacks.
 * <p>
 * NOT annotated with @RestController — registered conditionally via
 * {@link com.openclaw.channel.ChannelBeanConfig} only when WeChat is
 * configured.
 * <p>
 * Handles:
 * - GET /wechat-webhook → WeChat server URL verification (echostr)
 * - POST /wechat-webhook → Incoming user messages (XML)
 */
@Slf4j
public class WeChatWebhookController {

    private final String verifyToken;
    private final WeChatMessageHandler messageHandler;

    public WeChatWebhookController(String verifyToken, WeChatMessageHandler messageHandler) {
        this.verifyToken = verifyToken;
        this.messageHandler = messageHandler;
    }

    /**
     * WeChat server URL verification.
     * <p>
     * WeChat sends: GET ?signature=xxx&timestamp=xxx&nonce=xxx&echostr=xxx
     * We must validate the signature and return echostr if valid.
     */
    public ResponseEntity<String> verify(String signature, String timestamp, String nonce, String echostr) {
        log.info("WeChat webhook verification request received");

        if (WeChatXmlUtils.validateSignature(verifyToken, timestamp, nonce, signature)) {
            log.info("WeChat webhook verification succeeded");
            return ResponseEntity.ok(echostr);
        } else {
            log.warn("WeChat webhook verification failed — invalid signature");
            return ResponseEntity.status(403).body("Invalid signature");
        }
    }

    /**
     * Receive incoming messages from WeChat.
     * <p>
     * WeChat sends: POST with XML body.
     * We parse the XML, handle the message, and return a passive reply (or
     * "success").
     */
    public ResponseEntity<String> receiveMessage(String signature, String timestamp, String nonce, String xmlBody) {
        // Optionally validate signature on POST (some setups skip this)
        if (signature != null && timestamp != null && nonce != null) {
            if (!WeChatXmlUtils.validateSignature(verifyToken, timestamp, nonce, signature)) {
                log.warn("WeChat POST signature validation failed");
                return ResponseEntity.status(403).body("Invalid signature");
            }
        }

        log.debug("WeChat incoming message XML: {}", xmlBody);

        // Parse the XML message
        WeChatTypes.WeChatIncomingMessage message = WeChatXmlUtils.parseIncomingMessage(xmlBody);
        if (message == null) {
            log.warn("Failed to parse WeChat message XML");
            return ResponseEntity.ok("success");
        }

        // Handle the message and get a reply
        String reply = messageHandler.handleMessage(message);

        if (reply != null && !reply.isBlank()) {
            return ResponseEntity.ok()
                    .contentType(MediaType.APPLICATION_XML)
                    .body(reply);
        }

        // WeChat requires "success" if no passive reply
        return ResponseEntity.ok("success");
    }
}
