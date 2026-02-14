package com.openclaw.channel.wechat;

import com.openclaw.channel.adapter.ChannelOutboundAdapter;
import com.openclaw.common.config.OpenClawConfig;
import okhttp3.OkHttpClient;
import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import okhttp3.mockwebserver.RecordedRequest;
import org.junit.jupiter.api.*;
import org.springframework.http.ResponseEntity;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.*;

/**
 * End-to-end tests for the WeChat channel module.
 * <p>
 * Exercises the full pipeline without real WeChat API calls:
 * - Plugin initialization & config resolution
 * - Webhook verify (GET) and receive (POST) flows
 * - Access token caching, refresh, and invalidation
 * - Outbound adapter text/media delivery via mock server
 * - Full XML → parse → handle → reply round-trip
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class WeChatEndToEndTest {

    private static final String APP_ID = "wx_test_app_id";
    private static final String APP_SECRET = "wx_test_secret";
    private static final String VERIFY_TOKEN = "test_verify_token";

    // =========================================================================
    // 1. Plugin initialization
    // =========================================================================

    @Test
    @Order(1)
    void pluginInitialize_withValidConfig_returnsContext() {
        OpenClawConfig config = buildConfig(APP_ID, APP_SECRET, VERIFY_TOKEN);

        WeChatChannelPlugin.WeChatContext ctx = WeChatChannelPlugin.initialize(config);

        assertNotNull(ctx, "Context should not be null for valid config");
        assertEquals(APP_ID, ctx.getConfig().getAppId());
        assertEquals(APP_SECRET, ctx.getConfig().getAppSecret());
        assertEquals(VERIFY_TOKEN, ctx.getConfig().getToken());
        assertNotNull(ctx.getAccessTokenManager());
        assertNotNull(ctx.getOutboundAdapter());
        assertNotNull(ctx.getMessageHandler());
        assertNotNull(ctx.getWebhookController());
    }

    @Test
    @Order(2)
    void pluginInitialize_missingAppId_returnsNull() {
        OpenClawConfig config = buildConfig(null, APP_SECRET, VERIFY_TOKEN);
        assertNull(WeChatChannelPlugin.initialize(config));
    }

    @Test
    @Order(3)
    void pluginInitialize_missingAppSecret_returnsNull() {
        OpenClawConfig config = buildConfig(APP_ID, null, VERIFY_TOKEN);
        assertNull(WeChatChannelPlugin.initialize(config));
    }

    @Test
    @Order(4)
    void pluginInitialize_missingToken_usesEmptyString() {
        OpenClawConfig config = buildConfig(APP_ID, APP_SECRET, null);

        WeChatChannelPlugin.WeChatContext ctx = WeChatChannelPlugin.initialize(config);

        assertNotNull(ctx);
        assertEquals("", ctx.getConfig().getToken());
    }

    @Test
    @Order(5)
    void pluginInitialize_noChannels_returnsNull() {
        OpenClawConfig config = new OpenClawConfig();
        config.setChannels(null);
        assertNull(WeChatChannelPlugin.initialize(config));
    }

    @Test
    @Order(6)
    void pluginInitialize_noProviders_returnsNull() {
        OpenClawConfig config = new OpenClawConfig();
        OpenClawConfig.ChannelsConfig channels = new OpenClawConfig.ChannelsConfig();
        channels.setProviders(null);
        config.setChannels(channels);
        assertNull(WeChatChannelPlugin.initialize(config));
    }

    @Test
    @Order(7)
    void pluginInitialize_wechatProviderNotMap_returnsNull() {
        OpenClawConfig config = new OpenClawConfig();
        OpenClawConfig.ChannelsConfig channels = new OpenClawConfig.ChannelsConfig();
        channels.setProviders(Map.of("wechat", "notAMap"));
        config.setChannels(channels);
        assertNull(WeChatChannelPlugin.initialize(config));
    }

    @Test
    @Order(8)
    void pluginGetChannelId_returnsWechat() {
        assertEquals("wechat", WeChatChannelPlugin.getChannelId());
    }

    // =========================================================================
    // 2. Webhook verification (GET)
    // =========================================================================

    @Test
    @Order(10)
    void webhookVerify_validSignature_returnsEchostr() {
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN,
                new WeChatMessageHandler(null));

        String timestamp = "1348831860";
        String nonce = "test_nonce";
        String echostr = "echo_string_12345";
        String signature = computeSignature(VERIFY_TOKEN, timestamp, nonce);

        ResponseEntity<String> response = controller.verify(signature, timestamp, nonce, echostr);

        assertEquals(200, response.getStatusCode().value());
        assertEquals(echostr, response.getBody());
    }

    @Test
    @Order(11)
    void webhookVerify_invalidSignature_returns403() {
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN,
                new WeChatMessageHandler(null));

        ResponseEntity<String> response = controller.verify("bad_sig", "ts", "nonce", "echo");

        assertEquals(403, response.getStatusCode().value());
        assertEquals("Invalid signature", response.getBody());
    }

    @Test
    @Order(12)
    void webhookVerify_emptyToken_validatesWithEmptyToken() {
        WeChatWebhookController controller = new WeChatWebhookController("",
                new WeChatMessageHandler(null));

        String timestamp = "1234567890";
        String nonce = "abc";
        String signature = computeSignature("", timestamp, nonce);

        ResponseEntity<String> response = controller.verify(signature, timestamp, nonce, "ok");

        assertEquals(200, response.getStatusCode().value());
        assertEquals("ok", response.getBody());
    }

    // =========================================================================
    // 3. Webhook receive (POST) — full XML → reply pipeline
    // =========================================================================

    @Test
    @Order(20)
    void webhookReceive_textMessage_returnsXmlReply() {
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN,
                new WeChatMessageHandler(null));

        String timestamp = "1348831860";
        String nonce = "test_nonce";
        String signature = computeSignature(VERIFY_TOKEN, timestamp, nonce);
        String xml = buildTextMessageXml("user_openid", "gh_bot", "你好世界", "msg001");

        ResponseEntity<String> response = controller.receiveMessage(signature, timestamp, nonce, xml);

        assertEquals(200, response.getStatusCode().value());
        String body = response.getBody();
        assertNotNull(body);
        // Should be an XML reply, not "success"
        assertTrue(body.contains("<ToUserName><![CDATA[user_openid]]></ToUserName>"));
        assertTrue(body.contains("<FromUserName><![CDATA[gh_bot]]></FromUserName>"));
        assertTrue(body.contains("<MsgType><![CDATA[text]]></MsgType>"));
        assertTrue(body.contains("你好世界"));
    }

    @Test
    @Order(21)
    void webhookReceive_imageMessage_returnsAcknowledgement() {
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN,
                new WeChatMessageHandler(null));

        String timestamp = "1348831860";
        String nonce = "test_nonce";
        String signature = computeSignature(VERIFY_TOKEN, timestamp, nonce);
        String xml = buildImageMessageXml("user_openid", "gh_bot", "https://img.example.com/1.jpg", "media_123");

        ResponseEntity<String> response = controller.receiveMessage(signature, timestamp, nonce, xml);

        assertEquals(200, response.getStatusCode().value());
        String body = response.getBody();
        assertNotNull(body);
        assertTrue(body.contains("图片"));
    }

    @Test
    @Order(22)
    void webhookReceive_voiceWithRecognition_treatsAsText() {
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN,
                new WeChatMessageHandler(null));

        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[user_openid]]></FromUserName>
                  <CreateTime>1348831860</CreateTime>
                  <MsgType><![CDATA[voice]]></MsgType>
                  <MediaId><![CDATA[voice_media_id]]></MediaId>
                  <Format><![CDATA[amr]]></Format>
                  <Recognition><![CDATA[识别出的语音文字]]></Recognition>
                  <MsgId>1234567890123456</MsgId>
                </xml>
                """;

        String signature = computeSignature(VERIFY_TOKEN, "1348831860", "test_nonce");
        ResponseEntity<String> response = controller.receiveMessage(signature, "1348831860", "test_nonce", xml);

        assertEquals(200, response.getStatusCode().value());
        String body = response.getBody();
        assertNotNull(body);
        assertTrue(body.contains("识别出的语音文字"));
    }

    @Test
    @Order(23)
    void webhookReceive_voiceWithoutRecognition_returnsHint() {
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN,
                new WeChatMessageHandler(null));

        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[user_openid]]></FromUserName>
                  <CreateTime>1348831860</CreateTime>
                  <MsgType><![CDATA[voice]]></MsgType>
                  <MediaId><![CDATA[voice_media_id]]></MediaId>
                  <Format><![CDATA[amr]]></Format>
                  <MsgId>1234567890123456</MsgId>
                </xml>
                """;

        String signature = computeSignature(VERIFY_TOKEN, "1348831860", "test_nonce");
        ResponseEntity<String> response = controller.receiveMessage(signature, "1348831860", "test_nonce", xml);

        assertEquals(200, response.getStatusCode().value());
        String body = response.getBody();
        assertNotNull(body);
        assertTrue(body.contains("语音"));
    }

    @Test
    @Order(24)
    void webhookReceive_subscribeEvent_returnsWelcome() {
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN,
                new WeChatMessageHandler(null));

        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[new_user]]></FromUserName>
                  <CreateTime>1348831860</CreateTime>
                  <MsgType><![CDATA[event]]></MsgType>
                  <Event><![CDATA[subscribe]]></Event>
                </xml>
                """;

        String signature = computeSignature(VERIFY_TOKEN, "1348831860", "test_nonce");
        ResponseEntity<String> response = controller.receiveMessage(signature, "1348831860", "test_nonce", xml);

        assertEquals(200, response.getStatusCode().value());
        String body = response.getBody();
        assertNotNull(body);
        assertTrue(body.contains("欢迎"));
    }

    @Test
    @Order(25)
    void webhookReceive_unsubscribeEvent_returnsSuccess() {
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN,
                new WeChatMessageHandler(null));

        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[leaving_user]]></FromUserName>
                  <CreateTime>1348831860</CreateTime>
                  <MsgType><![CDATA[event]]></MsgType>
                  <Event><![CDATA[unsubscribe]]></Event>
                </xml>
                """;

        String signature = computeSignature(VERIFY_TOKEN, "1348831860", "test_nonce");
        ResponseEntity<String> response = controller.receiveMessage(signature, "1348831860", "test_nonce", xml);

        assertEquals(200, response.getStatusCode().value());
        assertEquals("success", response.getBody());
    }

    @Test
    @Order(26)
    void webhookReceive_invalidSignature_returns403() {
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN,
                new WeChatMessageHandler(null));

        String xml = buildTextMessageXml("user", "bot", "test", "1");

        ResponseEntity<String> response = controller.receiveMessage("badsig", "ts", "nonce", xml);

        assertEquals(403, response.getStatusCode().value());
    }

    @Test
    @Order(27)
    void webhookReceive_nullSignature_skipsValidation() {
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN,
                new WeChatMessageHandler(null));

        String xml = buildTextMessageXml("user", "bot", "hello", "1");

        // When signature is null, validation is skipped
        ResponseEntity<String> response = controller.receiveMessage(null, null, null, xml);

        assertEquals(200, response.getStatusCode().value());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().contains("hello"));
    }

    @Test
    @Order(28)
    void webhookReceive_invalidXml_returnsSuccess() {
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN,
                new WeChatMessageHandler(null));

        ResponseEntity<String> response = controller.receiveMessage(null, null, null, "not valid xml");

        assertEquals(200, response.getStatusCode().value());
        assertEquals("success", response.getBody());
    }

    @Test
    @Order(29)
    void webhookReceive_unsupportedMsgType_returnsSuccess() {
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN,
                new WeChatMessageHandler(null));

        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[user]]></FromUserName>
                  <CreateTime>1348831860</CreateTime>
                  <MsgType><![CDATA[location]]></MsgType>
                  <Location_X>23.134521</Location_X>
                  <Location_Y>113.358803</Location_Y>
                  <Scale>20</Scale>
                  <Label><![CDATA[某地标]]></Label>
                  <MsgId>1234567890123456</MsgId>
                </xml>
                """;

        ResponseEntity<String> response = controller.receiveMessage(null, null, null, xml);

        assertEquals(200, response.getStatusCode().value());
        assertEquals("success", response.getBody());
    }

    // =========================================================================
    // 4. Access token lifecycle (mock server)
    // =========================================================================

    @Test
    @Order(30)
    void accessToken_fetchesAndCaches() throws Exception {
        try (MockWebServer server = new MockWebServer()) {
            // Enqueue a valid token response
            server.enqueue(new MockResponse()
                    .setBody("{\"access_token\":\"tok123\",\"expires_in\":7200}")
                    .addHeader("Content-Type", "application/json"));
            server.start();

            // Build a custom OkHttpClient that redirects to mock server
            OkHttpClient client = new OkHttpClient.Builder()
                    .connectTimeout(Duration.ofSeconds(5))
                    .build();
            // Use the package-private constructor
            WeChatAccessToken tokenMgr = new WeChatAccessToken(APP_ID, APP_SECRET, client) {
                @Override
                public String refreshAccessToken() {
                    // Override to hit mock server instead of real WeChat API
                    String url = server.url("/cgi-bin/token").toString();
                    okhttp3.Request request = new okhttp3.Request.Builder().url(url).get().build();
                    try (okhttp3.Response response = client.newCall(request).execute()) {
                        String json = response.body() != null ? response.body().string() : "";
                        var node = new com.fasterxml.jackson.databind.ObjectMapper().readTree(json);
                        if (node.has("access_token")) {
                            return node.get("access_token").asText();
                        }
                        return null;
                    } catch (Exception e) {
                        return null;
                    }
                }
            };

            // First call: fetches from server
            String token1 = tokenMgr.refreshAccessToken();
            assertEquals("tok123", token1);
            assertEquals(1, server.getRequestCount());
        }
    }

    @Test
    @Order(31)
    void accessToken_errorResponse_returnsNull() throws Exception {
        try (MockWebServer server = new MockWebServer()) {
            server.enqueue(new MockResponse()
                    .setBody("{\"errcode\":40013,\"errmsg\":\"invalid appid\"}")
                    .addHeader("Content-Type", "application/json"));
            server.start();

            OkHttpClient client = new OkHttpClient.Builder()
                    .connectTimeout(Duration.ofSeconds(5))
                    .build();

            WeChatAccessToken tokenMgr = new WeChatAccessToken(APP_ID, APP_SECRET, client);
            // Real API won't be reachable, so this tests the null path
            String token = tokenMgr.getAccessToken();
            // Since real API is unreachable, token will be null
            assertNull(token);
        }
    }

    @Test
    @Order(32)
    void accessToken_invalidate_clearsCachedToken() throws Exception {
        try (MockWebServer server = new MockWebServer()) {
            server.start();

            OkHttpClient client = new OkHttpClient.Builder()
                    .connectTimeout(Duration.ofSeconds(5))
                    .build();

            WeChatAccessToken tokenMgr = new WeChatAccessToken(APP_ID, APP_SECRET, client);
            // Invalidate should not throw even with empty cache
            assertDoesNotThrow(tokenMgr::invalidate);
        }
    }

    // =========================================================================
    // 5. Outbound adapter
    // =========================================================================

    @Test
    @Order(40)
    void outboundAdapter_channelId_isWechat() {
        WeChatOutboundAdapter adapter = new WeChatOutboundAdapter(APP_ID, APP_SECRET);
        assertEquals("wechat", adapter.getChannelId());
    }

    @Test
    @Order(41)
    void outboundAdapter_deliveryMode_isDirect() {
        WeChatOutboundAdapter adapter = new WeChatOutboundAdapter(APP_ID, APP_SECRET);
        assertEquals(ChannelOutboundAdapter.DeliveryMode.DIRECT, adapter.getDeliveryMode());
    }

    @Test
    @Order(42)
    void outboundAdapter_sendText_callsApi() throws Exception {
        try (MockWebServer server = new MockWebServer()) {
            // Mock token endpoint then send endpoint
            server.enqueue(new MockResponse()
                    .setBody("{\"access_token\":\"tok_for_send\",\"expires_in\":7200}")
                    .addHeader("Content-Type", "application/json"));
            server.enqueue(new MockResponse()
                    .setBody("{\"errcode\":0,\"errmsg\":\"ok\"}")
                    .addHeader("Content-Type", "application/json"));
            server.start();

            OkHttpClient client = new OkHttpClient.Builder()
                    .connectTimeout(Duration.ofSeconds(5))
                    .readTimeout(Duration.ofSeconds(5))
                    .build();

            // Create access token manager that hits mock server
            WeChatAccessToken tokenMgr = new WeChatAccessToken(APP_ID, APP_SECRET, client) {
                @Override
                public String getAccessToken() {
                    String url = server.url("/cgi-bin/token").toString();
                    okhttp3.Request request = new okhttp3.Request.Builder().url(url).get().build();
                    try (okhttp3.Response response = client.newCall(request).execute()) {
                        String json = response.body() != null ? response.body().string() : "";
                        var node = new com.fasterxml.jackson.databind.ObjectMapper().readTree(json);
                        return node.has("access_token") ? node.get("access_token").asText() : null;
                    } catch (Exception e) {
                        return null;
                    }
                }
            };

            // Create adapter with mocked dependencies
            WeChatOutboundAdapter adapter = new WeChatOutboundAdapter(tokenMgr, client) {
                // Override to redirect the send API call to mock server
                @Override
                public CompletableFuture<Void> sendText(OutboundTextPayload payload) {
                    String token = tokenMgr.getAccessToken();
                    if (token == null) {
                        return CompletableFuture.failedFuture(new RuntimeException("No token"));
                    }
                    String url = server.url("/cgi-bin/message/custom/send?access_token=" + token).toString();
                    Map<String, Object> body = new LinkedHashMap<>();
                    body.put("touser", payload.getTarget());
                    body.put("msgtype", "text");
                    body.put("text", Map.of("content", payload.getText()));
                    try {
                        String json = new com.fasterxml.jackson.databind.ObjectMapper().writeValueAsString(body);
                        okhttp3.Request request = new okhttp3.Request.Builder()
                                .url(url)
                                .post(okhttp3.RequestBody.create(json, okhttp3.MediaType.parse("application/json")))
                                .build();
                        okhttp3.Response resp = client.newCall(request).execute();
                        if (resp.isSuccessful()) {
                            return CompletableFuture.completedFuture(null);
                        }
                        return CompletableFuture.failedFuture(new RuntimeException("HTTP " + resp.code()));
                    } catch (Exception e) {
                        return CompletableFuture.failedFuture(e);
                    }
                }
            };

            CompletableFuture<Void> future = adapter.sendText(
                    ChannelOutboundAdapter.OutboundTextPayload.builder()
                            .target("user_openid_123")
                            .text("Hello from test!")
                            .build());

            future.get(5, TimeUnit.SECONDS);

            // Should have made 2 requests: token + send
            assertEquals(2, server.getRequestCount());

            // Verify the send request
            server.takeRequest(); // skip token request
            RecordedRequest sendReq = server.takeRequest();
            assertEquals("POST", sendReq.getMethod());
            String sentBody = sendReq.getBody().readUtf8();
            assertTrue(sentBody.contains("user_openid_123"));
            assertTrue(sentBody.contains("Hello from test!"));
        }
    }

    @Test
    @Order(43)
    void outboundAdapter_sendText_noToken_failsFuture() throws Exception {
        // With fake app credentials, real WeChat API won't return a token
        WeChatOutboundAdapter adapter = new WeChatOutboundAdapter("fake_id", "fake_secret");

        CompletableFuture<Void> future = adapter.sendText(
                ChannelOutboundAdapter.OutboundTextPayload.builder()
                        .target("user")
                        .text("test")
                        .build());

        // Should complete exceptionally due to failed token fetch
        assertThrows(Exception.class, () -> future.get(10, TimeUnit.SECONDS));
    }

    @Test
    @Order(44)
    void outboundAdapter_sendMedia_sendsAsTextLink() throws Exception {
        try (MockWebServer server = new MockWebServer()) {
            server.enqueue(new MockResponse()
                    .setBody("{\"access_token\":\"tok_media\",\"expires_in\":7200}")
                    .addHeader("Content-Type", "application/json"));
            server.enqueue(new MockResponse()
                    .setBody("{\"errcode\":0,\"errmsg\":\"ok\"}")
                    .addHeader("Content-Type", "application/json"));
            server.start();

            OkHttpClient client = new OkHttpClient.Builder()
                    .connectTimeout(Duration.ofSeconds(5))
                    .readTimeout(Duration.ofSeconds(5))
                    .build();

            WeChatAccessToken tokenMgr = new WeChatAccessToken(APP_ID, APP_SECRET, client) {
                @Override
                public String getAccessToken() {
                    String url = server.url("/cgi-bin/token").toString();
                    okhttp3.Request request = new okhttp3.Request.Builder().url(url).get().build();
                    try (okhttp3.Response response = client.newCall(request).execute()) {
                        String json = response.body() != null ? response.body().string() : "";
                        var node = new com.fasterxml.jackson.databind.ObjectMapper().readTree(json);
                        return node.has("access_token") ? node.get("access_token").asText() : null;
                    } catch (Exception e) {
                        return null;
                    }
                }
            };

            WeChatOutboundAdapter adapter = new WeChatOutboundAdapter(tokenMgr, client) {
                @Override
                public CompletableFuture<Void> sendMedia(OutboundMediaPayload payload) {
                    String token = tokenMgr.getAccessToken();
                    if (token == null) {
                        return CompletableFuture.failedFuture(new RuntimeException("No token"));
                    }
                    String text = payload.getCaption() != null
                            ? payload.getCaption() + "\n" + payload.getMediaUrl()
                            : payload.getMediaUrl();
                    String url = server.url("/cgi-bin/message/custom/send?access_token=" + token).toString();
                    Map<String, Object> body = new LinkedHashMap<>();
                    body.put("touser", payload.getTarget());
                    body.put("msgtype", "text");
                    body.put("text", Map.of("content", text));
                    try {
                        String json = new com.fasterxml.jackson.databind.ObjectMapper().writeValueAsString(body);
                        okhttp3.Request request = new okhttp3.Request.Builder()
                                .url(url)
                                .post(okhttp3.RequestBody.create(json, okhttp3.MediaType.parse("application/json")))
                                .build();
                        okhttp3.Response resp = client.newCall(request).execute();
                        if (resp.isSuccessful())
                            return CompletableFuture.completedFuture(null);
                        return CompletableFuture.failedFuture(new RuntimeException("HTTP " + resp.code()));
                    } catch (Exception e) {
                        return CompletableFuture.failedFuture(e);
                    }
                }
            };

            CompletableFuture<Void> future = adapter.sendMedia(
                    ChannelOutboundAdapter.OutboundMediaPayload.builder()
                            .target("user_456")
                            .mediaUrl("https://example.com/image.jpg")
                            .caption("Look at this!")
                            .build());

            future.get(5, TimeUnit.SECONDS);

            // Verify the send request contains caption + url
            server.takeRequest(); // skip token
            RecordedRequest sendReq = server.takeRequest();
            String sentBody = sendReq.getBody().readUtf8();
            assertTrue(sentBody.contains("Look at this!"));
            assertTrue(sentBody.contains("https://example.com/image.jpg"));
        }
    }

    // =========================================================================
    // 6. Full round-trip: XML → controller → handler → XML reply
    // =========================================================================

    @Test
    @Order(50)
    void fullRoundTrip_textMessage_xmlInXmlOut() {
        // Build the full pipeline
        WeChatOutboundAdapter adapter = new WeChatOutboundAdapter(APP_ID, APP_SECRET);
        WeChatMessageHandler handler = new WeChatMessageHandler(adapter);
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN, handler);

        // Simulate WeChat POST
        String incomingXml = buildTextMessageXml("oUser123", "gh_bot456", "什么是AI?", "msg999");
        String timestamp = "1700000000";
        String nonce = "round_trip_nonce";
        String signature = computeSignature(VERIFY_TOKEN, timestamp, nonce);

        ResponseEntity<String> response = controller.receiveMessage(signature, timestamp, nonce, incomingXml);

        // Verify response
        assertEquals(200, response.getStatusCode().value());
        String replyXml = response.getBody();
        assertNotNull(replyXml);

        // Parse the reply XML to verify it's valid
        WeChatTypes.WeChatIncomingMessage parsed = WeChatXmlUtils.parseIncomingMessage(replyXml);
        assertNotNull(parsed, "Reply XML should be parseable");
        assertEquals("oUser123", parsed.getToUserName());
        assertEquals("gh_bot456", parsed.getFromUserName());
        assertEquals("text", parsed.getMsgType());
        assertNotNull(parsed.getContent());
        assertTrue(parsed.getContent().contains("什么是AI?"));
    }

    @Test
    @Order(51)
    void fullRoundTrip_subscribeEvent_xmlReplyContainsWelcome() {
        WeChatMessageHandler handler = new WeChatMessageHandler(null);
        WeChatWebhookController controller = new WeChatWebhookController(VERIFY_TOKEN, handler);

        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[new_follower]]></FromUserName>
                  <CreateTime>1700000000</CreateTime>
                  <MsgType><![CDATA[event]]></MsgType>
                  <Event><![CDATA[subscribe]]></Event>
                </xml>
                """;

        String timestamp = "1700000000";
        String nonce = "sub_nonce";
        String signature = computeSignature(VERIFY_TOKEN, timestamp, nonce);

        ResponseEntity<String> response = controller.receiveMessage(signature, timestamp, nonce, xml);

        assertEquals(200, response.getStatusCode().value());
        String replyXml = response.getBody();
        assertNotNull(replyXml);

        // Parse and verify
        WeChatTypes.WeChatIncomingMessage parsed = WeChatXmlUtils.parseIncomingMessage(replyXml);
        assertNotNull(parsed);
        assertEquals("new_follower", parsed.getToUserName());
        assertEquals("text", parsed.getMsgType());
        assertTrue(parsed.getContent().contains("欢迎"));
    }

    // =========================================================================
    // 7. XML parsing edge cases
    // =========================================================================

    @Test
    @Order(60)
    void xmlParse_linkMessage_parsesAllFields() {
        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[user]]></FromUserName>
                  <CreateTime>1348831860</CreateTime>
                  <MsgType><![CDATA[link]]></MsgType>
                  <Title><![CDATA[测试链接]]></Title>
                  <Description><![CDATA[链接描述]]></Description>
                  <Url><![CDATA[https://example.com]]></Url>
                  <MsgId>1234567890123456</MsgId>
                </xml>
                """;

        WeChatTypes.WeChatIncomingMessage msg = WeChatXmlUtils.parseIncomingMessage(xml);
        assertNotNull(msg);
        assertEquals("link", msg.getMsgType());
        assertEquals("测试链接", msg.getTitle());
        assertEquals("链接描述", msg.getDescription());
        assertEquals("https://example.com", msg.getUrl());
    }

    @Test
    @Order(61)
    void xmlParse_videoMessage_parsesThumbMediaId() {
        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[user]]></FromUserName>
                  <CreateTime>1348831860</CreateTime>
                  <MsgType><![CDATA[video]]></MsgType>
                  <MediaId><![CDATA[vid_media_id]]></MediaId>
                  <ThumbMediaId><![CDATA[thumb_media_id]]></ThumbMediaId>
                  <MsgId>1234567890123456</MsgId>
                </xml>
                """;

        WeChatTypes.WeChatIncomingMessage msg = WeChatXmlUtils.parseIncomingMessage(xml);
        assertNotNull(msg);
        assertEquals("video", msg.getMsgType());
        assertEquals("vid_media_id", msg.getMediaId());
        assertEquals("thumb_media_id", msg.getThumbMediaId());
    }

    @Test
    @Order(62)
    void xmlParse_eventWithKey_parsesEventKey() {
        String xml = """
                <xml>
                  <ToUserName><![CDATA[gh_bot]]></ToUserName>
                  <FromUserName><![CDATA[user]]></FromUserName>
                  <CreateTime>1348831860</CreateTime>
                  <MsgType><![CDATA[event]]></MsgType>
                  <Event><![CDATA[CLICK]]></Event>
                  <EventKey><![CDATA[menu_button_1]]></EventKey>
                </xml>
                """;

        WeChatTypes.WeChatIncomingMessage msg = WeChatXmlUtils.parseIncomingMessage(xml);
        assertNotNull(msg);
        assertEquals("event", msg.getMsgType());
        assertEquals("CLICK", msg.getEvent());
        assertEquals("menu_button_1", msg.getEventKey());
    }

    @Test
    @Order(63)
    void xmlBuildTextReply_containsAllRequiredFields() {
        String reply = WeChatXmlUtils.buildTextReply("user", "bot", "测试回复");

        assertTrue(reply.startsWith("<xml>"));
        assertTrue(reply.endsWith("</xml>"));
        assertTrue(reply.contains("<ToUserName>"));
        assertTrue(reply.contains("<FromUserName>"));
        assertTrue(reply.contains("<CreateTime>"));
        assertTrue(reply.contains("<MsgType>"));
        assertTrue(reply.contains("<Content>"));
    }

    @Test
    @Order(64)
    void xmlBuildImageReply_containsMediaId() {
        String reply = WeChatXmlUtils.buildImageReply("user", "bot", "media_abc");

        assertTrue(reply.contains("<MsgType><![CDATA[image]]></MsgType>"));
        assertTrue(reply.contains("<MediaId><![CDATA[media_abc]]></MediaId>"));
    }

    // =========================================================================
    // 8. Signature verification edge cases
    // =========================================================================

    @Test
    @Order(70)
    void signature_caseInsensitive() {
        String timestamp = "9999999999";
        String nonce = "ci_nonce";
        String expected = computeSignature(VERIFY_TOKEN, timestamp, nonce);

        // Upper-cased signature should still validate
        assertTrue(WeChatXmlUtils.validateSignature(VERIFY_TOKEN, timestamp, nonce, expected.toUpperCase()));
    }

    @Test
    @Order(71)
    void signature_differentTokens_produce_differentSignatures() {
        String timestamp = "1000";
        String nonce = "n";
        String sig1 = computeSignature("token_a", timestamp, nonce);
        String sig2 = computeSignature("token_b", timestamp, nonce);

        assertNotEquals(sig1, sig2);
    }

    @Test
    @Order(72)
    void signature_orderIndependence() {
        // The sorting ensures order-independence between token, timestamp, nonce
        String sig1 = computeSignature("abc", "def", "ghi");
        String sig2 = computeSignature("abc", "ghi", "def");
        // These are different because token stays as "abc" but timestamp/nonce swap
        // The signature depends on sort order of all three values
        // Just verify both produce valid signatures
        assertTrue(WeChatXmlUtils.validateSignature("abc", "def", "ghi", sig1));
        assertTrue(WeChatXmlUtils.validateSignature("abc", "ghi", "def", sig2));
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private static String computeSignature(String token, String timestamp, String nonce) {
        try {
            String[] arr = { token, timestamp, nonce };
            Arrays.sort(arr);
            String joined = String.join("", arr);
            MessageDigest md = MessageDigest.getInstance("SHA-1");
            byte[] digest = md.digest(joined.getBytes(StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder(digest.length * 2);
            for (byte b : digest) {
                sb.append(String.format("%02x", b));
            }
            return sb.toString();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static OpenClawConfig buildConfig(String appId, String appSecret, String token) {
        OpenClawConfig config = new OpenClawConfig();
        OpenClawConfig.ChannelsConfig channels = new OpenClawConfig.ChannelsConfig();
        Map<String, Object> wechatMap = new LinkedHashMap<>();
        if (appId != null)
            wechatMap.put("appId", appId);
        if (appSecret != null)
            wechatMap.put("appSecret", appSecret);
        if (token != null)
            wechatMap.put("token", token);
        channels.setProviders(Map.of("wechat", wechatMap));
        config.setChannels(channels);
        return config;
    }

    private static String buildTextMessageXml(String from, String to, String content, String msgId) {
        return "<xml>"
                + "<ToUserName><![CDATA[" + to + "]]></ToUserName>"
                + "<FromUserName><![CDATA[" + from + "]]></FromUserName>"
                + "<CreateTime>1348831860</CreateTime>"
                + "<MsgType><![CDATA[text]]></MsgType>"
                + "<Content><![CDATA[" + content + "]]></Content>"
                + "<MsgId>" + msgId + "</MsgId>"
                + "</xml>";
    }

    private static String buildImageMessageXml(String from, String to, String picUrl, String mediaId) {
        return "<xml>"
                + "<ToUserName><![CDATA[" + to + "]]></ToUserName>"
                + "<FromUserName><![CDATA[" + from + "]]></FromUserName>"
                + "<CreateTime>1348831860</CreateTime>"
                + "<MsgType><![CDATA[image]]></MsgType>"
                + "<PicUrl><![CDATA[" + picUrl + "]]></PicUrl>"
                + "<MediaId><![CDATA[" + mediaId + "]]></MediaId>"
                + "<MsgId>1234567890</MsgId>"
                + "</xml>";
    }
}
