package com.openclaw.channel.wechat;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.channel.adapter.ChannelOutboundAdapter;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;

import java.io.IOException;
import java.time.Duration;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * WeChat outbound adapter using the Customer Service Message API (客服消息接口).
 * <p>
 * Requires a verified Service Account (服务号).
 * Docs:
 * https://developers.weixin.qq.com/doc/offiaccount/Message_Management/Service_Center_messages.html
 */
@Slf4j
public class WeChatOutboundAdapter implements ChannelOutboundAdapter {

    private static final String SEND_URL = "https://api.weixin.qq.com/cgi-bin/message/custom/send?access_token=%s";

    private final WeChatAccessToken accessTokenManager;
    private final OkHttpClient httpClient;
    private final ObjectMapper objectMapper;

    public WeChatOutboundAdapter(String appId, String appSecret) {
        this.accessTokenManager = new WeChatAccessToken(appId, appSecret);
        this.objectMapper = new ObjectMapper();
        this.httpClient = new OkHttpClient.Builder()
                .connectTimeout(Duration.ofSeconds(10))
                .readTimeout(Duration.ofSeconds(30))
                .build();
    }

    /**
     * Constructor for testing — allows injecting a custom access token manager and
     * HTTP client.
     */
    WeChatOutboundAdapter(WeChatAccessToken accessTokenManager, OkHttpClient httpClient) {
        this.accessTokenManager = accessTokenManager;
        this.objectMapper = new ObjectMapper();
        this.httpClient = httpClient;
    }

    @Override
    public String getChannelId() {
        return "wechat";
    }

    @Override
    public DeliveryMode getDeliveryMode() {
        return DeliveryMode.DIRECT;
    }

    @Override
    public CompletableFuture<Void> sendText(OutboundTextPayload payload) {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("touser", payload.getTarget());
        body.put("msgtype", "text");
        body.put("text", Map.of("content", payload.getText()));

        return callSendApi(body);
    }

    @Override
    public CompletableFuture<Void> sendMedia(OutboundMediaPayload payload) {
        // For WeChat, media sending via customer service API requires a media_id
        // obtained from the material upload API. For now, we send the media URL
        // as a text link. Full media upload support can be added later.
        String text = payload.getCaption() != null
                ? payload.getCaption() + "\n" + payload.getMediaUrl()
                : payload.getMediaUrl();

        Map<String, Object> body = new LinkedHashMap<>();
        body.put("touser", payload.getTarget());
        body.put("msgtype", "text");
        body.put("text", Map.of("content", text));

        return callSendApi(body);
    }

    private CompletableFuture<Void> callSendApi(Map<String, Object> body) {
        CompletableFuture<Void> future = new CompletableFuture<>();

        String token = accessTokenManager.getAccessToken();
        if (token == null) {
            future.completeExceptionally(new RuntimeException("Failed to obtain WeChat access_token"));
            return future;
        }

        try {
            String url = String.format(SEND_URL, token);
            String json = objectMapper.writeValueAsString(body);

            Request request = new Request.Builder()
                    .url(url)
                    .post(RequestBody.create(json, MediaType.parse("application/json; charset=utf-8")))
                    .build();

            httpClient.newCall(request).enqueue(new Callback() {
                @Override
                public void onFailure(Call call, IOException e) {
                    log.error("WeChat send API call failed: {}", e.getMessage());
                    future.completeExceptionally(e);
                }

                @Override
                public void onResponse(Call call, Response response) throws IOException {
                    try (ResponseBody responseBody = response.body()) {
                        String respStr = responseBody != null ? responseBody.string() : "";
                        if (!response.isSuccessful()) {
                            log.error("WeChat send API HTTP error {}: {}", response.code(), respStr);
                            future.completeExceptionally(
                                    new RuntimeException("WeChat HTTP error " + response.code() + ": " + respStr));
                        } else if (respStr.contains("\"errcode\":0") || respStr.contains("\"errcode\": 0")) {
                            future.complete(null);
                        } else {
                            // Errcode != 0 means WeChat-level error
                            log.error("WeChat send API error: {}", respStr);
                            // If token expired (40001/42001), invalidate and retry once
                            if (respStr.contains("\"errcode\":40001") || respStr.contains("\"errcode\":42001")) {
                                accessTokenManager.invalidate();
                            }
                            future.completeExceptionally(
                                    new RuntimeException("WeChat send error: " + respStr));
                        }
                    }
                }
            });
        } catch (Exception e) {
            future.completeExceptionally(e);
        }

        return future;
    }
}
