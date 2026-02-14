package com.openclaw.channel.wechat;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import lombok.extern.slf4j.Slf4j;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;

import java.io.IOException;
import java.time.Duration;
import java.util.concurrent.TimeUnit;

/**
 * WeChat access_token management with Caffeine caching.
 * <p>
 * The access_token is valid for 7200 seconds (2 hours).
 * We cache it for 110 minutes to allow a comfortable refresh margin.
 */
@Slf4j
public class WeChatAccessToken {

    private static final String TOKEN_URL = "https://api.weixin.qq.com/cgi-bin/token"
            + "?grant_type=client_credential&appid=%s&secret=%s";

    private final String appId;
    private final String appSecret;
    private final OkHttpClient httpClient;
    private final ObjectMapper objectMapper;

    /** Cache keyed by appId → access_token string. */
    private final Cache<String, String> tokenCache;

    public WeChatAccessToken(String appId, String appSecret) {
        this(appId, appSecret, new OkHttpClient.Builder()
                .connectTimeout(Duration.ofSeconds(10))
                .readTimeout(Duration.ofSeconds(10))
                .build());
    }

    /** Constructor for testing – allows injecting a custom OkHttpClient. */
    WeChatAccessToken(String appId, String appSecret, OkHttpClient httpClient) {
        this.appId = appId;
        this.appSecret = appSecret;
        this.httpClient = httpClient;
        this.objectMapper = new ObjectMapper();
        this.tokenCache = Caffeine.newBuilder()
                .expireAfterWrite(110, TimeUnit.MINUTES)
                .maximumSize(8)
                .build();
    }

    /**
     * Get a valid access_token, fetching from WeChat API if not cached.
     */
    public String getAccessToken() {
        String cached = tokenCache.getIfPresent(appId);
        if (cached != null) {
            return cached;
        }
        return refreshAccessToken();
    }

    /**
     * Force-refresh the access_token from the WeChat API.
     */
    public String refreshAccessToken() {
        String url = String.format(TOKEN_URL, appId, appSecret);
        Request request = new Request.Builder().url(url).get().build();

        try (Response response = httpClient.newCall(request).execute()) {
            ResponseBody body = response.body();
            if (body == null) {
                log.error("WeChat token response body is null");
                return null;
            }
            String json = body.string();
            JsonNode node = objectMapper.readTree(json);

            if (node.has("access_token")) {
                String token = node.get("access_token").asText();
                tokenCache.put(appId, token);
                log.info("WeChat access_token refreshed successfully for appId: {}", appId);
                return token;
            } else {
                int errcode = node.has("errcode") ? node.get("errcode").asInt() : -1;
                String errmsg = node.has("errmsg") ? node.get("errmsg").asText() : "unknown";
                log.error("WeChat token error {}: {}", errcode, errmsg);
                return null;
            }
        } catch (IOException e) {
            log.error("Failed to fetch WeChat access_token: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Invalidate the cached token (e.g. when a 40001 error is received).
     */
    public void invalidate() {
        tokenCache.invalidate(appId);
    }
}
