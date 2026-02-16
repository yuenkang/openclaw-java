package com.openclaw.agent.providers;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import okhttp3.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

/**
 * Qwen Portal OAuth — refresh-token grant for 通义千问 Portal API access.
 * Translates TS providers/qwen-portal-oauth.ts.
 *
 */
public class QwenPortalOAuth {

    private static final Logger log = LoggerFactory.getLogger(QwenPortalOAuth.class);
    private static final ObjectMapper MAPPER = new ObjectMapper();

    // -----------------------------------------------------------------------
    // Constants
    // -----------------------------------------------------------------------

    private static final String QWEN_OAUTH_BASE_URL = "https://chat.qwen.ai";
    private static final String QWEN_OAUTH_TOKEN_ENDPOINT = QWEN_OAUTH_BASE_URL + "/api/v1/oauth2/token";
    private static final String QWEN_OAUTH_CLIENT_ID = "f0304373b74a44d2b584a3fb70ca9e56";

    private final OkHttpClient client;

    public QwenPortalOAuth(OkHttpClient client) {
        this.client = client;
    }

    public QwenPortalOAuth() {
        this(new OkHttpClient.Builder()
                .connectTimeout(15, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .build());
    }

    // -----------------------------------------------------------------------
    // Types
    // -----------------------------------------------------------------------

    /**
     * OAuth credentials for Qwen Portal.
     */
    public record QwenCredentials(
            String access,
            String refresh,
            long expires) {
    }

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /**
     * Refresh Qwen Portal OAuth credentials using a refresh token.
     *
     * @param credentials current credentials (must contain a valid refresh token)
     * @return new credentials with updated access token and expiry
     */
    public QwenCredentials refreshCredentials(QwenCredentials credentials) throws IOException {
        if (credentials.refresh() == null || credentials.refresh().isBlank()) {
            throw new IOException("Qwen OAuth refresh token missing; re-authenticate.");
        }

        RequestBody body = new FormBody.Builder()
                .add("grant_type", "refresh_token")
                .add("refresh_token", credentials.refresh())
                .add("client_id", QWEN_OAUTH_CLIENT_ID)
                .build();

        Request request = new Request.Builder()
                .url(QWEN_OAUTH_TOKEN_ENDPOINT)
                .post(body)
                .addHeader("Content-Type", "application/x-www-form-urlencoded")
                .addHeader("Accept", "application/json")
                .build();

        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                String text = response.body() != null ? response.body().string() : "";
                if (response.code() == 400) {
                    throw new IOException(
                            "Qwen OAuth refresh token expired or invalid. " +
                                    "Re-authenticate with: openclaw models auth login --provider qwen-portal");
                }
                throw new IOException("Qwen OAuth refresh failed: " +
                        (text.isEmpty() ? response.message() : text));
            }

            JsonNode json = MAPPER.readTree(response.body().string());
            String accessToken = json.path("access_token").asText();
            int expiresIn = json.path("expires_in").asInt(0);

            if (accessToken.isBlank() || expiresIn == 0) {
                throw new IOException("Qwen OAuth refresh response missing access token.");
            }

            String newRefresh = json.path("refresh_token").asText();
            if (newRefresh.isBlank()) {
                newRefresh = credentials.refresh();
            }

            return new QwenCredentials(
                    accessToken,
                    newRefresh,
                    System.currentTimeMillis() + (long) expiresIn * 1000);
        }
    }
}
