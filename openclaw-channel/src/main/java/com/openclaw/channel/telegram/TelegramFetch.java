package com.openclaw.channel.telegram;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;

/**
 * Telegram API fetch utilities and network configuration.
 * Resolves fetch implementation with optional proxy support.
 * Corresponds to TypeScript's telegram/fetch.ts.
 */
@Slf4j
public class TelegramFetch {

    private static final HttpClient HTTP_CLIENT = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(30))
            .build();

    /**
     * Call a Telegram Bot API method.
     */
    public static String callApi(String token, String method, String jsonBody) {
        return callApi(token, method, jsonBody, 30_000);
    }

    /**
     * Call a Telegram Bot API method with a timeout.
     */
    public static String callApi(String token, String method, String jsonBody, int timeoutMs) {
        String url = "https://api.telegram.org/bot" + token + "/" + method;

        try {
            HttpRequest.Builder builder = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .timeout(Duration.ofMillis(timeoutMs))
                    .header("Content-Type", "application/json");

            if (jsonBody != null && !jsonBody.isBlank()) {
                builder.POST(HttpRequest.BodyPublishers.ofString(jsonBody));
            } else {
                builder.GET();
            }

            HttpResponse<String> response = HTTP_CLIENT.send(
                    builder.build(), HttpResponse.BodyHandlers.ofString());

            if (response.statusCode() != 200) {
                log.warn("Telegram API error: {} {} -> HTTP {}",
                        method, url, response.statusCode());
            }

            return response.body();
        } catch (IOException | InterruptedException e) {
            log.error("Telegram API call failed: {} {}: {}",
                    method, url, e.getMessage());
            if (e instanceof InterruptedException) {
                Thread.currentThread().interrupt();
            }
            return null;
        }
    }

    /**
     * Get updates from Telegram (long polling).
     */
    public static String getUpdates(String token, Integer offset, int timeoutSeconds) {
        StringBuilder json = new StringBuilder("{");
        if (offset != null)
            json.append("\"offset\":").append(offset).append(",");
        json.append("\"timeout\":").append(timeoutSeconds);
        json.append("}");

        return callApi(token, "getUpdates", json.toString(),
                (timeoutSeconds + 5) * 1000);
    }

    /**
     * Get bot info (getMe).
     */
    public static String getMe(String token) {
        return callApi(token, "getMe", null);
    }

    /**
     * Call a Telegram Bot API method with multipart/form-data body.
     * Used for file uploads (sendPhoto, sendDocument, etc.).
     *
     * @param token         Bot token
     * @param method        API method name (e.g. "sendPhoto")
     * @param textFields    text form fields (key → value)
     * @param fileFieldName name of the file field (e.g. "photo")
     * @param fileBytes     file content bytes
     * @param fileName      file name (e.g. "screenshot.png")
     * @param fileMimeType  MIME type (e.g. "image/png")
     */
    public static String callApiMultipart(
            String token,
            String method,
            java.util.Map<String, String> textFields,
            String fileFieldName,
            byte[] fileBytes,
            String fileName,
            String fileMimeType) {

        String url = "https://api.telegram.org/bot" + token + "/" + method;
        String boundary = "----OpenClawBoundary" + System.currentTimeMillis();

        try {
            // Build multipart body
            var baos = new java.io.ByteArrayOutputStream();

            // Text fields
            for (var entry : textFields.entrySet()) {
                baos.write(("--" + boundary + "\r\n").getBytes());
                baos.write(("Content-Disposition: form-data; name=\"" + entry.getKey() + "\"\r\n\r\n").getBytes());
                baos.write(entry.getValue().getBytes(java.nio.charset.StandardCharsets.UTF_8));
                baos.write("\r\n".getBytes());
            }

            // File field
            baos.write(("--" + boundary + "\r\n").getBytes());
            baos.write(("Content-Disposition: form-data; name=\"" + fileFieldName
                    + "\"; filename=\"" + fileName + "\"\r\n").getBytes());
            baos.write(("Content-Type: " + fileMimeType + "\r\n\r\n").getBytes());
            baos.write(fileBytes);
            baos.write("\r\n".getBytes());

            // End boundary
            baos.write(("--" + boundary + "--\r\n").getBytes());

            byte[] body = baos.toByteArray();

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .timeout(Duration.ofSeconds(60))
                    .header("Content-Type", "multipart/form-data; boundary=" + boundary)
                    .POST(HttpRequest.BodyPublishers.ofByteArray(body))
                    .build();

            HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());

            if (response.statusCode() != 200) {
                log.warn("Telegram API multipart error: {} -> HTTP {} body={}",
                        method, response.statusCode(), response.body());
            }

            return response.body();
        } catch (IOException | InterruptedException e) {
            log.error("Telegram API multipart call failed: {}: {}", method, e.getMessage());
            if (e instanceof InterruptedException) {
                Thread.currentThread().interrupt();
            }
            return null;
        }
    }
}
