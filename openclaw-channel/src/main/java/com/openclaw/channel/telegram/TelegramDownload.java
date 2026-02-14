package com.openclaw.channel.telegram;

import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.*;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;

/**
 * Telegram file download utilities.
 * Downloads files from Telegram servers to local storage.
 * Corresponds to TypeScript's telegram/download.ts.
 */
@Slf4j
public class TelegramDownload {

    private static final HttpClient HTTP_CLIENT = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(30))
            .build();

    @Data
    @Builder
    public static class TelegramFileInfo {
        private String fileId;
        private String fileUniqueId;
        private Long fileSize;
        private String filePath;
    }

    @Data
    @Builder
    public static class SavedMedia {
        private String path;
        private String contentType;
        private long size;
    }

    /**
     * Get file info from Telegram API.
     */
    public static TelegramFileInfo getTelegramFile(String token, String fileId) {
        return getTelegramFile(token, fileId, 30_000);
    }

    /**
     * Get file info with timeout.
     */
    public static TelegramFileInfo getTelegramFile(String token, String fileId, int timeoutMs) {
        String json = "{\"file_id\":\"" + fileId + "\"}";
        String response = TelegramFetch.callApi(token, "getFile", json, timeoutMs);
        if (response == null)
            return null;

        // Parse minimal JSON response
        // In production, use Jackson. For now, extract file_path.
        String filePath = extractJsonString(response, "file_path");
        if (filePath == null) {
            log.warn("getFile returned no file_path for fileId: {}", fileId);
            return null;
        }

        return TelegramFileInfo.builder()
                .fileId(fileId)
                .filePath(filePath)
                .build();
    }

    /**
     * Download a file from Telegram servers.
     */
    public static SavedMedia downloadTelegramFile(
            String token, TelegramFileInfo info, Long maxBytes, String saveDir) {

        if (info == null || info.getFilePath() == null) {
            throw new IllegalArgumentException("file_path missing");
        }

        String url = "https://api.telegram.org/file/bot" + token + "/" + info.getFilePath();

        try {
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .timeout(Duration.ofSeconds(60))
                    .GET()
                    .build();

            HttpResponse<byte[]> response = HTTP_CLIENT.send(
                    request, HttpResponse.BodyHandlers.ofByteArray());

            if (response.statusCode() != 200) {
                throw new IOException("Failed to download: HTTP " + response.statusCode());
            }

            byte[] data = response.body();
            if (maxBytes != null && data.length > maxBytes) {
                throw new IOException("File too large: " + data.length + " > " + maxBytes);
            }

            // Detect content type
            String contentType = response.headers().firstValue("content-type")
                    .orElse(detectContentType(info.getFilePath()));

            // Save to disk
            String filename = Path.of(info.getFilePath()).getFileName().toString();
            Path savePath = Path.of(saveDir != null ? saveDir : System.getProperty("java.io.tmpdir"),
                    "telegram-downloads", filename);
            Files.createDirectories(savePath.getParent());
            Files.write(savePath, data);

            return SavedMedia.builder()
                    .path(savePath.toString())
                    .contentType(contentType)
                    .size(data.length)
                    .build();

        } catch (IOException | InterruptedException e) {
            log.error("Failed to download file from Telegram: {}", e.getMessage());
            if (e instanceof InterruptedException) {
                Thread.currentThread().interrupt();
            }
            throw new RuntimeException("Telegram file download failed", e);
        }
    }

    private static String detectContentType(String filePath) {
        if (filePath == null)
            return "application/octet-stream";
        String lower = filePath.toLowerCase();
        if (lower.endsWith(".jpg") || lower.endsWith(".jpeg"))
            return "image/jpeg";
        if (lower.endsWith(".png"))
            return "image/png";
        if (lower.endsWith(".gif"))
            return "image/gif";
        if (lower.endsWith(".webp"))
            return "image/webp";
        if (lower.endsWith(".mp4"))
            return "video/mp4";
        if (lower.endsWith(".ogg") || lower.endsWith(".oga"))
            return "audio/ogg";
        if (lower.endsWith(".mp3"))
            return "audio/mpeg";
        if (lower.endsWith(".pdf"))
            return "application/pdf";
        return "application/octet-stream";
    }

    private static String extractJsonString(String json, String key) {
        String search = "\"" + key + "\":\"";
        int idx = json.indexOf(search);
        if (idx < 0)
            return null;
        int start = idx + search.length();
        int end = json.indexOf("\"", start);
        if (end < 0)
            return null;
        return json.substring(start, end);
    }
}
