package com.openclaw.agent.media;

import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.nio.file.Path;
import java.util.Map;

/**
 * Fetches remote media (images, audio, documents) with size limits and MIME
 * detection.
 * Corresponds to TypeScript's media/fetch.ts.
 */
@Slf4j
public final class MediaFetcher {

    private static final int CONNECT_TIMEOUT_MS = 10_000;
    private static final int READ_TIMEOUT_MS = 30_000;
    private static final int MAX_REDIRECTS = 5;
    private static final long DEFAULT_MAX_BYTES = MediaConstants.MEDIA_MAX_BYTES;

    private MediaFetcher() {
    }

    @Data
    @Builder
    public static class FetchMediaResult {
        private byte[] buffer;
        private String contentType;
        private String fileName;
    }

    public static class MediaFetchException extends RuntimeException {
        private final String code;

        public MediaFetchException(String code, String message) {
            super(message);
            this.code = code;
        }

        public MediaFetchException(String code, String message, Throwable cause) {
            super(message, cause);
            this.code = code;
        }

        public String getCode() {
            return code;
        }
    }

    /**
     * Fetch remote media from a URL with size limit and MIME detection.
     *
     * @param url          the URL to fetch
     * @param maxBytes     maximum allowed response size (default 5MB)
     * @param headers      optional request headers
     * @param maxRedirects maximum redirects to follow (default 5)
     * @return the fetched media with buffer, contentType, and fileName
     */
    public static FetchMediaResult fetchRemoteMedia(
            String url, Long maxBytes, Map<String, String> headers, int maxRedirects) {

        long limit = maxBytes != null ? maxBytes : DEFAULT_MAX_BYTES;
        String finalUrl = url;

        try {
            HttpURLConnection conn = openConnection(url, headers, maxRedirects);
            finalUrl = conn.getURL().toString();

            int status = conn.getResponseCode();
            if (status < 200 || status >= 300) {
                String detail = "HTTP " + status
                        + (conn.getResponseMessage() != null ? " " + conn.getResponseMessage() : "");
                String redirected = !finalUrl.equals(url) ? " (redirected to " + finalUrl + ")" : "";
                throw new MediaFetchException("http_error",
                        "Failed to fetch media from " + url + redirected + ": " + detail);
            }

            // Check content-length upfront
            long contentLength = conn.getContentLengthLong();
            if (contentLength > 0 && contentLength > limit) {
                throw new MediaFetchException("max_bytes",
                        "Failed to fetch media from " + url + ": content length " + contentLength + " exceeds maxBytes "
                                + limit);
            }

            // Read body with limit
            byte[] buffer = readWithLimit(conn.getInputStream(), limit, url);

            // Resolve file name
            String fileNameFromUrl = extractFileNameFromUrl(finalUrl);
            String headerFileName = parseContentDispositionFileName(conn.getHeaderField("Content-Disposition"));
            String fileName = headerFileName != null ? headerFileName : fileNameFromUrl;

            // Detect MIME
            String headerMime = conn.getContentType();
            String filePathForMime = headerFileName != null && hasExtension(headerFileName) ? headerFileName : finalUrl;
            String contentType = MimeDetector.detectMime(buffer, headerMime, filePathForMime);

            // Append extension if missing
            if (fileName != null && !hasExtension(fileName) && contentType != null) {
                String ext = MimeDetector.extensionForMime(contentType);
                if (ext != null)
                    fileName = fileName + ext;
            }

            return FetchMediaResult.builder()
                    .buffer(buffer)
                    .contentType(contentType)
                    .fileName(fileName)
                    .build();

        } catch (MediaFetchException e) {
            throw e;
        } catch (Exception e) {
            throw new MediaFetchException("fetch_failed",
                    "Failed to fetch media from " + url + ": " + e.getMessage(), e);
        }
    }

    /**
     * Overload with default maxRedirects.
     */
    public static FetchMediaResult fetchRemoteMedia(String url, Long maxBytes, Map<String, String> headers) {
        return fetchRemoteMedia(url, maxBytes, headers, MAX_REDIRECTS);
    }

    /**
     * Overload with defaults.
     */
    public static FetchMediaResult fetchRemoteMedia(String url) {
        return fetchRemoteMedia(url, null, null, MAX_REDIRECTS);
    }

    // --- Internal helpers ---

    private static HttpURLConnection openConnection(String urlStr, Map<String, String> headers, int maxRedirects)
            throws IOException {
        HttpURLConnection conn = null;
        String currentUrl = urlStr;

        for (int i = 0; i <= maxRedirects; i++) {
            URL url = URI.create(currentUrl).toURL();
            conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("GET");
            conn.setConnectTimeout(CONNECT_TIMEOUT_MS);
            conn.setReadTimeout(READ_TIMEOUT_MS);
            conn.setInstanceFollowRedirects(false);

            if (headers != null) {
                headers.forEach(conn::setRequestProperty);
            }

            int status = conn.getResponseCode();
            if (status == HttpURLConnection.HTTP_MOVED_PERM
                    || status == HttpURLConnection.HTTP_MOVED_TEMP
                    || status == HttpURLConnection.HTTP_SEE_OTHER
                    || status == 307 || status == 308) {
                String location = conn.getHeaderField("Location");
                if (location == null)
                    break;
                // Resolve relative redirects
                currentUrl = URI.create(currentUrl).resolve(location).toString();
                conn.disconnect();
                continue;
            }
            return conn;
        }
        return conn;
    }

    private static byte[] readWithLimit(InputStream in, long maxBytes, String url) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte[] buf = new byte[8192];
        long total = 0;
        int read;
        while ((read = in.read(buf)) != -1) {
            total += read;
            if (total > maxBytes) {
                throw new MediaFetchException("max_bytes",
                        "Failed to fetch media from " + url + ": payload exceeds maxBytes " + maxBytes);
            }
            baos.write(buf, 0, read);
        }
        return baos.toByteArray();
    }

    private static String extractFileNameFromUrl(String url) {
        try {
            URI uri = URI.create(url);
            String path = uri.getPath();
            if (path == null || path.isEmpty() || "/".equals(path))
                return null;
            String base = Path.of(path).getFileName().toString();
            return base.isEmpty() ? null : base;
        } catch (Exception e) {
            return null;
        }
    }

    static String parseContentDispositionFileName(String header) {
        if (header == null || header.isBlank())
            return null;
        // Try filename*= first (RFC 5987)
        var starMatch = java.util.regex.Pattern
                .compile("filename\\*\\s*=\\s*([^;]+)", java.util.regex.Pattern.CASE_INSENSITIVE)
                .matcher(header);
        if (starMatch.find()) {
            String cleaned = stripQuotes(starMatch.group(1).trim());
            String[] parts = cleaned.split("''", 2);
            String encoded = parts.length > 1 ? parts[1] : cleaned;
            try {
                return Path.of(java.net.URLDecoder.decode(encoded, "UTF-8")).getFileName().toString();
            } catch (Exception e) {
                return Path.of(encoded).getFileName().toString();
            }
        }
        // Try filename=
        var match = java.util.regex.Pattern
                .compile("filename\\s*=\\s*([^;]+)", java.util.regex.Pattern.CASE_INSENSITIVE)
                .matcher(header);
        if (match.find()) {
            return Path.of(stripQuotes(match.group(1).trim())).getFileName().toString();
        }
        return null;
    }

    private static String stripQuotes(String value) {
        return value.replaceAll("^[\"']|[\"']$", "");
    }

    private static boolean hasExtension(String name) {
        if (name == null)
            return false;
        int dot = name.lastIndexOf('.');
        return dot > 0 && dot < name.length() - 1;
    }
}
