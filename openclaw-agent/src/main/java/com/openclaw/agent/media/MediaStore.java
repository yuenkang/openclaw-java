package com.openclaw.agent.media;

import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.util.Map;
import java.util.UUID;

/**
 * Local media file storage — download from URL or save from buffer.
 * Corresponds to TypeScript's media/store.ts.
 *
 * <p>
 * Media files are saved to {@code ~/.openclaw/media/} with an auto-generated
 * unique filename. Old files are cleaned up based on TTL.
 * </p>
 */
@Slf4j
public final class MediaStore {

    private static final long MAX_BYTES = MediaConstants.MEDIA_MAX_BYTES;
    private static final long DEFAULT_TTL_MS = MediaConstants.DEFAULT_TTL_MS;

    private MediaStore() {
    }

    @Data
    @Builder
    public static class SavedMedia {
        private String id;
        private String path;
        private long size;
        private String contentType;
    }

    /**
     * Resolve the default media storage directory.
     */
    public static Path getMediaDir() {
        String configDir = System.getProperty("user.home") + "/.openclaw";
        return Path.of(configDir, "media");
    }

    /**
     * Ensure the media directory exists.
     */
    public static Path ensureMediaDir() throws IOException {
        return ensureMediaDir(null);
    }

    /**
     * Ensure a media subdirectory exists (e.g. "inbound").
     */
    public static Path ensureMediaDir(String subdir) throws IOException {
        Path dir = subdir != null ? getMediaDir().resolve(subdir) : getMediaDir();
        Files.createDirectories(dir);
        return dir;
    }

    /**
     * Clean old media files exceeding the TTL.
     */
    public static void cleanOldMedia(long ttlMs) {
        Path dir = getMediaDir();
        if (!Files.isDirectory(dir))
            return;

        long cutoff = Instant.now().toEpochMilli() - ttlMs;
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(dir)) {
            for (Path entry : stream) {
                if (!Files.isRegularFile(entry))
                    continue;
                try {
                    BasicFileAttributes attrs = Files.readAttributes(entry, BasicFileAttributes.class);
                    if (attrs.lastModifiedTime().toMillis() < cutoff) {
                        Files.deleteIfExists(entry);
                        log.debug("Cleaned old media: {}", entry.getFileName());
                    }
                } catch (IOException e) {
                    log.trace("Skip cleanup for {}: {}", entry, e.getMessage());
                }
            }
        } catch (IOException e) {
            log.debug("Could not clean media dir: {}", e.getMessage());
        }
    }

    /**
     * Clean old media with default TTL (2 minutes).
     */
    public static void cleanOldMedia() {
        cleanOldMedia(DEFAULT_TTL_MS);
    }

    /**
     * Save media from a URL or local file path.
     *
     * <p>
     * If the source looks like a URL, it's downloaded. Otherwise, it's
     * treated as a local file path and copied.
     * </p>
     *
     * @param source  URL or local file path
     * @param headers optional HTTP headers for URL fetch
     * @param subdir  sub-directory under media dir (default: "")
     * @return saved media info
     */
    public static SavedMedia saveMediaSource(String source, Map<String, String> headers, String subdir)
            throws IOException {
        if (looksLikeUrl(source)) {
            MediaFetcher.FetchMediaResult fetched = MediaFetcher.fetchRemoteMedia(source, MAX_BYTES, headers);
            return saveMediaBuffer(fetched.getBuffer(), fetched.getContentType(), subdir, MAX_BYTES,
                    fetched.getFileName());
        }

        // Local file
        Path sourcePath = Path.of(source);
        if (!Files.exists(sourcePath)) {
            throw new IOException("Source file not found: " + source);
        }
        byte[] data = Files.readAllBytes(sourcePath);
        if (data.length > MAX_BYTES) {
            throw new IOException("File exceeds max size: " + data.length + " > " + MAX_BYTES);
        }
        String contentType = MimeDetector.detectMime(data, null, source);
        return saveMediaBuffer(data, contentType, subdir, MAX_BYTES, sourcePath.getFileName().toString());
    }

    /**
     * Save media from a byte buffer.
     *
     * @param buffer           raw media bytes
     * @param contentType      optional known content type
     * @param subdir           sub-directory (default: "inbound")
     * @param maxBytes         max allowed size
     * @param originalFilename optional original filename hint
     * @return saved media info
     */
    public static SavedMedia saveMediaBuffer(
            byte[] buffer, String contentType, String subdir, long maxBytes, String originalFilename)
            throws IOException {

        if (buffer.length > maxBytes) {
            throw new IOException("Buffer exceeds max size: " + buffer.length + " > " + maxBytes);
        }

        String effectiveSubdir = subdir != null ? subdir : "inbound";
        Path dir = ensureMediaDir(effectiveSubdir);

        String id = UUID.randomUUID().toString();

        // Build filename: {sanitized-original}---{uuid}.{ext}
        String ext = null;
        if (contentType != null)
            ext = MimeDetector.extensionForMime(contentType);
        if (ext == null && originalFilename != null)
            ext = MimeDetector.getFileExtension(originalFilename);
        if (ext == null)
            ext = ".bin";

        String safeName = originalFilename != null
                ? sanitizeFilename(stripExtension(originalFilename))
                : "file";
        if (safeName.length() > 60)
            safeName = safeName.substring(0, 60);

        String fileName = safeName + "---" + id.substring(0, 8) + ext;
        Path dest = dir.resolve(fileName);

        Files.write(dest, buffer);
        log.debug("Saved media: {} ({} bytes, {})", dest.getFileName(), buffer.length, contentType);

        return SavedMedia.builder()
                .id(id)
                .path(dest.toAbsolutePath().toString())
                .size(buffer.length)
                .contentType(contentType)
                .build();
    }

    /**
     * Extract the original filename from a stored media path.
     * Pattern: {original}---{uuid}.{ext} → "original.ext"
     */
    public static String extractOriginalFilename(String filePath) {
        if (filePath == null)
            return "file.bin";
        String base = Path.of(filePath).getFileName().toString();
        int sep = base.indexOf("---");
        if (sep < 0)
            return base.isEmpty() ? "file.bin" : base;
        String original = base.substring(0, sep);
        int dot = base.lastIndexOf('.');
        String ext = dot > sep ? base.substring(dot) : "";
        String result = original + ext;
        return result.isEmpty() ? "file.bin" : result;
    }

    /**
     * Check if a string looks like a URL.
     */
    public static boolean looksLikeUrl(String src) {
        return src != null && (src.startsWith("http://") || src.startsWith("https://"));
    }

    /**
     * Sanitize a filename for cross-platform safety.
     */
    public static String sanitizeFilename(String name) {
        if (name == null || name.isBlank())
            return "file";
        // Remove characters unsafe on Windows/SharePoint
        return name.replaceAll("[<>:\"/\\\\|?*\\x00-\\x1f]", "_").trim();
    }

    private static String stripExtension(String name) {
        if (name == null)
            return null;
        int dot = name.lastIndexOf('.');
        return dot > 0 ? name.substring(0, dot) : name;
    }
}
