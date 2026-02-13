package com.openclaw.agent.media;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URLConnection;
import java.nio.file.Path;
import java.util.*;

/**
 * MIME type detection utility.
 * Corresponds to TypeScript's media/mime.ts.
 *
 * <p>
 * Supports detection by:
 * <ul>
 * <li>File extension mapping</li>
 * <li>Content-Type header normalization</li>
 * <li>Magic byte sniffing (via URLConnection.guessContentTypeFromStream)</li>
 * </ul>
 */
public final class MimeDetector {

    private MimeDetector() {
    }

    /** Extension → MIME mapping. */
    private static final Map<String, String> MIME_BY_EXT;
    /** MIME → preferred extension mapping. */
    private static final Map<String, String> EXT_BY_MIME;
    /** Known audio file extensions. */
    private static final Set<String> AUDIO_EXTENSIONS = Set.of(
            ".aac", ".flac", ".m4a", ".mp3", ".oga", ".ogg", ".opus", ".wav");

    static {
        // Build extension ↔ mime maps
        Map<String, String> extByMime = new LinkedHashMap<>();
        extByMime.put("image/heic", ".heic");
        extByMime.put("image/heif", ".heif");
        extByMime.put("image/jpeg", ".jpg");
        extByMime.put("image/png", ".png");
        extByMime.put("image/webp", ".webp");
        extByMime.put("image/gif", ".gif");
        extByMime.put("audio/ogg", ".ogg");
        extByMime.put("audio/mpeg", ".mp3");
        extByMime.put("audio/x-m4a", ".m4a");
        extByMime.put("audio/mp4", ".m4a");
        extByMime.put("video/mp4", ".mp4");
        extByMime.put("video/quicktime", ".mov");
        extByMime.put("application/pdf", ".pdf");
        extByMime.put("application/json", ".json");
        extByMime.put("application/zip", ".zip");
        extByMime.put("application/gzip", ".gz");
        extByMime.put("application/x-tar", ".tar");
        extByMime.put("application/x-7z-compressed", ".7z");
        extByMime.put("application/vnd.rar", ".rar");
        extByMime.put("application/msword", ".doc");
        extByMime.put("application/vnd.ms-excel", ".xls");
        extByMime.put("application/vnd.ms-powerpoint", ".ppt");
        extByMime.put("application/vnd.openxmlformats-officedocument.wordprocessingml.document", ".docx");
        extByMime.put("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", ".xlsx");
        extByMime.put("application/vnd.openxmlformats-officedocument.presentationml.presentation", ".pptx");
        extByMime.put("text/csv", ".csv");
        extByMime.put("text/plain", ".txt");
        extByMime.put("text/markdown", ".md");
        EXT_BY_MIME = Map.copyOf(extByMime);

        // Invert: ext → mime
        Map<String, String> mimeByExt = new LinkedHashMap<>();
        extByMime.forEach((mime, ext) -> mimeByExt.put(ext, mime));
        mimeByExt.put(".jpeg", "image/jpeg"); // alias
        MIME_BY_EXT = Map.copyOf(mimeByExt);
    }

    /**
     * Normalize a Content-Type header to a clean MIME string.
     */
    public static String normalizeHeaderMime(String mime) {
        if (mime == null || mime.isBlank())
            return null;
        String cleaned = mime.split(";")[0].trim().toLowerCase();
        return cleaned.isEmpty() ? null : cleaned;
    }

    /**
     * Attempt to sniff a MIME type from the first bytes of a buffer.
     */
    public static String sniffMime(byte[] buffer) {
        if (buffer == null || buffer.length == 0)
            return null;
        try (InputStream is = new java.io.ByteArrayInputStream(buffer)) {
            String guessed = URLConnection.guessContentTypeFromStream(is);
            return guessed;
        } catch (IOException e) {
            return null;
        }
    }

    /**
     * Get the file extension from a file path or URL.
     */
    public static String getFileExtension(String filePath) {
        if (filePath == null || filePath.isBlank())
            return null;
        try {
            // If it looks like a URL, extract path component
            if (filePath.matches("(?i)^https?://.*")) {
                URI uri = URI.create(filePath);
                filePath = uri.getPath();
            }
        } catch (Exception ignored) {
            // fall through to plain path parsing
        }
        int dot = filePath.lastIndexOf('.');
        if (dot < 0 || dot == filePath.length() - 1)
            return null;
        // Avoid picking up dots in directory names
        int sep = Math.max(filePath.lastIndexOf('/'), filePath.lastIndexOf('\\'));
        if (dot < sep)
            return null;
        return filePath.substring(dot).toLowerCase();
    }

    /**
     * Check if a file name appears to be an audio file.
     */
    public static boolean isAudioFileName(String fileName) {
        String ext = getFileExtension(fileName);
        return ext != null && AUDIO_EXTENSIONS.contains(ext);
    }

    /**
     * Detect a MIME type from buffer bytes, header, and/or file path.
     * Priority: sniffed > extension > header (preferring specific over generic).
     */
    public static String detectMime(byte[] buffer, String headerMime, String filePath) {
        String ext = getFileExtension(filePath);
        String extMime = ext != null ? MIME_BY_EXT.get(ext) : null;
        String normalizedHeader = normalizeHeaderMime(headerMime);
        String sniffed = sniffMime(buffer);

        // Prefer sniffed, but don't let generic types override extension mapping
        if (sniffed != null && (!isGenericMime(sniffed) || extMime == null)) {
            return sniffed;
        }
        if (extMime != null)
            return extMime;
        if (normalizedHeader != null && !isGenericMime(normalizedHeader))
            return normalizedHeader;
        if (sniffed != null)
            return sniffed;
        return normalizedHeader; // may be null
    }

    /**
     * Get the preferred file extension for a MIME type.
     */
    public static String extensionForMime(String mime) {
        if (mime == null)
            return null;
        return EXT_BY_MIME.get(mime.toLowerCase());
    }

    /**
     * Check if a MIME type is generic (octet-stream or zip).
     */
    public static boolean isGenericMime(String mime) {
        if (mime == null)
            return true;
        String m = mime.toLowerCase();
        return "application/octet-stream".equals(m) || "application/zip".equals(m);
    }

    /**
     * Check if the media looks like a GIF.
     */
    public static boolean isGifMedia(String contentType, String fileName) {
        if (contentType != null && "image/gif".equalsIgnoreCase(contentType))
            return true;
        String ext = getFileExtension(fileName);
        return ".gif".equals(ext);
    }

    /**
     * Map an image format name to its MIME type.
     */
    public static String imageMimeFromFormat(String format) {
        if (format == null)
            return null;
        return switch (format.toLowerCase()) {
            case "jpg", "jpeg" -> "image/jpeg";
            case "heic" -> "image/heic";
            case "heif" -> "image/heif";
            case "png" -> "image/png";
            case "webp" -> "image/webp";
            case "gif" -> "image/gif";
            default -> null;
        };
    }

    /**
     * Get the media kind from a MIME type.
     */
    public static MediaConstants.MediaKind kindFromMime(String mime) {
        return MediaConstants.kindFromMime(mime);
    }
}
