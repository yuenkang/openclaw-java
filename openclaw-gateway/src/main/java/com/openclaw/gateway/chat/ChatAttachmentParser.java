package com.openclaw.gateway.chat;

import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Chat attachment parser — extracts images from base64 attachments.
 * Corresponds to TypeScript's chat-attachments.ts.
 */
public final class ChatAttachmentParser {

    private ChatAttachmentParser() {
    }

    private static final int DEFAULT_MAX_BYTES = 5_000_000; // 5 MB
    private static final Pattern DATA_URL_PATTERN = Pattern.compile("^data:[^;]+;base64,(.*)$");
    private static final Pattern BASE64_CHARSET = Pattern.compile("^[A-Za-z0-9+/=]+$");

    // =========================================================================
    // Types
    // =========================================================================

    public record ChatAttachment(
            String type,
            String mimeType,
            String fileName,
            Object content) {
    }

    public record ChatImageContent(
            String data,
            String mimeType) {
    }

    public record ParsedMessageWithImages(
            String message,
            List<ChatImageContent> images) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Parse attachments and extract images as structured content blocks.
     */
    public static ParsedMessageWithImages parseMessageWithAttachments(
            String message, List<ChatAttachment> attachments) {
        return parseMessageWithAttachments(message, attachments, DEFAULT_MAX_BYTES);
    }

    public static ParsedMessageWithImages parseMessageWithAttachments(
            String message, List<ChatAttachment> attachments, int maxBytes) {
        if (attachments == null || attachments.isEmpty()) {
            return new ParsedMessageWithImages(message, List.of());
        }

        List<ChatImageContent> images = new ArrayList<>();

        for (int idx = 0; idx < attachments.size(); idx++) {
            ChatAttachment att = attachments.get(idx);
            if (att == null)
                continue;

            String label = att.fileName() != null ? att.fileName()
                    : att.type() != null ? att.type()
                            : "attachment-" + (idx + 1);

            if (!(att.content() instanceof String contentStr)) {
                throw new IllegalArgumentException("attachment " + label + ": content must be base64 string");
            }

            String b64 = contentStr.trim();
            // Strip data URL prefix
            var dataUrlMatch = DATA_URL_PATTERN.matcher(b64);
            if (dataUrlMatch.matches()) {
                b64 = dataUrlMatch.group(1);
            }

            // Base64 sanity checks
            if (b64.length() % 4 != 0 || !BASE64_CHARSET.matcher(b64).matches()) {
                throw new IllegalArgumentException("attachment " + label + ": invalid base64 content");
            }

            byte[] decoded;
            try {
                decoded = Base64.getDecoder().decode(b64);
            } catch (Exception e) {
                throw new IllegalArgumentException("attachment " + label + ": invalid base64 content");
            }

            int sizeBytes = decoded.length;
            if (sizeBytes <= 0 || sizeBytes > maxBytes) {
                throw new IllegalArgumentException(
                        "attachment " + label + ": exceeds size limit (" + sizeBytes + " > " + maxBytes + " bytes)");
            }

            // Determine MIME type
            String providedMime = normalizeMime(att.mimeType());
            String sniffedMime = sniffMimeFromBytes(decoded);

            if (sniffedMime != null && !isImageMime(sniffedMime)) {
                continue; // non-image, skip
            }
            if (sniffedMime == null && !isImageMime(providedMime)) {
                continue; // can't detect, skip
            }

            String finalMime = sniffedMime != null ? sniffedMime
                    : providedMime != null ? providedMime
                            : att.mimeType();

            images.add(new ChatImageContent(b64, finalMime));
        }

        return new ParsedMessageWithImages(message, images);
    }

    // =========================================================================
    // Internal helpers
    // =========================================================================

    static String normalizeMime(String mime) {
        if (mime == null || mime.isEmpty())
            return null;
        String cleaned = mime.split(";")[0].trim().toLowerCase();
        return cleaned.isEmpty() ? null : cleaned;
    }

    static boolean isImageMime(String mime) {
        return mime != null && mime.startsWith("image/");
    }

    /**
     * Sniff MIME type from file magic bytes.
     */
    static String sniffMimeFromBytes(byte[] data) {
        if (data.length < 4)
            return null;

        // PNG: 89 50 4E 47
        if (data[0] == (byte) 0x89 && data[1] == 0x50 && data[2] == 0x4E && data[3] == 0x47) {
            return "image/png";
        }
        // JPEG: FF D8 FF
        if (data[0] == (byte) 0xFF && data[1] == (byte) 0xD8 && data[2] == (byte) 0xFF) {
            return "image/jpeg";
        }
        // GIF: 47 49 46
        if (data[0] == 0x47 && data[1] == 0x49 && data[2] == 0x46) {
            return "image/gif";
        }
        // WebP: RIFF....WEBP
        if (data.length >= 12 && data[0] == 0x52 && data[1] == 0x49 && data[2] == 0x46 && data[3] == 0x46
                && data[8] == 0x57 && data[9] == 0x45 && data[10] == 0x42 && data[11] == 0x50) {
            return "image/webp";
        }
        // BMP: 42 4D
        if (data[0] == 0x42 && data[1] == 0x4D) {
            return "image/bmp";
        }
        // SVG: starts with < (likely XML)
        if (data[0] == 0x3C) {
            String head = new String(data, 0, Math.min(256, data.length));
            if (head.contains("<svg")) {
                return "image/svg+xml";
            }
        }

        return null;
    }
}
