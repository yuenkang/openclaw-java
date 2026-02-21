package com.openclaw.media;

/**
 * Media size limits and kind classification.
 * Corresponds to TypeScript's media/constants.ts.
 */
public final class MediaConstants {

    public static final long MAX_IMAGE_BYTES = 6L * 1024 * 1024; // 6MB
    public static final long MAX_AUDIO_BYTES = 16L * 1024 * 1024; // 16MB
    public static final long MAX_VIDEO_BYTES = 16L * 1024 * 1024; // 16MB
    public static final long MAX_DOCUMENT_BYTES = 100L * 1024 * 1024; // 100MB
    public static final long MEDIA_MAX_BYTES = 5L * 1024 * 1024; // 5MB default fetch limit
    public static final long DEFAULT_TTL_MS = 2L * 60 * 1000; // 2 minutes

    private MediaConstants() {
    }

    public enum MediaKind {
        IMAGE, AUDIO, VIDEO, DOCUMENT, UNKNOWN
    }

    /**
     * Determine the media kind from a MIME type string.
     */
    public static MediaKind kindFromMime(String mime) {
        if (mime == null || mime.isBlank())
            return MediaKind.UNKNOWN;
        String lower = mime.toLowerCase();
        if (lower.startsWith("image/"))
            return MediaKind.IMAGE;
        if (lower.startsWith("audio/"))
            return MediaKind.AUDIO;
        if (lower.startsWith("video/"))
            return MediaKind.VIDEO;
        if (lower.equals("application/pdf"))
            return MediaKind.DOCUMENT;
        if (lower.startsWith("application/"))
            return MediaKind.DOCUMENT;
        return MediaKind.UNKNOWN;
    }

    /**
     * Maximum allowed bytes for a given media kind.
     */
    public static long maxBytesForKind(MediaKind kind) {
        return switch (kind) {
            case IMAGE -> MAX_IMAGE_BYTES;
            case AUDIO -> MAX_AUDIO_BYTES;
            case VIDEO -> MAX_VIDEO_BYTES;
            case DOCUMENT -> MAX_DOCUMENT_BYTES;
            case UNKNOWN -> MAX_DOCUMENT_BYTES;
        };
    }
}
