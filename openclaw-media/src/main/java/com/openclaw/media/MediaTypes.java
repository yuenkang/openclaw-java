package com.openclaw.media;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * Media understanding types — capabilities, attachments, outputs, decisions.
 * Corresponds to TypeScript's media-understanding/types.ts.
 */
public final class MediaTypes {

    private MediaTypes() {
    }

    // =========================================================================
    // Enums
    // =========================================================================

    public enum MediaKind {
        AUDIO_TRANSCRIPTION("audio.transcription"),
        VIDEO_DESCRIPTION("video.description"),
        IMAGE_DESCRIPTION("image.description");

        private final String label;

        MediaKind(String label) {
            this.label = label;
        }

        public String label() {
            return label;
        }

        public static MediaKind fromString(String s) {
            if (s == null)
                return null;
            return switch (s) {
                case "audio.transcription" -> AUDIO_TRANSCRIPTION;
                case "video.description" -> VIDEO_DESCRIPTION;
                case "image.description" -> IMAGE_DESCRIPTION;
                default -> null;
            };
        }
    }

    public enum Capability {
        IMAGE("image"),
        AUDIO("audio"),
        VIDEO("video");

        private final String label;

        Capability(String label) {
            this.label = label;
        }

        public String label() {
            return label;
        }

        public static Capability fromString(String s) {
            if (s == null)
                return null;
            return switch (s.toLowerCase()) {
                case "image" -> IMAGE;
                case "audio" -> AUDIO;
                case "video" -> VIDEO;
                default -> null;
            };
        }
    }

    public enum DecisionOutcome {
        SUCCESS("success"),
        SKIPPED("skipped"),
        DISABLED("disabled"),
        NO_ATTACHMENT("no-attachment"),
        SCOPE_DENY("scope-deny");

        private final String label;

        DecisionOutcome(String label) {
            this.label = label;
        }

        public String label() {
            return label;
        }
    }

    // =========================================================================
    // Attachment
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MediaAttachment {
        private String path;
        private String url;
        private String mime;
        private int index;
    }

    // =========================================================================
    // Output
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MediaOutput {
        private MediaKind kind;
        private int attachmentIndex;
        private String text;
        private String provider;
        private String model;
    }

    // =========================================================================
    // Decisions
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ModelDecision {
        private String provider;
        private String model;
        @Builder.Default
        private String type = "provider"; // "provider" or "cli"
        @Builder.Default
        private String outcome = "success"; // "success", "skipped", "failed"
        private String reason;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AttachmentDecision {
        private int attachmentIndex;
        @Builder.Default
        private List<ModelDecision> attempts = List.of();
        private ModelDecision chosen;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MediaDecision {
        private Capability capability;
        private DecisionOutcome outcome;
        @Builder.Default
        private List<AttachmentDecision> attachments = List.of();
    }

    // =========================================================================
    // Request/Result types for provider calls
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AudioTranscriptionRequest {
        private byte[] buffer;
        private String fileName;
        private String mime;
        private String apiKey;
        private String baseUrl;
        private String model;
        private String language;
        private String prompt;
        @Builder.Default
        private int timeoutMs = 30000;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AudioTranscriptionResult {
        private String text;
        private String model;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class VideoDescriptionRequest {
        private byte[] buffer;
        private String fileName;
        private String mime;
        private String apiKey;
        private String baseUrl;
        private String model;
        private String prompt;
        @Builder.Default
        private int timeoutMs = 60000;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class VideoDescriptionResult {
        private String text;
        private String model;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ImageDescriptionRequest {
        private byte[] buffer;
        private String fileName;
        private String mime;
        private String model;
        private String provider;
        private String prompt;
        private Integer maxTokens;
        @Builder.Default
        private int timeoutMs = 30000;
        private String profile;
        private String agentDir;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ImageDescriptionResult {
        private String text;
        private String model;
    }

    // =========================================================================
    // Apply result
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ApplyResult {
        @Builder.Default
        private List<MediaOutput> outputs = List.of();
        @Builder.Default
        private List<MediaDecision> decisions = List.of();
        private boolean appliedImage;
        private boolean appliedAudio;
        private boolean appliedVideo;
        private boolean appliedFile;
    }
}
