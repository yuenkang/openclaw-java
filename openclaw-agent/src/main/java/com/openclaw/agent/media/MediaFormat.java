package com.openclaw.agent.media;

import java.util.*;
import java.util.regex.Pattern;

/**
 * Format media understanding outputs into human-readable text.
 * Corresponds to TypeScript's media-understanding/format.ts.
 */
public final class MediaFormat {

    private MediaFormat() {
    }

    private static final Pattern MEDIA_PLACEHOLDER_RE = Pattern.compile("^<media:[^>]+>(\\s*\\([^)]*\\))?$",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern MEDIA_PLACEHOLDER_TOKEN_RE = Pattern.compile("^<media:[^>]+>(\\s*\\([^)]*\\))?\\s*",
            Pattern.CASE_INSENSITIVE);

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Extract user text from a message body, stripping media placeholders.
     *
     * @return cleaned user text, or null if body is only a placeholder
     */
    public static String extractMediaUserText(String body) {
        String trimmed = (body != null) ? body.trim() : "";
        if (trimmed.isEmpty()) {
            return null;
        }
        if (MEDIA_PLACEHOLDER_RE.matcher(trimmed).matches()) {
            return null;
        }
        String cleaned = MEDIA_PLACEHOLDER_TOKEN_RE.matcher(trimmed).replaceFirst("").trim();
        return cleaned.isEmpty() ? null : cleaned;
    }

    /**
     * Format media understanding outputs into a single body string.
     */
    public static String formatMediaUnderstandingBody(String body,
            List<MediaTypes.MediaOutput> outputs) {
        List<MediaTypes.MediaOutput> filtered = outputs.stream()
                .filter(o -> o.getText() != null && !o.getText().isBlank())
                .toList();

        if (filtered.isEmpty()) {
            return body != null ? body : "";
        }

        String userText = extractMediaUserText(body);
        List<String> sections = new ArrayList<>();

        // If multiple outputs and user text, put user text first
        if (userText != null && filtered.size() > 1) {
            sections.add("User text:\n" + userText);
        }

        // Count by kind for numbering
        Map<MediaTypes.MediaKind, Integer> counts = new LinkedHashMap<>();
        for (var output : filtered) {
            counts.merge(output.getKind(), Integer.valueOf(1), (a, b) -> Integer.valueOf(a.intValue() + b.intValue()));
        }
        Map<MediaTypes.MediaKind, Integer> seen = new LinkedHashMap<>();

        for (var output : filtered) {
            int count = counts.getOrDefault(output.getKind(), 1);
            int next = seen.merge(output.getKind(), Integer.valueOf(1),
                    (a, b) -> Integer.valueOf(a.intValue() + b.intValue()));
            String suffix = count > 1 ? " " + next + "/" + count : "";

            String sectionUserText = (filtered.size() == 1) ? userText : null;

            switch (output.getKind()) {
                case AUDIO_TRANSCRIPTION ->
                    sections.add(formatSection("Audio" + suffix, "Transcript",
                            output.getText(), sectionUserText));
                case IMAGE_DESCRIPTION ->
                    sections.add(formatSection("Image" + suffix, "Description",
                            output.getText(), sectionUserText));
                case VIDEO_DESCRIPTION ->
                    sections.add(formatSection("Video" + suffix, "Description",
                            output.getText(), sectionUserText));
            }
        }

        return String.join("\n\n", sections).trim();
    }

    /**
     * Format audio transcripts into a single string.
     */
    public static String formatAudioTranscripts(List<MediaTypes.MediaOutput> outputs) {
        if (outputs.size() == 1) {
            return outputs.get(0).getText();
        }
        List<String> parts = new ArrayList<>();
        for (int i = 0; i < outputs.size(); i++) {
            parts.add("Audio " + (i + 1) + ":\n" + outputs.get(i).getText());
        }
        return String.join("\n\n", parts);
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private static String formatSection(String title, String kind,
            String text, String userText) {
        List<String> lines = new ArrayList<>();
        lines.add("[" + title + "]");
        if (userText != null) {
            lines.add("User text:\n" + userText);
        }
        lines.add(kind + ":\n" + text);
        return String.join("\n", lines);
    }
}
