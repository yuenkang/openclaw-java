package com.openclaw.agent.autoreply;

import java.util.*;
import java.util.regex.Pattern;

/**
 * Inbound media note builder — formats media attachment metadata for the agent
 * prompt.
 * Mirrors {@code auto-reply/media-note.ts}.
 */
public final class MediaNote {

    private MediaNote() {
    }

    /**
     * Format a single media attachment line.
     */
    public static String formatMediaAttachedLine(String path, String url, String type,
            Integer index, Integer total) {
        String prefix = (index != null && total != null)
                ? "[media attached " + index + "/" + total + ": "
                : "[media attached: ";
        String typePart = (type != null && !type.isBlank()) ? " (" + type.trim() + ")" : "";
        String urlRaw = url != null ? url.trim() : "";
        String urlPart = !urlRaw.isEmpty() ? " | " + urlRaw : "";
        return prefix + path + typePart + urlPart + "]";
    }

    /**
     * Build an inbound media note from message context fields.
     *
     * @param mediaPath                   single media path (fallback)
     * @param mediaPaths                  array of media paths
     * @param mediaUrl                    single media URL (fallback)
     * @param mediaUrls                   array of media URLs
     * @param mediaType                   single media type (fallback)
     * @param mediaTypes                  array of media types
     * @param mediaUnderstanding          list of media understanding outputs (each
     *                                    with attachmentIndex)
     * @param mediaUnderstandingDecisions list of decisions with attachments
     * @return the formatted note, or null if no media
     */
    @SuppressWarnings("unchecked")
    public static String buildInboundMediaNote(
            String mediaPath,
            List<String> mediaPaths,
            String mediaUrl,
            List<String> mediaUrls,
            String mediaType,
            List<String> mediaTypes,
            List<Map<String, Object>> mediaUnderstanding,
            List<Map<String, Object>> mediaUnderstandingDecisions) {

        // Build suppressed attachment indices
        Set<Integer> suppressed = new HashSet<>();
        if (mediaUnderstanding != null) {
            for (Map<String, Object> output : mediaUnderstanding) {
                Object idx = output.get("attachmentIndex");
                if (idx instanceof Number n)
                    suppressed.add(n.intValue());
            }
        }
        if (mediaUnderstandingDecisions != null) {
            for (Map<String, Object> decision : mediaUnderstandingDecisions) {
                if (!"success".equals(decision.get("outcome")))
                    continue;
                Object attachments = decision.get("attachments");
                if (attachments instanceof List<?> attList) {
                    for (Object att : attList) {
                        if (att instanceof Map<?, ?> attMap) {
                            Object chosen = attMap.get("chosen");
                            if (chosen instanceof Map<?, ?> chosenMap
                                    && "success".equals(chosenMap.get("outcome"))) {
                                Object idx = attMap.get("attachmentIndex");
                                if (idx instanceof Number n)
                                    suppressed.add(n.intValue());
                            }
                        }
                    }
                }
            }
        }

        // Resolve paths
        List<String> paths;
        if (mediaPaths != null && !mediaPaths.isEmpty()) {
            paths = mediaPaths;
        } else if (mediaPath != null && !mediaPath.isBlank()) {
            paths = List.of(mediaPath.trim());
        } else {
            return null;
        }
        if (paths.isEmpty())
            return null;

        // Resolve URLs and types parallel arrays
        List<String> urls = (mediaUrls != null && mediaUrls.size() == paths.size()) ? mediaUrls : null;
        List<String> types = (mediaTypes != null && mediaTypes.size() == paths.size()) ? mediaTypes : null;

        // Filter suppressed
        record MediaEntry(String path, String type, String url, int index) {
        }
        List<MediaEntry> entries = new ArrayList<>();
        for (int i = 0; i < paths.size(); i++) {
            if (suppressed.contains(i))
                continue;
            String p = paths.get(i) != null ? paths.get(i) : "";
            String t = types != null ? types.get(i) : mediaType;
            String u = urls != null ? urls.get(i) : mediaUrl;
            entries.add(new MediaEntry(p, t, u, i));
        }
        if (entries.isEmpty())
            return null;

        if (entries.size() == 1) {
            MediaEntry e = entries.get(0);
            return formatMediaAttachedLine(e.path, e.url, e.type, null, null);
        }

        int count = entries.size();
        List<String> lines = new ArrayList<>();
        lines.add("[media attached: " + count + " files]");
        for (int idx = 0; idx < entries.size(); idx++) {
            MediaEntry e = entries.get(idx);
            lines.add(formatMediaAttachedLine(e.path, e.url, e.type, idx + 1, count));
        }
        return String.join("\n", lines);
    }
}
