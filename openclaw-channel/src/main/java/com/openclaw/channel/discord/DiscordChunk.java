package com.openclaw.channel.discord;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Discord message text chunking with code fence balancing.
 * Corresponds to TypeScript's discord/chunk.ts.
 */
public final class DiscordChunk {

    private DiscordChunk() {
    }

    public static final int DEFAULT_MAX_CHARS = 2000;
    public static final int DEFAULT_MAX_LINES = 17;

    private static final Pattern FENCE_RE = Pattern.compile("^( {0,3})(`{3,}|~{3,})(.*)$");

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Chunk text for Discord, respecting char limits, line limits,
     * and keeping fenced code blocks balanced across chunks.
     */
    public static List<String> chunk(String text, int maxChars, int maxLines) {
        int charLimit = Math.max(1, maxChars);
        int lineLimit = Math.max(1, maxLines);

        String body = text != null ? text : "";
        if (body.isEmpty())
            return List.of();

        if (body.length() <= charLimit && countLines(body) <= lineLimit) {
            return List.of(body);
        }

        String[] lines = body.split("\n", -1);
        List<String> chunks = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        int currentLines = 0;
        OpenFence openFence = null;

        for (String originalLine : lines) {
            OpenFence fenceInfo = parseFenceLine(originalLine);
            OpenFence nextOpenFence = openFence;

            if (fenceInfo != null) {
                if (openFence == null) {
                    nextOpenFence = fenceInfo;
                } else if (openFence.markerChar == fenceInfo.markerChar
                        && fenceInfo.markerLen >= openFence.markerLen) {
                    nextOpenFence = null;
                }
            }

            int reserveChars = nextOpenFence != null ? closeFenceLine(nextOpenFence).length() + 1 : 0;
            int reserveLines = nextOpenFence != null ? 1 : 0;
            int effectiveCharLimit = Math.max(1, charLimit - reserveChars);
            int effectiveLineLimit = Math.max(1, lineLimit - reserveLines);
            int prefixLen = current.length() > 0 ? current.length() + 1 : 0;
            int segmentLimit = Math.max(1, effectiveCharLimit - prefixLen);
            List<String> segments = splitLongLine(originalLine, segmentLimit, openFence != null);

            for (int segIdx = 0; segIdx < segments.size(); segIdx++) {
                String segment = segments.get(segIdx);
                boolean isLineCont = segIdx > 0;
                String delimiter = isLineCont ? "" : (current.length() > 0 ? "\n" : "");
                String addition = delimiter + segment;
                int nextLen = current.length() + addition.length();
                int nextLines = currentLines + (isLineCont ? 0 : 1);

                if ((nextLen > effectiveCharLimit || nextLines > effectiveLineLimit) && current.length() > 0) {
                    // Flush
                    String payload = closeFenceIfNeeded(current.toString(), openFence);
                    if (!payload.isBlank())
                        chunks.add(payload);
                    current = new StringBuilder();
                    currentLines = 0;
                    if (openFence != null) {
                        current.append(openFence.openLine);
                        currentLines = 1;
                    }
                }

                if (current.length() > 0) {
                    current.append(addition);
                    if (!isLineCont)
                        currentLines++;
                } else {
                    current.append(segment);
                    currentLines = 1;
                }
            }

            openFence = nextOpenFence;
        }

        if (current.length() > 0) {
            String payload = closeFenceIfNeeded(current.toString(), openFence);
            if (!payload.isBlank())
                chunks.add(payload);
        }

        return rebalanceReasoningItalics(body, chunks);
    }

    public static List<String> chunk(String text) {
        return chunk(text, DEFAULT_MAX_CHARS, DEFAULT_MAX_LINES);
    }

    // =========================================================================
    // Internal helpers
    // =========================================================================

    private record OpenFence(String indent, char markerChar, int markerLen, String openLine) {
    }

    private static int countLines(String text) {
        if (text == null || text.isEmpty())
            return 0;
        return text.split("\n", -1).length;
    }

    private static OpenFence parseFenceLine(String line) {
        Matcher m = FENCE_RE.matcher(line);
        if (!m.matches())
            return null;
        String indent = m.group(1) != null ? m.group(1) : "";
        String marker = m.group(2) != null ? m.group(2) : "";
        return new OpenFence(indent, marker.charAt(0), marker.length(), line);
    }

    private static String closeFenceLine(OpenFence f) {
        return f.indent + String.valueOf(f.markerChar).repeat(f.markerLen);
    }

    private static String closeFenceIfNeeded(String text, OpenFence fence) {
        if (fence == null)
            return text;
        String closeLine = closeFenceLine(fence);
        if (text.isEmpty())
            return closeLine;
        if (!text.endsWith("\n"))
            return text + "\n" + closeLine;
        return text + closeLine;
    }

    private static List<String> splitLongLine(String line, int maxChars, boolean preserveWhitespace) {
        int limit = Math.max(1, maxChars);
        if (line.length() <= limit)
            return List.of(line);

        List<String> out = new ArrayList<>();
        String remaining = line;
        while (remaining.length() > limit) {
            if (preserveWhitespace) {
                out.add(remaining.substring(0, limit));
                remaining = remaining.substring(limit);
                continue;
            }
            String window = remaining.substring(0, limit);
            int breakIdx = -1;
            for (int i = window.length() - 1; i >= 0; i--) {
                if (Character.isWhitespace(window.charAt(i))) {
                    breakIdx = i;
                    break;
                }
            }
            if (breakIdx <= 0)
                breakIdx = limit;
            out.add(remaining.substring(0, breakIdx));
            remaining = remaining.substring(breakIdx);
        }
        if (!remaining.isEmpty())
            out.add(remaining);
        return out;
    }

    private static List<String> rebalanceReasoningItalics(String source, List<String> chunks) {
        if (chunks.size() <= 1)
            return chunks;
        if (!source.startsWith("Reasoning:\n_") || !source.stripTrailing().endsWith("_")) {
            return chunks;
        }

        List<String> adjusted = new ArrayList<>(chunks);
        for (int i = 0; i < adjusted.size(); i++) {
            boolean isLast = i == adjusted.size() - 1;
            String current = adjusted.get(i);

            if (!current.stripTrailing().endsWith("_")) {
                adjusted.set(i, current + "_");
            }
            if (isLast)
                break;

            String next = adjusted.get(i + 1);
            int leadingLen = next.length() - next.stripLeading().length();
            String leading = next.substring(0, leadingLen);
            String body = next.substring(leadingLen);
            if (!body.startsWith("_")) {
                adjusted.set(i + 1, leading + "_" + body);
            }
        }
        return adjusted;
    }
}
