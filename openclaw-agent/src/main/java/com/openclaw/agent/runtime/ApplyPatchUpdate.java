package com.openclaw.agent.runtime;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.UnaryOperator;

/**
 * Applies update hunks (diff chunks) to an existing file.
 * Mirrors {@code agents/apply-patch-update.ts}.
 */
public final class ApplyPatchUpdate {

    private ApplyPatchUpdate() {
    }

    /** A single update chunk within a file patch. */
    public record UpdateFileChunk(
            String changeContext,
            List<String> oldLines,
            List<String> newLines,
            boolean isEndOfFile) {
    }

    /**
     * Read a file, apply the update chunks, and return the patched content.
     */
    public static String applyUpdateHunk(String filePath, List<UpdateFileChunk> chunks)
            throws IOException {
        String contents;
        try {
            contents = Files.readString(Path.of(filePath));
        } catch (IOException e) {
            throw new IOException("Failed to read file to update " + filePath + ": " + e.getMessage(), e);
        }

        List<String> originalLines = new ArrayList<>(List.of(contents.split("\n", -1)));
        if (!originalLines.isEmpty() && originalLines.get(originalLines.size() - 1).isEmpty()) {
            originalLines.remove(originalLines.size() - 1);
        }

        List<int[]> replacements = computeReplacements(originalLines, filePath, chunks);
        List<String> newLines = applyReplacements(originalLines, replacements, chunks);
        if (newLines.isEmpty() || !newLines.get(newLines.size() - 1).isEmpty()) {
            newLines.add("");
        }
        return String.join("\n", newLines);
    }

    // --- Internal ---

    private static List<int[]> computeReplacements(
            List<String> originalLines, String filePath, List<UpdateFileChunk> chunks) {
        // Each replacement: [startIndex, oldLength, chunkIndex]
        List<int[]> replacements = new ArrayList<>();
        int lineIndex = 0;

        for (int ci = 0; ci < chunks.size(); ci++) {
            UpdateFileChunk chunk = chunks.get(ci);
            if (chunk.changeContext() != null) {
                Integer ctxIndex = seekSequence(
                        originalLines, List.of(chunk.changeContext()), lineIndex, false);
                if (ctxIndex == null) {
                    throw new IllegalArgumentException(
                            "Failed to find context '" + chunk.changeContext() + "' in " + filePath);
                }
                lineIndex = ctxIndex + 1;
            }

            if (chunk.oldLines().isEmpty()) {
                int insertionIndex = (!originalLines.isEmpty()
                        && originalLines.get(originalLines.size() - 1).isEmpty())
                                ? originalLines.size() - 1
                                : originalLines.size();
                replacements.add(new int[] { insertionIndex, 0, ci });
                continue;
            }

            List<String> pattern = chunk.oldLines();
            Integer found = seekSequence(originalLines, pattern, lineIndex, chunk.isEndOfFile());

            if (found == null && !pattern.isEmpty() && pattern.get(pattern.size() - 1).isEmpty()) {
                pattern = pattern.subList(0, pattern.size() - 1);
                found = seekSequence(originalLines, pattern, lineIndex, chunk.isEndOfFile());
            }

            if (found == null) {
                throw new IllegalArgumentException(
                        "Failed to find expected lines in " + filePath + ":\n"
                                + String.join("\n", chunk.oldLines()));
            }

            replacements.add(new int[] { found, pattern.size(), ci });
            lineIndex = found + pattern.size();
        }

        replacements.sort((a, b) -> Integer.compare(a[0], b[0]));
        return replacements;
    }

    private static List<String> applyReplacements(
            List<String> lines,
            List<int[]> replacements,
            List<UpdateFileChunk> chunks) {
        List<String> result = new ArrayList<>(lines);
        // Apply in reverse order to preserve indices
        List<int[]> reversed = new ArrayList<>(replacements);
        Collections.reverse(reversed);
        for (int[] rep : reversed) {
            int startIndex = rep[0];
            int oldLen = rep[1];
            int chunkIndex = rep[2];
            List<String> newLines = resolveNewLines(chunks.get(chunkIndex), oldLen);
            // Remove old lines
            for (int i = 0; i < oldLen && startIndex < result.size(); i++) {
                result.remove(startIndex);
            }
            // Insert new lines
            for (int i = 0; i < newLines.size(); i++) {
                result.add(startIndex + i, newLines.get(i));
            }
        }
        return result;
    }

    private static List<String> resolveNewLines(UpdateFileChunk chunk, int usedOldLen) {
        List<String> lines = chunk.newLines();
        // Trim trailing empty line if oldLines was also trimmed
        if (usedOldLen < chunk.oldLines().size()
                && !lines.isEmpty() && lines.get(lines.size() - 1).isEmpty()) {
            return lines.subList(0, lines.size() - 1);
        }
        return lines;
    }

    static Integer seekSequence(
            List<String> lines, List<String> pattern, int start, boolean eof) {
        if (pattern.isEmpty()) {
            return start;
        }
        if (pattern.size() > lines.size()) {
            return null;
        }
        int maxStart = lines.size() - pattern.size();
        int searchStart = eof && lines.size() >= pattern.size() ? maxStart : start;
        if (searchStart > maxStart) {
            return null;
        }

        // Exact match
        for (int i = searchStart; i <= maxStart; i++) {
            if (linesMatch(lines, pattern, i, UnaryOperator.identity()))
                return i;
        }
        // trimEnd match
        for (int i = searchStart; i <= maxStart; i++) {
            if (linesMatch(lines, pattern, i, String::stripTrailing))
                return i;
        }
        // trim match
        for (int i = searchStart; i <= maxStart; i++) {
            if (linesMatch(lines, pattern, i, String::strip))
                return i;
        }
        // normalize punctuation match
        for (int i = searchStart; i <= maxStart; i++) {
            if (linesMatch(lines, pattern, i, v -> normalizePunctuation(v.strip())))
                return i;
        }
        return null;
    }

    private static boolean linesMatch(
            List<String> lines, List<String> pattern, int start,
            UnaryOperator<String> normalize) {
        for (int idx = 0; idx < pattern.size(); idx++) {
            if (!normalize.apply(lines.get(start + idx))
                    .equals(normalize.apply(pattern.get(idx)))) {
                return false;
            }
        }
        return true;
    }

    static String normalizePunctuation(String value) {
        StringBuilder sb = new StringBuilder(value.length());
        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
            sb.append(switch (ch) {
                case '\u2010', '\u2011', '\u2012', '\u2013', '\u2014', '\u2015', '\u2212' -> '-';
                case '\u2018', '\u2019', '\u201A', '\u201B' -> '\'';
                case '\u201C', '\u201D', '\u201E', '\u201F' -> '"';
                case '\u00A0', '\u2002', '\u2003', '\u2004', '\u2005', '\u2006', '\u2007',
                        '\u2008', '\u2009', '\u200A', '\u202F', '\u205F', '\u3000' ->
                    ' ';
                default -> ch;
            });
        }
        return sb.toString();
    }
}
