package com.openclaw.agent.autoreply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.function.Function;

/**
 * Message chunking — split long agent replies into capped-length
 * segments for channel delivery. Handles paragraph, sentence,
 * word, and hard-char boundary splitting with configurable
 * MAX_CHARS and minimal orphan suppression.
 * Mirrors {@code auto-reply/chunk.ts}.
 */
public final class Chunk {

    private static final Logger log = LoggerFactory.getLogger(Chunk.class);

    private Chunk() {
    }

    public static final int DEFAULT_MAX_CHARS = 4096;
    public static final int MIN_CHUNK_SIZE = 64;
    public static final String CONTINUATION_MARKER = "…";

    /** A single chunk from splitting a message. */
    public record MessageChunk(String text, int index, boolean isLast) {
    }

    /** Options for splitMessage. */
    public record SplitOptions(
            Integer maxChars,
            boolean addContinuationMarker,
            Function<String, Boolean> skipChunk) {

        public static SplitOptions defaults() {
            return new SplitOptions(null, true, null);
        }
    }

    /**
     * Split a long message into chunks respecting paragraph, sentence,
     * word, and char boundaries.
     */
    public static List<MessageChunk> splitMessage(String text, SplitOptions options) {
        if (text == null || text.isEmpty()) {
            return List.of(new MessageChunk("", 0, true));
        }

        int maxChars = options != null && options.maxChars() != null && options.maxChars() > 0
                ? Math.max(options.maxChars(), MIN_CHUNK_SIZE)
                : DEFAULT_MAX_CHARS;
        boolean addMarker = options == null || options.addContinuationMarker();
        Function<String, Boolean> skipChunk = options != null ? options.skipChunk() : null;

        if (text.length() <= maxChars) {
            return List.of(new MessageChunk(text, 0, true));
        }

        List<String> rawChunks = splitIntoParagraphChunks(text, maxChars);
        List<MessageChunk> result = new ArrayList<>();
        int index = 0;
        for (int i = 0; i < rawChunks.size(); i++) {
            String chunk = rawChunks.get(i).trim();
            if (chunk.isEmpty())
                continue;
            if (skipChunk != null && Boolean.TRUE.equals(skipChunk.apply(chunk)))
                continue;
            boolean isLast = (i == rawChunks.size() - 1);
            String finalText = addMarker && !isLast
                    ? chunk + "\n" + CONTINUATION_MARKER
                    : chunk;
            result.add(new MessageChunk(finalText, index, isLast));
            index++;
        }

        // Fix isLast flags
        if (!result.isEmpty()) {
            List<MessageChunk> fixed = new ArrayList<>(result.size());
            for (int i = 0; i < result.size(); i++) {
                MessageChunk c = result.get(i);
                fixed.add(new MessageChunk(c.text(), i, i == result.size() - 1));
            }
            result = fixed;
        }

        return result.isEmpty() ? List.of(new MessageChunk("", 0, true)) : result;
    }

    /**
     * Split text respecting paragraph boundaries.
     */
    private static List<String> splitIntoParagraphChunks(String text, int maxChars) {
        String[] paragraphs = text.split("\n\n+");
        List<String> chunks = new ArrayList<>();
        StringBuilder current = new StringBuilder();

        for (String para : paragraphs) {
            String trimmed = para.trim();
            if (trimmed.isEmpty())
                continue;

            if (current.isEmpty()) {
                if (trimmed.length() <= maxChars) {
                    current.append(trimmed);
                } else {
                    chunks.addAll(splitLongParagraph(trimmed, maxChars));
                }
                continue;
            }

            String candidate = current + "\n\n" + trimmed;
            if (candidate.length() <= maxChars) {
                current.append("\n\n").append(trimmed);
            } else {
                // Flush current and start new
                chunks.add(current.toString());
                current.setLength(0);
                if (trimmed.length() <= maxChars) {
                    current.append(trimmed);
                } else {
                    chunks.addAll(splitLongParagraph(trimmed, maxChars));
                }
            }
        }
        if (!current.isEmpty()) {
            chunks.add(current.toString());
        }
        return chunks;
    }

    /**
     * Split a single long paragraph by sentence and word boundaries.
     */
    private static List<String> splitLongParagraph(String paragraph, int maxChars) {
        // Try sentence split first
        String[] sentences = paragraph.split("(?<=\\.\\s)|(?<=!\\s)|(?<=\\?\\s)");
        List<String> chunks = new ArrayList<>();
        StringBuilder current = new StringBuilder();

        for (String sentence : sentences) {
            if (current.isEmpty()) {
                if (sentence.length() <= maxChars) {
                    current.append(sentence);
                } else {
                    chunks.addAll(splitByWords(sentence, maxChars));
                }
                continue;
            }
            String candidate = current.toString() + sentence;
            if (candidate.length() <= maxChars) {
                current.append(sentence);
            } else {
                chunks.add(current.toString().trim());
                current.setLength(0);
                if (sentence.length() <= maxChars) {
                    current.append(sentence);
                } else {
                    chunks.addAll(splitByWords(sentence, maxChars));
                }
            }
        }
        if (!current.isEmpty()) {
            chunks.add(current.toString().trim());
        }
        return chunks;
    }

    /**
     * Split text by word boundaries, falling back to hard char split.
     */
    private static List<String> splitByWords(String text, int maxChars) {
        String[] words = text.split("\\s+");
        List<String> chunks = new ArrayList<>();
        StringBuilder current = new StringBuilder();

        for (String word : words) {
            if (word.length() > maxChars) {
                // Hard-split oversized word
                if (!current.isEmpty()) {
                    chunks.add(current.toString().trim());
                    current.setLength(0);
                }
                for (int i = 0; i < word.length(); i += maxChars) {
                    chunks.add(word.substring(i, Math.min(i + maxChars, word.length())));
                }
                continue;
            }
            if (current.isEmpty()) {
                current.append(word);
                continue;
            }
            if (current.length() + 1 + word.length() <= maxChars) {
                current.append(" ").append(word);
            } else {
                chunks.add(current.toString().trim());
                current.setLength(0);
                current.append(word);
            }
        }
        if (!current.isEmpty()) {
            chunks.add(current.toString().trim());
        }
        return chunks;
    }

    /**
     * Convenience: split message with default options.
     */
    public static List<MessageChunk> splitMessage(String text) {
        return splitMessage(text, SplitOptions.defaults());
    }

    /**
     * Convenience: split message with a custom max chars.
     */
    public static List<MessageChunk> splitMessage(String text, int maxChars) {
        return splitMessage(text, new SplitOptions(maxChars, true, null));
    }
}
