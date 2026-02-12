package com.openclaw.agent.runtime.subscribe;

import com.openclaw.agent.runtime.subscribe.SubscribeTypes.BlockReplyChunking;

import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Block reply chunker — splits streamed text into sized chunks with
 * markdown-safe fence handling.
 * Corresponds to TypeScript pi-embedded-block-chunker.ts.
 */
public class BlockChunker {

    private String buffer = "";
    private final BlockReplyChunking chunking;

    public BlockChunker(BlockReplyChunking chunking) {
        this.chunking = chunking;
    }

    public void append(String text) {
        if (text == null || text.isEmpty())
            return;
        buffer += text;
    }

    public void reset() {
        buffer = "";
    }

    public String getBufferedText() {
        return buffer;
    }

    public boolean hasBuffered() {
        return !buffer.isEmpty();
    }

    /**
     * Drain the buffer, emitting chunks.
     */
    public void drain(boolean force, Consumer<String> emit) {
        int minChars = Math.max(1, chunking.minChars());
        int maxChars = Math.max(minChars, chunking.maxChars());

        // Flush-on-paragraph mode (eager paragraph splitting)
        if (chunking.flushOnParagraph() && !force) {
            drainParagraphs(emit, maxChars);
            return;
        }

        if (buffer.length() < minChars && !force)
            return;

        if (force && buffer.length() <= maxChars) {
            if (!buffer.trim().isEmpty())
                emit.accept(buffer);
            buffer = "";
            return;
        }

        while (buffer.length() >= minChars || (force && !buffer.isEmpty())) {
            int breakIdx;
            if (force && buffer.length() <= maxChars) {
                breakIdx = pickSoftBreakIndex(buffer, 1);
            } else {
                breakIdx = pickBreakIndex(buffer, force ? 1 : -1);
            }
            if (breakIdx <= 0) {
                if (force) {
                    emit.accept(buffer);
                    buffer = "";
                }
                return;
            }

            String chunk = buffer.substring(0, breakIdx);
            if (chunk.trim().isEmpty()) {
                buffer = stripLeadingNewlines(buffer.substring(breakIdx)).stripLeading();
                continue;
            }
            emit.accept(chunk);

            int nextStart = breakIdx < buffer.length()
                    && Character.isWhitespace(buffer.charAt(breakIdx))
                            ? breakIdx + 1
                            : breakIdx;
            buffer = stripLeadingNewlines(buffer.substring(nextStart));

            if (buffer.length() < minChars && !force)
                return;
            if (buffer.length() < maxChars && !force)
                return;
        }
    }

    private void drainParagraphs(Consumer<String> emit, int maxChars) {
        while (!buffer.isEmpty()) {
            int paraIdx = findNextParagraphBreak(buffer, 0);
            if (paraIdx < 0 || paraIdx > maxChars) {
                if (buffer.length() >= maxChars) {
                    int breakIdx = pickBreakIndex(buffer, 1);
                    if (breakIdx > 0) {
                        String chunk = buffer.substring(0, breakIdx);
                        if (!chunk.trim().isEmpty())
                            emit.accept(chunk);
                        buffer = stripLeadingNewlines(buffer.substring(breakIdx));
                        continue;
                    }
                }
                return;
            }
            String chunk = buffer.substring(0, paraIdx);
            if (!chunk.trim().isEmpty())
                emit.accept(chunk);
            // Skip the paragraph break
            int skipEnd = paraIdx;
            while (skipEnd < buffer.length() && (buffer.charAt(skipEnd) == '\n' || buffer.charAt(skipEnd) == '\r'
                    || buffer.charAt(skipEnd) == ' ' || buffer.charAt(skipEnd) == '\t'))
                skipEnd++;
            buffer = stripLeadingNewlines(buffer.substring(skipEnd));
        }
    }

    private int pickSoftBreakIndex(String buf, int minCharsOverride) {
        int min = Math.max(1, minCharsOverride >= 0 ? minCharsOverride : chunking.minChars());
        if (buf.length() < min)
            return -1;
        var pref = chunking.breakPreference() != null
                ? chunking.breakPreference()
                : BlockReplyChunking.BreakPreference.paragraph;

        if (pref == BlockReplyChunking.BreakPreference.paragraph) {
            int i = buf.indexOf("\n\n");
            while (i >= 0) {
                if (i >= min)
                    return i;
                i = buf.indexOf("\n\n", i + 2);
            }
        }
        if (pref == BlockReplyChunking.BreakPreference.paragraph
                || pref == BlockReplyChunking.BreakPreference.newline) {
            int i = buf.indexOf('\n');
            while (i >= 0) {
                if (i >= min)
                    return i;
                i = buf.indexOf('\n', i + 1);
            }
        }
        if (pref != BlockReplyChunking.BreakPreference.newline) {
            Matcher m = Pattern.compile("[.!?](?=\\s|$)").matcher(buf);
            int sentenceIdx = -1;
            while (m.find()) {
                int at = m.start();
                if (at >= min)
                    sentenceIdx = at + 1;
            }
            if (sentenceIdx >= min)
                return sentenceIdx;
        }
        return -1;
    }

    private int pickBreakIndex(String buf, int minCharsOverride) {
        int min = Math.max(1, minCharsOverride >= 0 ? minCharsOverride : chunking.minChars());
        int max = Math.max(min, chunking.maxChars());
        if (buf.length() < min)
            return -1;
        String window = buf.substring(0, Math.min(max, buf.length()));
        var pref = chunking.breakPreference() != null
                ? chunking.breakPreference()
                : BlockReplyChunking.BreakPreference.paragraph;

        if (pref == BlockReplyChunking.BreakPreference.paragraph) {
            int i = window.lastIndexOf("\n\n");
            while (i >= min)
                return i;
        }
        if (pref == BlockReplyChunking.BreakPreference.paragraph
                || pref == BlockReplyChunking.BreakPreference.newline) {
            int i = window.lastIndexOf('\n');
            if (i >= min)
                return i;
        }
        if (pref != BlockReplyChunking.BreakPreference.newline) {
            Matcher m = Pattern.compile("[.!?](?=\\s|$)").matcher(window);
            int sentenceIdx = -1;
            while (m.find()) {
                if (m.start() >= min)
                    sentenceIdx = m.start() + 1;
            }
            if (sentenceIdx >= min)
                return sentenceIdx;
        }
        if (pref == BlockReplyChunking.BreakPreference.newline && buf.length() < max)
            return -1;
        for (int i = window.length() - 1; i >= min; i--) {
            if (Character.isWhitespace(window.charAt(i)))
                return i;
        }
        if (buf.length() >= max)
            return max;
        return -1;
    }

    private static int findNextParagraphBreak(String buffer, int start) {
        if (start < 0)
            return -1;
        Matcher m = Pattern.compile("\\n[\\t ]*\\n+").matcher(buffer);
        while (m.find(start)) {
            if (m.start() >= 0)
                return m.start();
        }
        return -1;
    }

    private static String stripLeadingNewlines(String value) {
        int i = 0;
        while (i < value.length() && value.charAt(i) == '\n')
            i++;
        return i > 0 ? value.substring(i) : value;
    }
}
