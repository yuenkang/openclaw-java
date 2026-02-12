package com.openclaw.agent.runtime.subscribe;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Embedded subscribe utilities — text stripping, thinking tag parsing,
 * assistant text extraction.
 * Corresponds to TypeScript pi-embedded-utils.ts.
 */
public final class EmbeddedUtils {

    private EmbeddedUtils() {
    }

    // ── Minimax tool call XML stripping ──────────────────────────────

    private static final Pattern MINIMAX_TOOL_CALL_RE = Pattern.compile("minimax:tool_call", Pattern.CASE_INSENSITIVE);
    private static final Pattern INVOKE_BLOCK_RE = Pattern.compile("<invoke\\b[^>]*>[\\s\\S]*?</invoke>",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern MINIMAX_TAG_RE = Pattern.compile("</?minimax:tool_call>", Pattern.CASE_INSENSITIVE);

    public static String stripMinimaxToolCallXml(String text) {
        if (text == null || text.isEmpty())
            return text;
        if (!MINIMAX_TOOL_CALL_RE.matcher(text).find())
            return text;
        String cleaned = INVOKE_BLOCK_RE.matcher(text).replaceAll("");
        return MINIMAX_TAG_RE.matcher(cleaned).replaceAll("");
    }

    // ── Downgraded tool call text stripping ──────────────────────────

    private static final Pattern TOOL_CALL_MARKER_RE = Pattern.compile("\\[Tool (?:Call|Result)",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern TOOL_CALL_BLOCK_RE = Pattern.compile("\\[Tool Call:[^\\]]*\\]",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern TOOL_RESULT_BLOCK_RE = Pattern.compile(
            "\\[Tool Result for ID[^\\]]*\\]\\n?[\\s\\S]*?(?=\\n*\\[Tool |\\n*$)", Pattern.CASE_INSENSITIVE);

    public static String stripDowngradedToolCallText(String text) {
        if (text == null || text.isEmpty())
            return text;
        if (!TOOL_CALL_MARKER_RE.matcher(text).find())
            return text;
        String cleaned = stripToolCallBlocks(text);
        cleaned = TOOL_RESULT_BLOCK_RE.matcher(cleaned).replaceAll("");
        return cleaned.trim();
    }

    private static String stripToolCallBlocks(String input) {
        Matcher m = TOOL_CALL_BLOCK_RE.matcher(input);
        StringBuilder result = new StringBuilder();
        int cursor = 0;
        while (m.find()) {
            int start = m.start();
            if (start < cursor)
                continue;
            result.append(input, cursor, start);
            int index = m.end();
            // Skip trailing whitespace and "Arguments:" block
            while (index < input.length() && (input.charAt(index) == ' ' || input.charAt(index) == '\t'))
                index++;
            if (index < input.length() && input.charAt(index) == '\r')
                index++;
            if (index < input.length() && input.charAt(index) == '\n')
                index++;
            while (index < input.length() && (input.charAt(index) == ' ' || input.charAt(index) == '\t'))
                index++;
            if (index + 9 <= input.length() && input.substring(index, index + 9).equalsIgnoreCase("arguments")) {
                index += 9;
                if (index < input.length() && input.charAt(index) == ':')
                    index++;
                if (index < input.length() && input.charAt(index) == ' ')
                    index++;
                Integer jsonEnd = consumeJsonish(input, index, true);
                if (jsonEnd != null)
                    index = jsonEnd;
            }
            if (index < input.length() && (input.charAt(index) == '\n' || input.charAt(index) == '\r')
                    && (result.length() == 0 || result.charAt(result.length() - 1) == '\n'
                            || result.charAt(result.length() - 1) == '\r')) {
                if (input.charAt(index) == '\r')
                    index++;
                if (index < input.length() && input.charAt(index) == '\n')
                    index++;
            }
            cursor = index;
        }
        result.append(input, cursor, input.length());
        return result.toString();
    }

    private static Integer consumeJsonish(String input, int start, boolean allowLeadingNewlines) {
        int index = start;
        while (index < input.length()) {
            char ch = input.charAt(index);
            if (ch == ' ' || ch == '\t') {
                index++;
                continue;
            }
            if (allowLeadingNewlines && (ch == '\n' || ch == '\r')) {
                index++;
                continue;
            }
            break;
        }
        if (index >= input.length())
            return null;
        char startChar = input.charAt(index);
        if (startChar == '{' || startChar == '[') {
            int depth = 0;
            boolean inString = false, escape = false;
            for (int i = index; i < input.length(); i++) {
                char ch = input.charAt(i);
                if (inString) {
                    if (escape) {
                        escape = false;
                    } else if (ch == '\\')
                        escape = true;
                    else if (ch == '"')
                        inString = false;
                    continue;
                }
                if (ch == '"') {
                    inString = true;
                    continue;
                }
                if (ch == '{' || ch == '[') {
                    depth++;
                    continue;
                }
                if (ch == '}' || ch == ']') {
                    depth--;
                    if (depth == 0)
                        return i + 1;
                }
            }
            return null;
        }
        if (startChar == '"') {
            boolean escape = false;
            for (int i = index + 1; i < input.length(); i++) {
                char ch = input.charAt(i);
                if (escape) {
                    escape = false;
                    continue;
                }
                if (ch == '\\') {
                    escape = true;
                    continue;
                }
                if (ch == '"')
                    return i + 1;
            }
            return null;
        }
        int end = index;
        while (end < input.length() && input.charAt(end) != '\n' && input.charAt(end) != '\r')
            end++;
        return end;
    }

    // ── Thinking tag operations ─────────────────────────────────────

    private static final Pattern THINKING_TAG_RE = Pattern.compile(
            "<\\s*(/?\\s*)(?:think(?:ing)?|thought|antthinking)\\s*>", Pattern.CASE_INSENSITIVE);

    public static String stripThinkingTagsFromText(String text) {
        if (text == null || text.isEmpty())
            return "";
        Matcher m = THINKING_TAG_RE.matcher(text);
        StringBuilder result = new StringBuilder();
        int lastIndex = 0;
        boolean inThinking = false;
        while (m.find()) {
            if (!inThinking)
                result.append(text, lastIndex, m.start());
            inThinking = !m.group(1).contains("/");
            lastIndex = m.end();
        }
        if (!inThinking)
            result.append(text, lastIndex, text.length());
        return result.toString().trim();
    }

    public static String extractThinkingFromTaggedText(String text) {
        if (text == null || text.isEmpty())
            return "";
        Matcher m = THINKING_TAG_RE.matcher(text);
        StringBuilder result = new StringBuilder();
        int lastIndex = 0;
        boolean inThinking = false;
        while (m.find()) {
            if (inThinking)
                result.append(text, lastIndex, m.start());
            boolean isClose = m.group(1).contains("/");
            inThinking = !isClose;
            lastIndex = m.end();
        }
        return result.toString().trim();
    }

    public static String extractThinkingFromTaggedStream(String text) {
        if (text == null || text.isEmpty())
            return "";
        String closed = extractThinkingFromTaggedText(text);
        if (!closed.isEmpty())
            return closed;
        Pattern openRe = Pattern.compile("<\\s*(?:think(?:ing)?|thought|antthinking)\\s*>", Pattern.CASE_INSENSITIVE);
        Pattern closeRe = Pattern.compile("<\\s*/\\s*(?:think(?:ing)?|thought|antthinking)\\s*>",
                Pattern.CASE_INSENSITIVE);
        Matcher openM = openRe.matcher(text);
        List<int[]> opens = new ArrayList<>();
        while (openM.find())
            opens.add(new int[] { openM.start(), openM.end() });
        if (opens.isEmpty())
            return "";
        Matcher closeM = closeRe.matcher(text);
        int lastCloseEnd = -1;
        while (closeM.find())
            lastCloseEnd = closeM.start();
        int[] lastOpen = opens.get(opens.size() - 1);
        if (lastCloseEnd > lastOpen[0])
            return closed;
        return text.substring(lastOpen[1]).trim();
    }

    /** Split text with <think>/<thinking> tags into thinking+text blocks. */
    public sealed interface ThinkTagBlock permits ThinkingBlock, TextBlock {
    }

    public record ThinkingBlock(String thinking) implements ThinkTagBlock {
    }

    public record TextBlock(String text) implements ThinkTagBlock {
    }

    public static List<ThinkTagBlock> splitThinkingTaggedText(String text) {
        if (text == null)
            return null;
        String trimmedStart = text.stripLeading();
        if (!trimmedStart.startsWith("<"))
            return null;
        Pattern openRe = Pattern.compile("<\\s*(?:think(?:ing)?|thought|antthinking)\\s*>", Pattern.CASE_INSENSITIVE);
        Pattern closeRe = Pattern.compile("<\\s*/\\s*(?:think(?:ing)?|thought|antthinking)\\s*>",
                Pattern.CASE_INSENSITIVE);
        if (!openRe.matcher(trimmedStart).find())
            return null;
        if (!closeRe.matcher(text).find())
            return null;

        Matcher m = THINKING_TAG_RE.matcher(text);
        boolean inThinking = false;
        int cursor = 0, thinkingStart = 0;
        List<ThinkTagBlock> blocks = new ArrayList<>();
        while (m.find()) {
            boolean isClose = m.group(1).contains("/");
            if (!inThinking && !isClose) {
                String before = text.substring(cursor, m.start());
                if (!before.isEmpty())
                    blocks.add(new TextBlock(before));
                thinkingStart = m.end();
                inThinking = true;
            } else if (inThinking && isClose) {
                String thinking = text.substring(thinkingStart, m.start()).trim();
                if (!thinking.isEmpty())
                    blocks.add(new ThinkingBlock(thinking));
                cursor = m.end();
                inThinking = false;
            }
        }
        if (inThinking)
            return null;
        String tail = text.substring(cursor);
        if (!tail.isEmpty())
            blocks.add(new TextBlock(tail));
        boolean hasThinking = blocks.stream().anyMatch(b -> b instanceof ThinkingBlock);
        return hasThinking ? blocks : null;
    }

    // ── Assistant text extraction ────────────────────────────────────

    public static String extractAssistantText(List<Map<String, Object>> content) {
        if (content == null)
            return "";
        List<String> texts = new ArrayList<>();
        for (Map<String, Object> block : content) {
            if (!"text".equals(block.get("type")))
                continue;
            Object textObj = block.get("text");
            if (!(textObj instanceof String s))
                continue;
            String cleaned = stripThinkingTagsFromText(
                    stripDowngradedToolCallText(stripMinimaxToolCallXml(s))).trim();
            if (!cleaned.isEmpty())
                texts.add(cleaned);
        }
        return String.join("\n", texts).trim();
    }

    public static String extractAssistantThinking(List<Map<String, Object>> content) {
        if (content == null)
            return "";
        List<String> blocks = new ArrayList<>();
        for (Map<String, Object> block : content) {
            if ("thinking".equals(block.get("type"))) {
                Object thinkObj = block.get("thinking");
                if (thinkObj instanceof String s) {
                    String trimmed = s.trim();
                    if (!trimmed.isEmpty())
                        blocks.add(trimmed);
                }
            }
        }
        return String.join("\n", blocks).trim();
    }

    // ── Formatting ──────────────────────────────────────────────────

    public static String formatReasoningMessage(String text) {
        if (text == null)
            return "";
        String trimmed = text.trim();
        if (trimmed.isEmpty())
            return "";
        String[] lines = trimmed.split("\n", -1);
        StringBuilder sb = new StringBuilder("Reasoning:\n");
        for (String line : lines) {
            sb.append(line.isEmpty() ? line : "_" + line + "_").append("\n");
        }
        return sb.toString().stripTrailing();
    }
}
