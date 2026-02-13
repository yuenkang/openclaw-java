package com.openclaw.gateway.chat;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Chat message sanitizer — strips envelope headers and message ID hints.
 * Corresponds to TypeScript's chat-sanitize.ts.
 */
public final class ChatSanitizer {

    private ChatSanitizer() {
    }

    private static final Pattern ENVELOPE_PREFIX = Pattern.compile("^\\[([^\\]]+)\\]\\s*");

    private static final Set<String> ENVELOPE_CHANNELS = Set.of(
            "WebChat", "WhatsApp", "Telegram", "Signal", "Slack", "Discord",
            "Google Chat", "iMessage", "Teams", "Matrix", "Zalo", "Zalo Personal", "BlueBubbles");

    private static final Pattern MESSAGE_ID_LINE = Pattern.compile(
            "^\\s*\\[message_id:\\s*[^\\]]+\\]\\s*$", Pattern.CASE_INSENSITIVE);

    private static final Pattern TIMESTAMP_ISO = Pattern.compile("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}Z\\b");
    private static final Pattern TIMESTAMP_SIMPLE = Pattern.compile("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}\\b");

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Strip envelope prefix (e.g. [WebChat 2024-01-01T00:00Z]) from user message
     * text.
     */
    public static String stripEnvelope(String text) {
        Matcher m = ENVELOPE_PREFIX.matcher(text);
        if (!m.find())
            return text;
        String header = m.group(1);
        if (header == null || !looksLikeEnvelopeHeader(header))
            return text;
        return text.substring(m.end());
    }

    /**
     * Strip [message_id: ...] lines from text.
     */
    public static String stripMessageIdHints(String text) {
        if (!text.contains("[message_id:"))
            return text;
        String[] lines = text.split("\\r?\\n", -1);
        List<String> filtered = new ArrayList<>();
        boolean changed = false;
        for (String line : lines) {
            if (MESSAGE_ID_LINE.matcher(line).matches()) {
                changed = true;
            } else {
                filtered.add(line);
            }
        }
        return changed ? String.join("\n", filtered) : text;
    }

    /**
     * Strip envelope and message ID hints from a single message map.
     * Only processes messages with role="user".
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> stripEnvelopeFromMessage(Map<String, Object> message) {
        if (message == null)
            return message;
        String role = message.get("role") instanceof String r ? r.toLowerCase() : "";
        if (!"user".equals(role))
            return message;

        Object content = message.get("content");
        boolean changed = false;
        Map<String, Object> next = new java.util.LinkedHashMap<>(message);

        if (content instanceof String text) {
            String stripped = stripMessageIdHints(stripEnvelope(text));
            if (!stripped.equals(text)) {
                next.put("content", stripped);
                changed = true;
            }
        } else if (content instanceof List<?> contentList) {
            var result = stripEnvelopeFromContent((List<Map<String, Object>>) contentList);
            if (result.changed) {
                next.put("content", result.content);
                changed = true;
            }
        } else {
            Object text = message.get("text");
            if (text instanceof String textStr) {
                String stripped = stripMessageIdHints(stripEnvelope(textStr));
                if (!stripped.equals(textStr)) {
                    next.put("text", stripped);
                    changed = true;
                }
            }
        }

        return changed ? next : message;
    }

    /**
     * Strip envelope from all messages in a list.
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> stripEnvelopeFromMessages(List<Map<String, Object>> messages) {
        if (messages == null || messages.isEmpty())
            return messages;
        boolean changed = false;
        List<Map<String, Object>> next = new ArrayList<>(messages.size());
        for (var msg : messages) {
            var stripped = stripEnvelopeFromMessage(msg);
            if (stripped != msg)
                changed = true;
            next.add(stripped);
        }
        return changed ? next : messages;
    }

    // =========================================================================
    // Internal
    // =========================================================================

    private static boolean looksLikeEnvelopeHeader(String header) {
        if (TIMESTAMP_ISO.matcher(header).find())
            return true;
        if (TIMESTAMP_SIMPLE.matcher(header).find())
            return true;
        return ENVELOPE_CHANNELS.stream().anyMatch(label -> header.startsWith(label + " "));
    }

    private record ContentStripResult(List<Map<String, Object>> content, boolean changed) {
    }

    @SuppressWarnings("unchecked")
    private static ContentStripResult stripEnvelopeFromContent(List<Map<String, Object>> content) {
        boolean changed = false;
        List<Map<String, Object>> next = new ArrayList<>(content.size());
        for (var item : content) {
            if (item == null || !"text".equals(item.get("type")) || !(item.get("text") instanceof String text)) {
                next.add(item);
                continue;
            }
            String stripped = stripMessageIdHints(stripEnvelope(text));
            if (stripped.equals(text)) {
                next.add(item);
            } else {
                var copy = new java.util.LinkedHashMap<>(item);
                copy.put("text", stripped);
                next.add(copy);
                changed = true;
            }
        }
        return new ContentStripResult(next, changed);
    }
}
