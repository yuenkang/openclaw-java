package com.openclaw.agent.runtime.subscribe;

import com.openclaw.agent.runtime.subscribe.HandlerTypes.MessagingToolSend;

import java.util.List;
import java.util.Map;

/**
 * Tool result sanitization, extraction, and error detection.
 * Corresponds to TypeScript pi-embedded-subscribe.tools.ts.
 */
public final class SubscribeToolUtils {

    private SubscribeToolUtils() {
    }

    public static final int TOOL_RESULT_MAX_CHARS = 8000;
    public static final int TOOL_ERROR_MAX_CHARS = 400;

    /** Truncate text with UTF-16 safety and append truncation marker. */
    public static String truncateToolText(String text) {
        if (text == null || text.length() <= TOOL_RESULT_MAX_CHARS)
            return text;
        return text.substring(0, TOOL_RESULT_MAX_CHARS) + "\n…(truncated)…";
    }

    /** Normalize tool error text to first line, truncated. */
    public static String normalizeToolErrorText(String text) {
        if (text == null)
            return null;
        String trimmed = text.trim();
        if (trimmed.isEmpty())
            return null;
        String firstLine = trimmed.split("\\r?\\n", 2)[0].trim();
        if (firstLine.isEmpty())
            return null;
        return firstLine.length() > TOOL_ERROR_MAX_CHARS
                ? firstLine.substring(0, TOOL_ERROR_MAX_CHARS) + "…"
                : firstLine;
    }

    /** Sanitize a tool result: truncate text blocks, strip image data. */
    @SuppressWarnings("unchecked")
    public static Object sanitizeToolResult(Object result) {
        if (!(result instanceof Map<?, ?> record))
            return result;
        Map<String, Object> map = (Map<String, Object>) record;
        Object contentObj = map.get("content");
        if (!(contentObj instanceof List<?> content))
            return map;
        List<Object> sanitized = content.stream().map(item -> {
            if (!(item instanceof Map<?, ?> entry))
                return item;
            Map<String, Object> entryMap = (Map<String, Object>) entry;
            String type = entryMap.get("type") instanceof String s ? s : null;
            if ("text".equals(type) && entryMap.get("text") instanceof String text) {
                return new java.util.LinkedHashMap<>(Map.of(
                        "type", "text", "text", truncateToolText(text)));
            }
            if ("image".equals(type)) {
                Map<String, Object> cleaned = new java.util.LinkedHashMap<>(entryMap);
                Object data = cleaned.remove("data");
                int bytes = data instanceof String s ? s.length() : 0;
                cleaned.put("bytes", bytes);
                cleaned.put("omitted", true);
                return cleaned;
            }
            return entryMap;
        }).toList();
        Map<String, Object> out = new java.util.LinkedHashMap<>(map);
        out.put("content", sanitized);
        return out;
    }

    /** Extract text from tool result content blocks. */
    @SuppressWarnings("unchecked")
    public static String extractToolResultText(Object result) {
        if (!(result instanceof Map<?, ?> record))
            return null;
        Object contentObj = ((Map<String, Object>) record).get("content");
        if (!(contentObj instanceof List<?> content))
            return null;
        List<String> texts = content.stream()
                .filter(item -> item instanceof Map)
                .map(item -> (Map<String, Object>) item)
                .filter(m -> "text".equals(m.get("type")) && m.get("text") instanceof String)
                .map(m -> ((String) m.get("text")).trim())
                .filter(s -> !s.isEmpty())
                .toList();
        return texts.isEmpty() ? null : String.join("\n", texts);
    }

    /** Check if tool result indicates an error. */
    @SuppressWarnings("unchecked")
    public static boolean isToolResultError(Object result) {
        if (!(result instanceof Map<?, ?> record))
            return false;
        Object details = ((Map<String, Object>) record).get("details");
        if (!(details instanceof Map<?, ?> detailsMap))
            return false;
        Object status = ((Map<String, Object>) detailsMap).get("status");
        if (!(status instanceof String s))
            return false;
        String normalized = s.trim().toLowerCase();
        return "error".equals(normalized) || "timeout".equals(normalized);
    }

    /** Extract error message from tool result. */
    @SuppressWarnings("unchecked")
    public static String extractToolErrorMessage(Object result) {
        if (!(result instanceof Map<?, ?> record))
            return null;
        Map<String, Object> map = (Map<String, Object>) record;
        String fromDetails = extractErrorField(map.get("details"));
        if (fromDetails != null)
            return fromDetails;
        String fromRoot = extractErrorField(map);
        if (fromRoot != null)
            return fromRoot;
        String text = extractToolResultText(result);
        if (text == null)
            return null;
        try {
            Object parsed = new com.fasterxml.jackson.databind.ObjectMapper().readValue(text, Object.class);
            String fromJson = extractErrorField(parsed);
            if (fromJson != null)
                return fromJson;
        } catch (Exception ignored) {
        }
        return normalizeToolErrorText(text);
    }

    @SuppressWarnings("unchecked")
    private static String extractErrorField(Object value) {
        if (!(value instanceof Map<?, ?> record))
            return null;
        Map<String, Object> map = (Map<String, Object>) record;
        String fromError = readErrorCandidate(map.get("error"));
        if (fromError != null)
            return fromError;
        String fromMessage = readErrorCandidate(map.get("message"));
        if (fromMessage != null)
            return fromMessage;
        String fromReason = readErrorCandidate(map.get("reason"));
        if (fromReason != null)
            return fromReason;
        Object status = map.get("status");
        return status instanceof String s ? normalizeToolErrorText(s) : null;
    }

    @SuppressWarnings("unchecked")
    private static String readErrorCandidate(Object value) {
        if (value instanceof String s)
            return normalizeToolErrorText(s);
        if (!(value instanceof Map<?, ?> record))
            return null;
        Map<String, Object> map = (Map<String, Object>) record;
        if (map.get("message") instanceof String s)
            return normalizeToolErrorText(s);
        if (map.get("error") instanceof String s)
            return normalizeToolErrorText(s);
        return null;
    }

    /** Extract messaging tool send info from args. */
    public static MessagingToolSend extractMessagingToolSend(String toolName, Map<String, Object> args) {
        String action = args.get("action") instanceof String s ? s.trim() : "";
        String accountId = args.get("accountId") instanceof String s && !s.isBlank() ? s.trim() : null;
        if ("message".equals(toolName)) {
            if (!"send".equals(action) && !"thread-reply".equals(action))
                return null;
            String to = args.get("to") instanceof String s ? s : null;
            if (to == null)
                return null;
            String providerHint = args.get("provider") instanceof String s ? s.trim() : "";
            if (providerHint.isEmpty())
                providerHint = args.get("channel") instanceof String s ? s.trim() : "";
            String provider = providerHint.isEmpty() ? "message" : providerHint.toLowerCase();
            return new MessagingToolSend(toolName, provider, accountId, to);
        }
        // TODO: provider plugin docking
        return null;
    }

    /** Normalize text for duplicate comparison. */
    public static String normalizeTextForComparison(String text) {
        if (text == null)
            return "";
        return text.trim().replaceAll("\\s+", " ").toLowerCase();
    }

    /** Check if text is a duplicate of already-sent messaging tool texts. */
    public static boolean isMessagingToolDuplicateNormalized(
            String normalizedText, List<String> sentNormalized) {
        if (normalizedText == null || normalizedText.isEmpty())
            return false;
        for (String sent : sentNormalized) {
            if (normalizedText.equals(sent))
                return true;
            if (sent.contains(normalizedText) || normalizedText.contains(sent))
                return true;
        }
        return false;
    }
}
