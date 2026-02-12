package com.openclaw.agent.embedded;

import java.util.*;

/**
 * Session message image sanitization utilities.
 * Mirrors {@code agents/pi-embedded-helpers/images.ts}.
 *
 * <p>
 * Sanitizes oversized base64 images, strips thought signatures, and cleans
 * tool call IDs for provider compatibility in historical session transcripts.
 * </p>
 */
public final class ImageSanitizer {

    private ImageSanitizer() {
    }

    /**
     * Check if an assistant message has empty or whitespace-only text content.
     */
    @SuppressWarnings("unchecked")
    public static boolean isEmptyAssistantMessageContent(Map<String, Object> message) {
        if (message == null)
            return true;
        Object content = message.get("content");
        if (content == null)
            return true;
        if (!(content instanceof List<?> list))
            return false;
        return list.stream().allMatch(block -> {
            if (block == null)
                return true;
            if (!(block instanceof Map<?, ?> map))
                return true;
            Object type = map.get("type");
            if (!"text".equals(type))
                return false;
            Object text = map.get("text");
            return !(text instanceof String s) || s.trim().isEmpty();
        });
    }

    /**
     * Sanitize session messages for provider compatibility.
     * <p>
     * Performs the following on historical transcript messages:
     * </p>
     * <ul>
     * <li>Strips oversized images from tool results, user messages, and error-stop
     * assistant messages</li>
     * <li>Optionally strips thought signatures (Claude → Gemini
     * cross-provider)</li>
     * <li>Filters empty text blocks from assistant messages</li>
     * <li>Skips assistant messages that become empty after filtering</li>
     * </ul>
     *
     * @param messages        list of agent messages as maps
     * @param label           label for logging
     * @param fullSanitize    if true, also sanitize tool-call IDs and thought
     *                        signatures
     * @param stripSignatures if true, strip non-base64 thought signatures
     * @return sanitized copy of the message list
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> sanitizeSessionMessagesImages(
            List<Map<String, Object>> messages,
            String label,
            boolean fullSanitize,
            boolean stripSignatures) {
        if (messages == null)
            return messages;
        List<Map<String, Object>> out = new ArrayList<>();

        for (Map<String, Object> msg : messages) {
            if (msg == null) {
                out.add(null);
                continue;
            }
            String role = (String) msg.get("role");
            if (role == null) {
                out.add(msg);
                continue;
            }

            switch (role) {
                case "toolResult" -> {
                    // Sanitize images in tool result content
                    out.add(sanitizeContentBlocks(msg, label));
                }
                case "user" -> {
                    Object content = msg.get("content");
                    if (content instanceof List<?>) {
                        out.add(sanitizeContentBlocks(msg, label));
                    } else {
                        out.add(msg);
                    }
                }
                case "assistant" -> {
                    Map<String, Object> sanitized = processAssistant(msg, label,
                            fullSanitize, stripSignatures);
                    if (sanitized != null)
                        out.add(sanitized);
                }
                default -> out.add(msg);
            }
        }
        return out;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> processAssistant(
            Map<String, Object> msg, String label,
            boolean fullSanitize, boolean stripSignatures) {
        Object contentObj = msg.get("content");
        if (!(contentObj instanceof List<?> contentList))
            return msg;
        List<Map<String, Object>> content = (List<Map<String, Object>>) contentList;

        // If error-stop, just sanitize images
        if ("error".equals(msg.get("stopReason"))) {
            return sanitizeContentBlocks(msg, label);
        }

        List<Map<String, Object>> processed = content;

        // Strip thought signatures if requested
        if (stripSignatures && fullSanitize) {
            processed = BootstrapHelpers.stripThoughtSignatures(processed, false, false);
        }

        // Filter empty text blocks (full sanitize mode only)
        if (fullSanitize) {
            List<Map<String, Object>> filtered = new ArrayList<>();
            for (Map<String, Object> block : processed) {
                if (block == null)
                    continue;
                if ("text".equals(block.get("type"))) {
                    Object text = block.get("text");
                    if (text instanceof String s && s.trim().isEmpty())
                        continue;
                }
                filtered.add(block);
            }
            processed = filtered;
        }

        if (processed.isEmpty())
            return null;

        Map<String, Object> result = new LinkedHashMap<>(msg);
        result.put("content", processed);
        return result;
    }

    /**
     * Placeholder image sanitization — replaces oversized base64 images with a text
     * marker.
     * In the full implementation, this would resize/compress images.
     */
    @SuppressWarnings("unchecked")
    private static Map<String, Object> sanitizeContentBlocks(Map<String, Object> msg, String label) {
        Object contentObj = msg.get("content");
        if (!(contentObj instanceof List<?> list))
            return msg;
        List<Map<String, Object>> blocks = (List<Map<String, Object>>) list;
        boolean changed = false;
        List<Map<String, Object>> result = new ArrayList<>(blocks.size());

        for (Map<String, Object> block : blocks) {
            if (block == null) {
                result.add(null);
                continue;
            }
            Object type = block.get("type");
            if ("image".equals(type) || "image_url".equals(type)) {
                Object data = block.get("data");
                Object source = block.get("source");
                // Check for oversized base64 data (> 10MB heuristic)
                if (data instanceof String s && s.length() > 10_000_000) {
                    Map<String, Object> placeholder = new LinkedHashMap<>();
                    placeholder.put("type", "text");
                    placeholder.put("text", "[image too large, removed from transcript]");
                    result.add(placeholder);
                    changed = true;
                    continue;
                }
            }
            result.add(block);
        }

        if (!changed)
            return msg;
        Map<String, Object> copy = new LinkedHashMap<>(msg);
        copy.put("content", result);
        return copy;
    }
}
