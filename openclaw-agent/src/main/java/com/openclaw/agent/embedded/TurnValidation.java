package com.openclaw.agent.embedded;

import java.util.*;

/**
 * Conversation turn validation/merging for Gemini and Anthropic APIs.
 * Mirrors {@code agents/pi-embedded-helpers/turns.ts}.
 *
 * <p>
 * Both APIs require strict turn ordering; this utility merges consecutive
 * same-role messages to prevent rejections.
 * </p>
 *
 * <p>
 * Messages are represented as {@code Map<String, Object>} where fields
 * include "role", "content", "timestamp", "usage", "stopReason",
 * "errorMessage".
 * This avoids coupling to a specific AgentMessage class until the full core
 * message model is finalized.
 * </p>
 */
public final class TurnValidation {

    private TurnValidation() {
    }

    /**
     * Validate turns for Gemini: merge consecutive assistant messages.
     */
    public static List<Map<String, Object>> validateGeminiTurns(List<Map<String, Object>> messages) {
        if (messages == null || messages.isEmpty())
            return messages;
        List<Map<String, Object>> result = new ArrayList<>();
        String lastRole = null;

        for (Map<String, Object> msg : messages) {
            if (msg == null) {
                result.add(msg);
                continue;
            }
            String role = (String) msg.get("role");
            if (role == null) {
                result.add(msg);
                continue;
            }

            if ("assistant".equals(role) && "assistant".equals(lastRole) && !result.isEmpty()) {
                Map<String, Object> lastMsg = result.get(result.size() - 1);
                Map<String, Object> merged = mergeAssistantMessages(lastMsg, msg);
                result.set(result.size() - 1, merged);
                continue;
            }

            result.add(msg);
            lastRole = role;
        }
        return result;
    }

    /**
     * Validate turns for Anthropic: merge consecutive user messages.
     */
    public static List<Map<String, Object>> validateAnthropicTurns(List<Map<String, Object>> messages) {
        if (messages == null || messages.isEmpty())
            return messages;
        List<Map<String, Object>> result = new ArrayList<>();
        String lastRole = null;

        for (Map<String, Object> msg : messages) {
            if (msg == null) {
                result.add(msg);
                continue;
            }
            String role = (String) msg.get("role");
            if (role == null) {
                result.add(msg);
                continue;
            }

            if ("user".equals(role) && "user".equals(lastRole) && !result.isEmpty()) {
                Map<String, Object> lastMsg = result.get(result.size() - 1);
                Map<String, Object> merged = mergeUserMessages(lastMsg, msg);
                result.set(result.size() - 1, merged);
                continue;
            }

            result.add(msg);
            lastRole = role;
        }
        return result;
    }

    /**
     * Merge consecutive user turns: concatenate content arrays.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> mergeUserMessages(
            Map<String, Object> previous, Map<String, Object> current) {
        List<Object> merged = new ArrayList<>();
        Object prev = previous.get("content");
        Object curr = current.get("content");
        if (prev instanceof List)
            merged.addAll((List<Object>) prev);
        if (curr instanceof List)
            merged.addAll((List<Object>) curr);

        Map<String, Object> result = new LinkedHashMap<>(current);
        result.put("content", merged);
        if (!result.containsKey("timestamp") && previous.containsKey("timestamp")) {
            result.put("timestamp", previous.get("timestamp"));
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> mergeAssistantMessages(
            Map<String, Object> previous, Map<String, Object> current) {
        List<Object> merged = new ArrayList<>();
        Object prev = previous.get("content");
        Object curr = current.get("content");
        if (prev instanceof List)
            merged.addAll((List<Object>) prev);
        if (curr instanceof List)
            merged.addAll((List<Object>) curr);

        Map<String, Object> result = new LinkedHashMap<>(previous);
        result.put("content", merged);
        // Later message overrides these fields
        if (current.containsKey("usage"))
            result.put("usage", current.get("usage"));
        if (current.containsKey("stopReason"))
            result.put("stopReason", current.get("stopReason"));
        if (current.containsKey("errorMessage"))
            result.put("errorMessage", current.get("errorMessage"));
        return result;
    }
}
