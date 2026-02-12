package com.openclaw.agent.runtime;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import lombok.extern.slf4j.Slf4j;

/**
 * Conversation turn validation and fixing for provider APIs.
 * Corresponds to TypeScript's pi-embedded-helpers/turns.ts.
 *
 * <p>
 * Each provider enforces specific turn ordering rules:
 * <ul>
 * <li>Gemini: strict alternating, merges consecutive assistant messages</li>
 * <li>Anthropic: strict alternating, merges consecutive user messages</li>
 * </ul>
 */
@Slf4j
public final class TurnValidator {

    private TurnValidator() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * Validates and fixes conversation turn sequences for Gemini API.
     * Gemini requires strict alternating user→assistant pattern.
     * Merges consecutive assistant messages together.
     */
    public static List<JsonNode> validateGeminiTurns(List<JsonNode> messages) {
        if (messages == null || messages.isEmpty()) {
            return messages;
        }

        List<JsonNode> result = new ArrayList<>();
        String lastRole = null;

        for (JsonNode msg : messages) {
            if (msg == null || !msg.isObject()) {
                result.add(msg);
                continue;
            }
            String role = msg.has("role") ? msg.get("role").asText() : null;
            if (role == null) {
                result.add(msg);
                continue;
            }

            if ("assistant".equals(role) && "assistant".equals(lastRole) && !result.isEmpty()) {
                // Merge consecutive assistant messages
                JsonNode lastMsg = result.get(result.size() - 1);
                JsonNode merged = mergeAssistantMessages(lastMsg, msg);
                result.set(result.size() - 1, merged);
                continue;
            }

            result.add(msg);
            lastRole = role;
        }

        return result;
    }

    /**
     * Validates and fixes conversation turn sequences for Anthropic API.
     * Anthropic requires strict alternating user→assistant pattern.
     * Merges consecutive user messages together.
     */
    public static List<JsonNode> validateAnthropicTurns(List<JsonNode> messages) {
        if (messages == null || messages.isEmpty()) {
            return messages;
        }

        List<JsonNode> result = new ArrayList<>();
        String lastRole = null;

        for (JsonNode msg : messages) {
            if (msg == null || !msg.isObject()) {
                result.add(msg);
                continue;
            }
            String role = msg.has("role") ? msg.get("role").asText() : null;
            if (role == null) {
                result.add(msg);
                continue;
            }

            if ("user".equals(role) && "user".equals(lastRole) && !result.isEmpty()) {
                // Merge consecutive user messages
                JsonNode lastMsg = result.get(result.size() - 1);
                JsonNode merged = mergeUserMessages(lastMsg, msg);
                result.set(result.size() - 1, merged);
                continue;
            }

            result.add(msg);
            lastRole = role;
        }

        return result;
    }

    /**
     * Validate turns based on the given provider type.
     *
     * @param messages message list
     * @param provider provider id (e.g. "anthropic", "google", "openai")
     * @return validated/fixed message list
     */
    public static List<JsonNode> validateTurnsForProvider(List<JsonNode> messages, String provider) {
        if (provider == null) {
            return messages;
        }
        String normalized = provider.toLowerCase();
        if (normalized.contains("google") || normalized.contains("gemini")) {
            return validateGeminiTurns(messages);
        }
        if (normalized.contains("anthropic") || normalized.contains("claude")) {
            return validateAnthropicTurns(messages);
        }
        // OpenAI and others: no strict turn ordering needed
        return messages;
    }

    // ── Merge helpers ─────────────────────────────────────────────────

    /**
     * Merge two consecutive assistant messages by combining their content arrays.
     */
    private static JsonNode mergeAssistantMessages(JsonNode previous, JsonNode current) {
        ObjectNode merged = previous.deepCopy();
        ArrayNode mergedContent = mergeContentArrays(
                previous.get("content"), current.get("content"));
        merged.set("content", mergedContent);

        // Take latest usage/stopReason/errorMessage if present
        if (current.has("usage")) {
            merged.set("usage", current.get("usage"));
        }
        if (current.has("stopReason")) {
            merged.set("stopReason", current.get("stopReason"));
        }
        if (current.has("errorMessage")) {
            merged.set("errorMessage", current.get("errorMessage"));
        }
        return merged;
    }

    /**
     * Merge two consecutive user messages by combining their content arrays.
     */
    private static JsonNode mergeUserMessages(JsonNode previous, JsonNode current) {
        ObjectNode merged = current.deepCopy();
        ArrayNode mergedContent = mergeContentArrays(
                previous.get("content"), current.get("content"));
        merged.set("content", mergedContent);

        // Preserve earliest timestamp
        if (!current.has("timestamp") && previous.has("timestamp")) {
            merged.set("timestamp", previous.get("timestamp"));
        }
        return merged;
    }

    /**
     * Merge two content arrays (or nulls) into a single array.
     */
    private static ArrayNode mergeContentArrays(JsonNode a, JsonNode b) {
        ArrayNode result = MAPPER.createArrayNode();
        if (a != null && a.isArray()) {
            for (JsonNode item : a) {
                result.add(item);
            }
        }
        if (b != null && b.isArray()) {
            for (JsonNode item : b) {
                result.add(item);
            }
        }
        return result;
    }
}
