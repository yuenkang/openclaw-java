package com.openclaw.agent.runtime;

import java.util.ArrayList;
import java.util.HashSet;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Pattern;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import lombok.extern.slf4j.Slf4j;

/**
 * Session history sanitization for cross-provider compatibility.
 * Corresponds to parts of TypeScript's pi-embedded-runner/google.ts
 * and pi-embedded-helpers (images, sanitize-session-messages, tool-call-id).
 *
 * <p>
 * Responsibilities:
 * <ul>
 * <li>Normalize tool call IDs (ensure uniqueness, truncate invalid)</li>
 * <li>Clean stale/invalid image references</li>
 * <li>Strip thinking block signatures from Google sessions</li>
 * <li>Remove empty assistant text blocks</li>
 * <li>Apply Google turn ordering fix (prepend dummy user turn if needed)</li>
 * </ul>
 */
@Slf4j
public final class SessionSanitizer {

    private SessionSanitizer() {
    }

    private static final Pattern TOOL_CALL_ID_VALID_RE = Pattern.compile("^[a-zA-Z0-9_-]{1,64}$");

    // Anthropic requires tool call IDs matching this pattern
    private static final Pattern ANTHROPIC_TOOL_ID_RE = Pattern.compile("^toolu_[A-Za-z0-9]{20,}$");

    private static final Pattern THINKING_SIGNATURE_RE = Pattern.compile("^[A-Za-z0-9+/]+={0,2}$");

    // ── Main sanitize entry point ─────────────────────────────────────

    /**
     * Sanitize session history for a given provider.
     * Applies all relevant normalizations.
     *
     * @param messages raw session messages
     * @param provider provider id (e.g. "anthropic", "google", "openai")
     * @param modelApi optional model API type
     * @return sanitized messages
     */
    public static List<JsonNode> sanitizeSessionHistory(
            List<JsonNode> messages, String provider, String modelApi) {
        if (messages == null || messages.isEmpty()) {
            return messages;
        }

        List<JsonNode> result = new ArrayList<>();
        for (JsonNode msg : messages) {
            if (msg == null) {
                continue;
            }
            result.add(msg.deepCopy());
        }

        // 1. Normalize tool call IDs
        result = normalizeToolCallIds(result);

        // 2. Strip thinking signatures for Google
        String normalizedProvider = provider != null ? provider.toLowerCase() : "";
        if (normalizedProvider.contains("google") || normalizedProvider.contains("gemini")) {
            result = stripThinkingSignatures(result);
        }

        // 3. Remove empty assistant text blocks
        result = removeEmptyAssistantBlocks(result);

        // 4. Validate turns for provider
        result = TurnValidator.validateTurnsForProvider(result, provider);

        // 5. Google: ensure first message is from user
        if (normalizedProvider.contains("google") || normalizedProvider.contains("gemini")) {
            result = ensureFirstMessageIsUser(result);
        }

        return result;
    }

    // ── Tool call ID normalization ────────────────────────────────────

    /**
     * Ensure tool call IDs are valid and unique.
     * Truncates or replaces invalid IDs.
     */
    public static List<JsonNode> normalizeToolCallIds(List<JsonNode> messages) {
        Set<String> seenIds = new HashSet<>();

        for (JsonNode msg : messages) {
            if (!msg.isObject() || !msg.has("content") || !msg.get("content").isArray()) {
                continue;
            }

            ArrayNode content = (ArrayNode) msg.get("content");
            for (int i = 0; i < content.size(); i++) {
                JsonNode block = content.get(i);
                if (!block.isObject()) {
                    continue;
                }

                String type = block.has("type") ? block.get("type").asText() : "";

                // tool_use / tool_call blocks
                if ("tool_use".equals(type) || "tool_call".equals(type)) {
                    normalizeBlockId(block, "id", seenIds);
                }

                // tool_result blocks
                if ("tool_result".equals(type)) {
                    normalizeBlockId(block, "tool_use_id", seenIds);
                }
            }
        }

        return messages;
    }

    private static void normalizeBlockId(JsonNode block, String idField, Set<String> seenIds) {
        if (!block.isObject()) {
            return;
        }
        ObjectNode obj = (ObjectNode) block;
        String id = obj.has(idField) ? obj.get(idField).asText() : null;

        if (id == null || id.isBlank() || !TOOL_CALL_ID_VALID_RE.matcher(id).matches()) {
            // Generate a new valid ID
            String newId = "toolu_" + UUID.randomUUID().toString().replace("-", "").substring(0, 24);
            obj.put(idField, newId);
            seenIds.add(newId);
            log.debug("Replaced invalid tool call id '{}' → '{}'", id, newId);
        } else if (seenIds.contains(id)) {
            // Duplicate — generate new
            String newId = id + "_" + UUID.randomUUID().toString().replace("-", "").substring(0, 8);
            if (newId.length() > 64) {
                newId = newId.substring(0, 64);
            }
            obj.put(idField, newId);
            seenIds.add(newId);
            log.debug("Deduplicated tool call id '{}' → '{}'", id, newId);
        } else {
            seenIds.add(id);
        }
    }

    // ── Thinking signature stripping ──────────────────────────────────

    /**
     * Strip base64 thinking block signatures from assistant messages.
     * Google models sometimes emit these.
     */
    public static List<JsonNode> stripThinkingSignatures(List<JsonNode> messages) {
        for (JsonNode msg : messages) {
            if (!msg.isObject() || !"assistant".equals(
                    msg.has("role") ? msg.get("role").asText() : null)) {
                continue;
            }
            if (!msg.has("content") || !msg.get("content").isArray()) {
                continue;
            }

            ArrayNode content = (ArrayNode) msg.get("content");
            List<Integer> toRemove = new ArrayList<>();

            for (int i = 0; i < content.size(); i++) {
                JsonNode block = content.get(i);
                if (!block.isObject()) {
                    continue;
                }
                String type = block.has("type") ? block.get("type").asText() : "";
                if ("thinking".equals(type) && block.has("thinking")) {
                    String thinking = block.get("thinking").asText("");
                    // If it's just a base64 signature, mark for removal
                    if (!thinking.isBlank() && THINKING_SIGNATURE_RE.matcher(thinking.trim()).matches()
                            && thinking.trim().length() >= 20) {
                        toRemove.add(i);
                    }
                }
            }

            // Remove in reverse order to preserve indices
            for (int i = toRemove.size() - 1; i >= 0; i--) {
                content.remove(toRemove.get(i).intValue());
            }
        }
        return messages;
    }

    // ── Empty assistant block removal ─────────────────────────────────

    /**
     * Remove empty text blocks from assistant messages.
     * Preserves at least one content block per assistant message.
     */
    public static List<JsonNode> removeEmptyAssistantBlocks(List<JsonNode> messages) {
        for (JsonNode msg : messages) {
            if (!msg.isObject() || !"assistant".equals(
                    msg.has("role") ? msg.get("role").asText() : null)) {
                continue;
            }
            if (!msg.has("content") || !msg.get("content").isArray()) {
                continue;
            }

            ArrayNode content = (ArrayNode) msg.get("content");
            if (content.size() <= 1) {
                continue; // Never remove the last block
            }

            List<Integer> emptyTextIndices = new ArrayList<>();
            int nonEmptyCount = 0;

            for (int i = 0; i < content.size(); i++) {
                JsonNode block = content.get(i);
                if (!block.isObject()) {
                    nonEmptyCount++;
                    continue;
                }
                String type = block.has("type") ? block.get("type").asText() : "";
                if ("text".equals(type)) {
                    String text = block.has("text") ? block.get("text").asText("") : "";
                    if (text.isBlank()) {
                        emptyTextIndices.add(i);
                    } else {
                        nonEmptyCount++;
                    }
                } else {
                    nonEmptyCount++;
                }
            }

            // Only remove empty blocks if there are non-empty ones
            if (nonEmptyCount > 0) {
                for (int i = emptyTextIndices.size() - 1; i >= 0; i--) {
                    content.remove(emptyTextIndices.get(i).intValue());
                }
            }
        }
        return messages;
    }

    // ── Google turn ordering ──────────────────────────────────────────

    /**
     * Ensure the first message is from "user" (Google requirement).
     * If the first message is from "assistant", prepend a synthetic user message.
     */
    public static List<JsonNode> ensureFirstMessageIsUser(List<JsonNode> messages) {
        if (messages.isEmpty()) {
            return messages;
        }

        JsonNode first = messages.get(0);
        if (first.isObject() && first.has("role") && "user".equals(first.get("role").asText())) {
            return messages; // already fine
        }

        // Prepend a synthetic user message
        com.fasterxml.jackson.databind.ObjectMapper mapper = new com.fasterxml.jackson.databind.ObjectMapper();
        ObjectNode synthetic = mapper.createObjectNode();
        synthetic.put("role", "user");
        ArrayNode contentArr = mapper.createArrayNode();
        ObjectNode textBlock = mapper.createObjectNode();
        textBlock.put("type", "text");
        textBlock.put("text", "(conversation context follows)");
        contentArr.add(textBlock);
        synthetic.set("content", contentArr);

        List<JsonNode> result = new ArrayList<>();
        result.add(synthetic);
        result.addAll(messages);

        log.debug("Prepended synthetic user message for Google turn ordering");
        return result;
    }
}
