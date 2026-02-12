package com.openclaw.agent.runtime;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

/**
 * OpenAI-specific message processing helpers.
 * Corresponds to TypeScript pi-embedded-helpers/openai.ts.
 *
 * <p>
 * Handles downgrading/cleaning of OpenAI reasoning blocks that may cause
 * API rejections when replayed in conversation history.
 * </p>
 */
@Slf4j
public final class OpenAIHelpers {

    private OpenAIHelpers() {
    }

    /**
     * Parse an OpenAI reasoning signature from a thinking block's thinkingSignature
     * field.
     * Valid signatures have format {@code {"id":"rs_xxx","type":"reasoning..."}}.
     *
     * @return Two-element array [id, type] or null if not a valid reasoning
     *         signature
     */
    static String[] parseReasoningSignature(JsonNode value) {
        if (value == null || value.isNull())
            return null;

        JsonNode obj = value;
        if (value.isTextual()) {
            String text = value.asText().trim();
            if (!text.startsWith("{") || !text.endsWith("}"))
                return null;
            try {
                obj = new com.fasterxml.jackson.databind.ObjectMapper().readTree(text);
            } catch (Exception e) {
                return null;
            }
        }

        if (!obj.isObject())
            return null;

        String id = obj.has("id") && obj.get("id").isTextual() ? obj.get("id").asText() : "";
        String type = obj.has("type") && obj.get("type").isTextual() ? obj.get("type").asText() : "";

        if (!id.startsWith("rs_"))
            return null;
        if ("reasoning".equals(type) || type.startsWith("reasoning.")) {
            return new String[] { id, type };
        }
        return null;
    }

    /**
     * Check if there is a non-thinking content block after the given index.
     */
    static boolean hasFollowingNonThinkingBlock(ArrayNode content, int index) {
        for (int i = index + 1; i < content.size(); i++) {
            JsonNode block = content.get(i);
            if (block == null || !block.isObject())
                return true;
            if (!"thinking".equals(block.path("type").asText("")))
                return true;
        }
        return false;
    }

    /**
     * Downgrade OpenAI reasoning blocks in conversation history.
     *
     * <p>
     * OpenAI Responses API can reject transcripts that contain standalone reasoning
     * item ids without the required following item. This method strips such blocks
     * to keep history usable.
     * </p>
     *
     * @param messages Conversation history as JSON array elements
     * @return Cleaned messages with problematic reasoning blocks removed
     */
    public static List<JsonNode> downgradeReasoningBlocks(List<JsonNode> messages) {
        List<JsonNode> out = new ArrayList<>(messages.size());

        for (JsonNode msg : messages) {
            if (msg == null || !msg.isObject()) {
                out.add(msg);
                continue;
            }

            String role = msg.path("role").asText("");
            if (!"assistant".equals(role)) {
                out.add(msg);
                continue;
            }

            JsonNode contentNode = msg.get("content");
            if (contentNode == null || !contentNode.isArray()) {
                out.add(msg);
                continue;
            }

            ArrayNode content = (ArrayNode) contentNode;
            boolean changed = false;
            ArrayNode nextContent = content.arrayNode();

            for (int i = 0; i < content.size(); i++) {
                JsonNode block = content.get(i);
                if (block == null || !block.isObject()) {
                    nextContent.add(block);
                    continue;
                }

                if (!"thinking".equals(block.path("type").asText(""))) {
                    nextContent.add(block);
                    continue;
                }

                String[] signature = parseReasoningSignature(block.get("thinkingSignature"));
                if (signature == null) {
                    nextContent.add(block);
                    continue;
                }

                if (hasFollowingNonThinkingBlock(content, i)) {
                    nextContent.add(block);
                    continue;
                }

                // Drop this problematic reasoning block
                changed = true;
                log.debug("Dropping orphaned OpenAI reasoning block: id={}", signature[0]);
            }

            if (!changed) {
                out.add(msg);
                continue;
            }

            if (nextContent.isEmpty()) {
                // Entire message was reasoning blocks — skip it
                continue;
            }

            ObjectNode cleaned = ((ObjectNode) msg).deepCopy();
            cleaned.set("content", nextContent);
            out.add(cleaned);
        }

        return out;
    }
}
