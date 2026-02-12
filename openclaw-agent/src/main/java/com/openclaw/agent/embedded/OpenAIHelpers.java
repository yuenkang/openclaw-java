package com.openclaw.agent.embedded;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.*;

/**
 * OpenAI reasoning block downgrade for transcript compatibility.
 * Mirrors {@code agents/pi-embedded-helpers/openai.ts}.
 */
public final class OpenAIHelpers {

    private OpenAIHelpers() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * Parsed OpenAI reasoning signature (e.g. {"id":"rs_xxx","type":"reasoning"}).
     */
    public record ReasoningSignature(String id, String type) {
    }

    /**
     * Parse an OpenAI reasoning signature from a value.
     */
    public static ReasoningSignature parseReasoningSignature(Object value) {
        if (value == null)
            return null;
        Map<String, Object> candidate = null;
        if (value instanceof String s) {
            String trimmed = s.trim();
            if (!trimmed.startsWith("{") || !trimmed.endsWith("}"))
                return null;
            try {
                JsonNode node = MAPPER.readTree(trimmed);
                Map<String, Object> parsed = new LinkedHashMap<>();
                node.fields().forEachRemaining(e -> parsed.put(e.getKey(), e.getValue().asText()));
                candidate = parsed;
            } catch (Exception e) {
                return null;
            }
        } else if (value instanceof Map<?, ?> m) {
            @SuppressWarnings("unchecked")
            Map<String, Object> typed = (Map<String, Object>) m;
            candidate = typed;
        }
        if (candidate == null)
            return null;
        Object idObj = candidate.get("id");
        Object typeObj = candidate.get("type");
        String id = idObj instanceof String si ? si : "";
        String type = typeObj instanceof String st ? st : "";
        if (!id.startsWith("rs_"))
            return null;
        if ("reasoning".equals(type) || type.startsWith("reasoning.")) {
            return new ReasoningSignature(id, type);
        }
        return null;
    }

    /**
     * Downgrade OpenAI reasoning blocks in a transcript.
     * Drops standalone reasoning blocks that lack a following non-thinking block,
     * which would cause the Responses API to reject the request.
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> downgradeReasoningBlocks(
            List<Map<String, Object>> messages) {
        if (messages == null)
            return messages;
        List<Map<String, Object>> out = new ArrayList<>();

        for (Map<String, Object> msg : messages) {
            if (msg == null) {
                out.add(null);
                continue;
            }
            String role = (String) msg.get("role");
            if (!"assistant".equals(role)) {
                out.add(msg);
                continue;
            }
            Object contentObj = msg.get("content");
            if (!(contentObj instanceof List<?> contentList)) {
                out.add(msg);
                continue;
            }
            List<Map<String, Object>> content = (List<Map<String, Object>>) contentList;

            boolean changed = false;
            List<Map<String, Object>> nextContent = new ArrayList<>();
            for (int i = 0; i < content.size(); i++) {
                Map<String, Object> block = content.get(i);
                if (block == null || !"thinking".equals(block.get("type"))) {
                    nextContent.add(block);
                    continue;
                }
                ReasoningSignature sig = parseReasoningSignature(block.get("thinkingSignature"));
                if (sig == null) {
                    nextContent.add(block);
                    continue;
                }
                if (hasFollowingNonThinkingBlock(content, i)) {
                    nextContent.add(block);
                    continue;
                }
                changed = true; // drop this block
            }

            if (!changed) {
                out.add(msg);
                continue;
            }
            if (nextContent.isEmpty())
                continue;
            Map<String, Object> merged = new LinkedHashMap<>(msg);
            merged.put("content", nextContent);
            out.add(merged);
        }
        return out;
    }

    private static boolean hasFollowingNonThinkingBlock(List<Map<String, Object>> content, int index) {
        for (int i = index + 1; i < content.size(); i++) {
            Map<String, Object> block = content.get(i);
            if (block == null)
                return true;
            if (!"thinking".equals(block.get("type")))
                return true;
        }
        return false;
    }
}
