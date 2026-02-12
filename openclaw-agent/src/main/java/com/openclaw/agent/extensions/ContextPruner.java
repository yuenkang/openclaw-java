package com.openclaw.agent.extensions;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

/**
 * Context message pruner — soft-trims and hard-clears tool results to fit
 * context window.
 * Mirrors pi-extensions/context-pruning/pruner.ts.
 */
public final class ContextPruner {

    private ContextPruner() {
    }

    private static final int CHARS_PER_TOKEN_ESTIMATE = 4;
    private static final int IMAGE_CHAR_ESTIMATE = 8_000;

    // --- Text helpers ---

    private static List<String> collectTextSegments(List<Map<String, Object>> content) {
        List<String> parts = new ArrayList<>();
        for (Map<String, Object> block : content) {
            if ("text".equals(block.get("type")) && block.get("text") instanceof String t) {
                parts.add(t);
            }
        }
        return parts;
    }

    private static int estimateJoinedTextLength(List<String> parts) {
        if (parts.isEmpty())
            return 0;
        int len = 0;
        for (String p : parts)
            len += p.length();
        len += Math.max(0, parts.size() - 1);
        return len;
    }

    private static String takeHead(List<String> parts, int maxChars) {
        if (maxChars <= 0 || parts.isEmpty())
            return "";
        int remaining = maxChars;
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < parts.size() && remaining > 0; i++) {
            if (i > 0) {
                out.append('\n');
                remaining--;
                if (remaining <= 0)
                    break;
            }
            String p = parts.get(i);
            if (p.length() <= remaining) {
                out.append(p);
                remaining -= p.length();
            } else {
                out.append(p, 0, remaining);
                remaining = 0;
            }
        }
        return out.toString();
    }

    private static String takeTail(List<String> parts, int maxChars) {
        if (maxChars <= 0 || parts.isEmpty())
            return "";
        int remaining = maxChars;
        List<String> pieces = new ArrayList<>();
        for (int i = parts.size() - 1; i >= 0 && remaining > 0; i--) {
            String p = parts.get(i);
            if (p.length() <= remaining) {
                pieces.add(p);
                remaining -= p.length();
            } else {
                pieces.add(p.substring(p.length() - remaining));
                remaining = 0;
                break;
            }
            if (remaining > 0 && i > 0) {
                pieces.add("\n");
                remaining--;
            }
        }
        StringBuilder sb = new StringBuilder();
        for (int i = pieces.size() - 1; i >= 0; i--)
            sb.append(pieces.get(i));
        return sb.toString();
    }

    @SuppressWarnings("unchecked")
    private static boolean hasImageBlocks(List<Map<String, Object>> content) {
        for (Map<String, Object> block : content) {
            if ("image".equals(block.get("type")))
                return true;
        }
        return false;
    }

    @SuppressWarnings("unchecked")
    private static int estimateMessageChars(Map<String, Object> message) {
        String role = (String) message.get("role");
        Object contentObj = message.get("content");

        if ("user".equals(role)) {
            if (contentObj instanceof String s)
                return s.length();
            if (contentObj instanceof List<?> list) {
                int chars = 0;
                for (Object b : list) {
                    if (b instanceof Map<?, ?> block) {
                        if ("text".equals(block.get("type")) && block.get("text") instanceof String t)
                            chars += t.length();
                        if ("image".equals(block.get("type")))
                            chars += IMAGE_CHAR_ESTIMATE;
                    }
                }
                return chars;
            }
        }

        if ("assistant".equals(role) && contentObj instanceof List<?> list) {
            int chars = 0;
            for (Object b : list) {
                if (b instanceof Map<?, ?> block) {
                    if ("text".equals(block.get("type")) && block.get("text") instanceof String t)
                        chars += t.length();
                    if ("thinking".equals(block.get("type")) && block.get("thinking") instanceof String t)
                        chars += t.length();
                    if ("toolCall".equals(block.get("type")))
                        chars += 128;
                }
            }
            return chars;
        }

        if ("toolResult".equals(role) && contentObj instanceof List<?> list) {
            int chars = 0;
            for (Object b : list) {
                if (b instanceof Map<?, ?> block) {
                    if ("text".equals(block.get("type")) && block.get("text") instanceof String t)
                        chars += t.length();
                    if ("image".equals(block.get("type")))
                        chars += IMAGE_CHAR_ESTIMATE;
                }
            }
            return chars;
        }

        return 256;
    }

    private static int estimateContextChars(List<Map<String, Object>> messages) {
        int sum = 0;
        for (Map<String, Object> m : messages)
            sum += estimateMessageChars(m);
        return sum;
    }

    // --- Index resolution ---

    private static Integer findAssistantCutoff(List<Map<String, Object>> messages, int keepLast) {
        if (keepLast <= 0)
            return messages.size();
        int remaining = keepLast;
        for (int i = messages.size() - 1; i >= 0; i--) {
            if ("assistant".equals(messages.get(i).get("role"))) {
                remaining--;
                if (remaining == 0)
                    return i;
            }
        }
        return null;
    }

    private static Integer findFirstUserIndex(List<Map<String, Object>> messages) {
        for (int i = 0; i < messages.size(); i++) {
            if ("user".equals(messages.get(i).get("role")))
                return i;
        }
        return null;
    }

    // --- Soft trim ---

    @SuppressWarnings("unchecked")
    private static Map<String, Object> softTrimToolResult(
            Map<String, Object> msg,
            ContextPruningSettings.Effective settings) {
        List<Map<String, Object>> content = (List<Map<String, Object>>) msg.get("content");
        if (hasImageBlocks(content))
            return null;

        List<String> parts = collectTextSegments(content);
        int rawLen = estimateJoinedTextLength(parts);
        if (rawLen <= settings.getSoftTrim().getMaxChars())
            return null;

        int headChars = Math.max(0, settings.getSoftTrim().getHeadChars());
        int tailChars = Math.max(0, settings.getSoftTrim().getTailChars());
        if (headChars + tailChars >= rawLen)
            return null;

        String head = takeHead(parts, headChars);
        String tail = takeTail(parts, tailChars);
        String trimmed = head + "\n...\n" + tail;
        String note = "\n\n[Tool result trimmed: kept first " + headChars
                + " chars and last " + tailChars + " chars of " + rawLen + " chars.]";

        Map<String, Object> result = new java.util.HashMap<>(msg);
        result.put("content", List.of(Map.of("type", "text", "text", trimmed + note)));
        return result;
    }

    // --- Main pruning entry point ---

    /**
     * Prune context messages to fit within the context window.
     * Returns the original list if no pruning is needed.
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> pruneContextMessages(
            List<Map<String, Object>> messages,
            ContextPruningSettings.Effective settings,
            Integer contextWindowTokensOverride,
            Integer modelContextWindow,
            Predicate<String> isToolPrunable) {

        int contextWindowTokens;
        if (contextWindowTokensOverride != null && contextWindowTokensOverride > 0) {
            contextWindowTokens = contextWindowTokensOverride;
        } else if (modelContextWindow != null && modelContextWindow > 0) {
            contextWindowTokens = modelContextWindow;
        } else {
            return messages;
        }

        int charWindow = contextWindowTokens * CHARS_PER_TOKEN_ESTIMATE;
        if (charWindow <= 0)
            return messages;

        Integer cutoffIndex = findAssistantCutoff(messages, settings.getKeepLastAssistants());
        if (cutoffIndex == null)
            return messages;

        Integer firstUserIndex = findFirstUserIndex(messages);
        int pruneStart = firstUserIndex == null ? messages.size() : firstUserIndex;

        if (isToolPrunable == null) {
            isToolPrunable = ContextPruningTools.makeToolPrunablePredicate(settings.getTools());
        }

        int totalChars = estimateContextChars(messages);
        double ratio = (double) totalChars / charWindow;
        if (ratio < settings.getSoftTrimRatio())
            return messages;

        List<Integer> prunableIndexes = new ArrayList<>();
        List<Map<String, Object>> next = null;

        for (int i = pruneStart; i < cutoffIndex; i++) {
            Map<String, Object> msg = messages.get(i);
            if (!"toolResult".equals(msg.get("role")))
                continue;
            String toolName = (String) msg.getOrDefault("toolName", "");
            if (!isToolPrunable.test(toolName))
                continue;
            List<Map<String, Object>> content = (List<Map<String, Object>>) msg.get("content");
            if (content != null && hasImageBlocks(content))
                continue;
            prunableIndexes.add(i);

            Map<String, Object> updated = softTrimToolResult(msg, settings);
            if (updated == null)
                continue;

            int beforeChars = estimateMessageChars(msg);
            int afterChars = estimateMessageChars(updated);
            totalChars += afterChars - beforeChars;
            if (next == null)
                next = new ArrayList<>(messages);
            next.set(i, updated);
        }

        List<Map<String, Object>> afterSoftTrim = next != null ? next : messages;
        ratio = (double) totalChars / charWindow;
        if (ratio < settings.getHardClearRatio())
            return afterSoftTrim;
        if (!settings.getHardClear().isEnabled())
            return afterSoftTrim;

        // Hard clear prunable tool results
        int prunableToolChars = 0;
        for (int idx : prunableIndexes) {
            Map<String, Object> msg = afterSoftTrim.get(idx);
            if ("toolResult".equals(msg.get("role"))) {
                prunableToolChars += estimateMessageChars(msg);
            }
        }
        if (prunableToolChars < settings.getMinPrunableToolChars())
            return afterSoftTrim;

        for (int idx : prunableIndexes) {
            if (ratio < settings.getHardClearRatio())
                break;
            Map<String, Object> msg = (next != null ? next : messages).get(idx);
            if (!"toolResult".equals(msg.get("role")))
                continue;

            int beforeChars = estimateMessageChars(msg);
            Map<String, Object> cleared = new java.util.HashMap<>(msg);
            cleared.put("content", List.of(Map.of("type", "text", "text",
                    settings.getHardClear().getPlaceholder())));
            if (next == null)
                next = new ArrayList<>(messages);
            next.set(idx, cleared);
            totalChars += estimateMessageChars(cleared) - beforeChars;
            ratio = (double) totalChars / charWindow;
        }

        return next != null ? next : messages;
    }
}
