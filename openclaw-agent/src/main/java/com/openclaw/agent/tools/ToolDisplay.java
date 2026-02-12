package com.openclaw.agent.tools;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;

/**
 * Tool display resolution — emoji, title, verb, detail for UI rendering.
 * Corresponds to TypeScript's tool-display.ts.
 *
 * <p>
 * Uses the same tool-display.json configuration file as the TS version,
 * loaded from classpath resources.
 * </p>
 */
@Slf4j
public final class ToolDisplay {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final int MAX_DETAIL_ENTRIES = 8;
    private static final String DEFAULT_EMOJI = "🧩";

    // Loaded from tool-display.json
    private static JsonNode CONFIG;
    private static JsonNode FALLBACK;
    private static JsonNode TOOL_MAP;

    private static final Map<String, String> DETAIL_LABEL_OVERRIDES = Map.ofEntries(
            Map.entry("agentId", "agent"),
            Map.entry("sessionKey", "session"),
            Map.entry("targetId", "target"),
            Map.entry("targetUrl", "url"),
            Map.entry("nodeId", "node"),
            Map.entry("requestId", "request"),
            Map.entry("messageId", "message"),
            Map.entry("threadId", "thread"),
            Map.entry("channelId", "channel"),
            Map.entry("guildId", "guild"),
            Map.entry("userId", "user"),
            Map.entry("runTimeoutSeconds", "timeout"),
            Map.entry("timeoutSeconds", "timeout"),
            Map.entry("includeTools", "tools"),
            Map.entry("pollQuestion", "poll"),
            Map.entry("maxChars", "max chars"));

    static {
        try (InputStream is = ToolDisplay.class.getClassLoader()
                .getResourceAsStream("tool-display.json")) {
            if (is != null) {
                CONFIG = MAPPER.readTree(is);
                FALLBACK = CONFIG.has("fallback") ? CONFIG.get("fallback") : MAPPER.createObjectNode();
                TOOL_MAP = CONFIG.has("tools") ? CONFIG.get("tools") : MAPPER.createObjectNode();
            } else {
                CONFIG = MAPPER.createObjectNode();
                FALLBACK = MAPPER.createObjectNode();
                TOOL_MAP = MAPPER.createObjectNode();
                log.warn("tool-display.json not found on classpath; using defaults");
            }
        } catch (IOException e) {
            CONFIG = MAPPER.createObjectNode();
            FALLBACK = MAPPER.createObjectNode();
            TOOL_MAP = MAPPER.createObjectNode();
            log.warn("Failed to load tool-display.json: {}", e.getMessage());
        }
    }

    private ToolDisplay() {
    }

    // --- Result type ---

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ToolDisplayInfo {
        private String name;
        private String emoji;
        private String title;
        private String label;
        private String verb;
        private String detail;
    }

    // --- Main resolution ---

    /**
     * Resolve display information for a tool call.
     *
     * @param name tool name
     * @param args tool arguments (may be null)
     * @param meta optional meta string fallback
     * @return display info
     */
    public static ToolDisplayInfo resolveToolDisplay(String name, JsonNode args, String meta) {
        String normalizedName = normalizeName(name);
        String key = normalizedName.toLowerCase();
        JsonNode spec = TOOL_MAP.has(key) ? TOOL_MAP.get(key) : null;

        String emoji = getStr(spec, "emoji", getStr(FALLBACK, "emoji", DEFAULT_EMOJI));
        String title = getStr(spec, "title", defaultTitle(normalizedName));
        String label = getStr(spec, "label", title);

        // Resolve action
        String action = null;
        if (args != null && args.isObject() && args.has("action")) {
            JsonNode actionNode = args.get("action");
            if (actionNode.isTextual()) {
                action = actionNode.asText().trim();
            }
        }
        JsonNode actionSpec = resolveActionSpec(spec, action);
        String verb = normalizeVerb(
                actionSpec != null && actionSpec.has("label")
                        ? actionSpec.get("label").asText()
                        : action);

        // Resolve detail
        String detail = null;
        if ("read".equals(key)) {
            detail = resolveReadDetail(args);
        }
        if (detail == null && ("write".equals(key) || "edit".equals(key) || "attach".equals(key))) {
            detail = resolveWriteDetail(args);
        }

        // Try detail keys
        List<String> detailKeys = getDetailKeys(actionSpec, spec);
        if (detail == null && !detailKeys.isEmpty()) {
            detail = resolveDetailFromKeys(args, detailKeys);
        }

        if (detail == null && meta != null && !meta.isBlank()) {
            detail = meta;
        }

        // Shorten home dir
        if (detail != null) {
            String home = System.getProperty("user.home");
            if (home != null && !home.isBlank()) {
                detail = detail.replace(home, "~");
            }
        }

        return ToolDisplayInfo.builder()
                .name(normalizedName)
                .emoji(emoji)
                .title(title)
                .label(label)
                .verb(verb)
                .detail(detail)
                .build();
    }

    public static ToolDisplayInfo resolveToolDisplay(String name, JsonNode args) {
        return resolveToolDisplay(name, args, null);
    }

    /**
     * Format a single-line tool summary.
     */
    public static String formatToolSummary(ToolDisplayInfo display) {
        String detail = formatToolDetail(display);
        return detail != null
                ? display.emoji + " " + display.label + ": " + detail
                : display.emoji + " " + display.label;
    }

    /**
     * Format verb + detail portion.
     */
    public static String formatToolDetail(ToolDisplayInfo display) {
        List<String> parts = new ArrayList<>();
        if (display.verb != null && !display.verb.isBlank()) {
            parts.add(display.verb);
        }
        if (display.detail != null && !display.detail.isBlank()) {
            parts.add(display.detail);
        }
        return parts.isEmpty() ? null : String.join(" · ", parts);
    }

    // --- Internals ---

    private static String normalizeName(String name) {
        return (name == null || name.isBlank()) ? "tool" : name.trim();
    }

    private static String defaultTitle(String name) {
        String cleaned = name.replace('_', ' ').trim();
        if (cleaned.isEmpty())
            return "Tool";
        String[] words = cleaned.split("\\s+");
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < words.length; i++) {
            if (i > 0)
                sb.append(' ');
            String w = words[i];
            if (w.length() <= 2 && w.toUpperCase().equals(w)) {
                sb.append(w);
            } else {
                sb.append(Character.toUpperCase(w.charAt(0)));
                if (w.length() > 1)
                    sb.append(w.substring(1));
            }
        }
        return sb.toString();
    }

    private static String normalizeVerb(String value) {
        if (value == null || value.isBlank())
            return null;
        return value.trim().replace('_', ' ');
    }

    private static String getStr(JsonNode node, String key, String defaultValue) {
        if (node != null && node.has(key) && node.get(key).isTextual()) {
            return node.get(key).asText();
        }
        return defaultValue;
    }

    private static JsonNode resolveActionSpec(JsonNode spec, String action) {
        if (spec == null || action == null || action.isBlank())
            return null;
        if (spec.has("actions") && spec.get("actions").isObject()) {
            JsonNode actions = spec.get("actions");
            if (actions.has(action))
                return actions.get(action);
        }
        return null;
    }

    private static List<String> getDetailKeys(JsonNode actionSpec, JsonNode spec) {
        // Priority: actionSpec.detailKeys > spec.detailKeys > fallback.detailKeys
        for (JsonNode source : new JsonNode[] { actionSpec, spec, FALLBACK }) {
            if (source != null && source.has("detailKeys") && source.get("detailKeys").isArray()) {
                List<String> keys = new ArrayList<>();
                for (JsonNode k : source.get("detailKeys")) {
                    if (k.isTextual())
                        keys.add(k.asText());
                }
                return keys;
            }
        }
        return Collections.emptyList();
    }

    private static String resolveReadDetail(JsonNode args) {
        if (args == null || !args.isObject())
            return null;
        String path = args.has("path") && args.get("path").isTextual()
                ? args.get("path").asText()
                : null;
        if (path == null)
            return null;
        Integer offset = args.has("offset") && args.get("offset").isNumber()
                ? args.get("offset").asInt()
                : null;
        Integer limit = args.has("limit") && args.get("limit").isNumber()
                ? args.get("limit").asInt()
                : null;
        if (offset != null && limit != null) {
            return path + ":" + offset + "-" + (offset + limit);
        }
        return path;
    }

    private static String resolveWriteDetail(JsonNode args) {
        if (args == null || !args.isObject())
            return null;
        return args.has("path") && args.get("path").isTextual()
                ? args.get("path").asText()
                : null;
    }

    private static String resolveDetailFromKeys(JsonNode args, List<String> keys) {
        if (args == null || !args.isObject())
            return null;
        List<String[]> entries = new ArrayList<>(); // [label, value]
        for (String key : keys) {
            JsonNode value = lookupValueByPath(args, key);
            String display = coerceDisplayValue(value);
            if (display != null) {
                entries.add(new String[] { formatDetailKey(key), display });
            }
        }
        if (entries.isEmpty())
            return null;
        if (entries.size() == 1)
            return entries.get(0)[1];

        // Deduplicate
        Set<String> seen = new LinkedHashSet<>();
        List<String[]> unique = new ArrayList<>();
        for (String[] e : entries) {
            String token = e[0] + ":" + e[1];
            if (seen.add(token)) {
                unique.add(e);
            }
        }
        if (unique.isEmpty())
            return null;
        StringBuilder sb = new StringBuilder();
        int count = Math.min(unique.size(), MAX_DETAIL_ENTRIES);
        for (int i = 0; i < count; i++) {
            if (i > 0)
                sb.append(" · ");
            sb.append(unique.get(i)[0]).append(' ').append(unique.get(i)[1]);
        }
        return sb.toString();
    }

    private static JsonNode lookupValueByPath(JsonNode args, String path) {
        if (args == null || !args.isObject())
            return null;
        JsonNode current = args;
        for (String segment : path.split("\\.")) {
            if (segment.isEmpty())
                return null;
            if (current == null || !current.isObject())
                return null;
            current = current.get(segment);
        }
        return current;
    }

    private static String coerceDisplayValue(JsonNode value) {
        if (value == null || value.isNull())
            return null;
        if (value.isTextual()) {
            String trimmed = value.asText().trim();
            if (trimmed.isEmpty())
                return null;
            String firstLine = trimmed.split("\\r?\\n")[0].trim();
            if (firstLine.isEmpty())
                return null;
            return firstLine.length() > 160 ? firstLine.substring(0, 157) + "…" : firstLine;
        }
        if (value.isBoolean()) {
            return value.asBoolean() ? "true" : null;
        }
        if (value.isNumber()) {
            double d = value.asDouble();
            if (!Double.isFinite(d) || d == 0)
                return null;
            return value.isInt() || value.isLong() ? String.valueOf(value.asLong()) : String.valueOf(d);
        }
        if (value.isArray()) {
            List<String> items = new ArrayList<>();
            for (JsonNode item : value) {
                String display = coerceDisplayValue(item);
                if (display != null)
                    items.add(display);
            }
            if (items.isEmpty())
                return null;
            if (items.size() <= 3) {
                return String.join(", ", items);
            }
            return items.get(0) + ", " + items.get(1) + ", " + items.get(2) + "…";
        }
        return null;
    }

    private static String formatDetailKey(String raw) {
        String[] segments = raw.split("\\.");
        String last = segments[segments.length - 1];
        String override = DETAIL_LABEL_OVERRIDES.get(last);
        if (override != null)
            return override;
        String cleaned = last.replace('_', ' ').replace('-', ' ');
        // camelCase → spaced
        String spaced = cleaned.replaceAll("([a-z0-9])([A-Z])", "$1 $2");
        return spaced.trim().toLowerCase();
    }
}
