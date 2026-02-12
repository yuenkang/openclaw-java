package com.openclaw.agent.skills;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses YAML-like frontmatter from SKILL.md files and extracts metadata.
 * Corresponds to TypeScript's skills/frontmatter.ts.
 */
@Slf4j
public class SkillFrontmatterParser {

    private static final ObjectMapper JSON = new ObjectMapper();

    /**
     * Regex to extract frontmatter block between --- delimiters at the start of a
     * file.
     * Matches: ---\nkey: value\nkey: value\n---
     */
    private static final Pattern FRONTMATTER_PATTERN = Pattern.compile("\\A---\\s*\\n(.*?)\\n---\\s*\\n?",
            Pattern.DOTALL);

    /**
     * Simple key: value pattern for frontmatter lines.
     */
    private static final Pattern KV_PATTERN = Pattern.compile("^([a-zA-Z][a-zA-Z0-9_-]*)\\s*:\\s*(.*)$");

    // =========================================================================
    // Frontmatter parsing
    // =========================================================================

    /**
     * Parse YAML-like frontmatter from SKILL.md content.
     *
     * @param content full SKILL.md file content
     * @return map of key→value pairs from frontmatter; empty if none found
     */
    public static Map<String, String> parseFrontmatter(String content) {
        if (content == null || content.isBlank()) {
            return Map.of();
        }

        Matcher m = FRONTMATTER_PATTERN.matcher(content);
        if (!m.find()) {
            return Map.of();
        }

        String block = m.group(1);
        Map<String, String> result = new LinkedHashMap<>();
        StringBuilder currentKey = null;
        StringBuilder currentValue = null;

        for (String line : block.split("\\n")) {
            Matcher kv = KV_PATTERN.matcher(line);
            if (kv.matches()) {
                // Flush previous key
                if (currentKey != null && currentValue != null) {
                    result.put(currentKey.toString(), currentValue.toString().trim());
                }
                currentKey = new StringBuilder(kv.group(1));
                currentValue = new StringBuilder(kv.group(2));
            } else if (currentKey != null && currentValue != null) {
                // Continuation line (for multi-line values)
                currentValue.append("\n").append(line);
            }
        }

        // Flush last key
        if (currentKey != null && currentValue != null) {
            result.put(currentKey.toString(), currentValue.toString().trim());
        }

        return result;
    }

    /**
     * Extract the body content (after frontmatter) from SKILL.md.
     */
    public static String extractBody(String content) {
        if (content == null || content.isBlank())
            return "";
        Matcher m = FRONTMATTER_PATTERN.matcher(content);
        if (m.find()) {
            return content.substring(m.end()).trim();
        }
        return content.trim();
    }

    // =========================================================================
    // Metadata resolution
    // =========================================================================

    /**
     * Resolve OpenClaw-specific metadata from parsed frontmatter.
     * Looks for a "metadata" key containing a JSON object with an "openclaw"
     * sub-key.
     *
     * @param frontmatter parsed frontmatter map
     * @return resolved metadata, or null if not present
     */
    public static SkillTypes.SkillMetadata resolveMetadata(Map<String, String> frontmatter) {
        String raw = frontmatter.get("metadata");
        if (raw == null || raw.isBlank())
            return null;

        try {
            JsonNode root = JSON.readTree(raw);
            if (root == null || !root.isObject())
                return null;

            // Look for openclaw metadata (try multiple keys for compat)
            JsonNode meta = findMetadataNode(root);
            if (meta == null || !meta.isObject())
                return null;

            return new SkillTypes.SkillMetadata(
                    meta.has("always") ? meta.get("always").asBoolean() : null,
                    textOrNull(meta, "skillKey"),
                    textOrNull(meta, "primaryEnv"),
                    textOrNull(meta, "emoji"),
                    textOrNull(meta, "homepage"),
                    stringList(meta, "os"),
                    resolveRequires(meta),
                    null);
        } catch (Exception e) {
            log.debug("Failed to parse skill metadata: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Resolve invocation policy from frontmatter.
     */
    public static SkillTypes.SkillInvocationPolicy resolveInvocationPolicy(
            Map<String, String> frontmatter) {
        boolean userInvocable = parseBool(frontmatter.get("user-invocable"), true);
        boolean disableModelInvocation = parseBool(
                frontmatter.get("disable-model-invocation"), false);
        return new SkillTypes.SkillInvocationPolicy(userInvocable, disableModelInvocation);
    }

    /**
     * Resolve the skill key (identifier) from skill name and entry.
     */
    public static String resolveSkillKey(SkillTypes.Skill skill, SkillTypes.SkillEntry entry) {
        if (entry != null && entry.metadata() != null
                && entry.metadata().skillKey() != null) {
            return entry.metadata().skillKey();
        }
        return skill.name();
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private static JsonNode findMetadataNode(JsonNode root) {
        // Try "openclaw", then legacy "pi" key
        for (String key : List.of("openclaw", "pi", "manifest")) {
            JsonNode node = root.get(key);
            if (node != null && node.isObject())
                return node;
        }
        return null;
    }

    private static String textOrNull(JsonNode node, String field) {
        JsonNode child = node.get(field);
        return (child != null && child.isTextual()) ? child.asText() : null;
    }

    private static List<String> stringList(JsonNode node, String field) {
        JsonNode child = node.get(field);
        if (child == null)
            return List.of();
        if (child.isArray()) {
            List<String> result = new ArrayList<>();
            child.forEach(el -> {
                if (el.isTextual())
                    result.add(el.asText().trim());
            });
            return result;
        }
        if (child.isTextual()) {
            return Arrays.stream(child.asText().split(","))
                    .map(String::trim)
                    .filter(s -> !s.isEmpty())
                    .toList();
        }
        return List.of();
    }

    private static SkillTypes.SkillRequires resolveRequires(JsonNode meta) {
        JsonNode req = meta.get("requires");
        if (req == null || !req.isObject())
            return null;
        return new SkillTypes.SkillRequires(
                stringList(req, "bins"),
                stringList(req, "anyBins"),
                stringList(req, "env"),
                stringList(req, "config"));
    }

    private static boolean parseBool(String value, boolean fallback) {
        if (value == null || value.isBlank())
            return fallback;
        String v = value.trim().toLowerCase();
        return switch (v) {
            case "true", "yes", "1", "on" -> true;
            case "false", "no", "0", "off" -> false;
            default -> fallback;
        };
    }
}
