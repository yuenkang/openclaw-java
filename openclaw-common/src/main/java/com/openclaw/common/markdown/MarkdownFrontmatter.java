package com.openclaw.common.markdown;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * YAML frontmatter extraction from Markdown documents.
 * Translates TS markdown/frontmatter.ts.
 *
 */
public final class MarkdownFrontmatter {

    private MarkdownFrontmatter() {
    }

    private static final Pattern FRONTMATTER_PATTERN = Pattern.compile(
            "\\A---\\s*\\n(.*?)\\n---\\s*\\n?", Pattern.DOTALL);

    /**
     * Result of frontmatter extraction.
     */
    public record FrontmatterResult(
            Map<String, String> data,
            String content) {
        /**
         * Returns true if frontmatter was found.
         */
        public boolean hasFrontmatter() {
            return !data.isEmpty();
        }
    }

    /**
     * Extract YAML frontmatter from a Markdown document.
     *
     * @param markdown raw markdown text
     * @return result with frontmatter data and remaining content
     */
    public static FrontmatterResult extract(String markdown) {
        if (markdown == null || markdown.isEmpty()) {
            return new FrontmatterResult(Map.of(), markdown != null ? markdown : "");
        }

        Matcher m = FRONTMATTER_PATTERN.matcher(markdown);
        if (!m.find()) {
            return new FrontmatterResult(Map.of(), markdown);
        }

        String yamlBlock = m.group(1);
        String remainingContent = markdown.substring(m.end());
        Map<String, String> data = parseSimpleYaml(yamlBlock);

        return new FrontmatterResult(data, remainingContent);
    }

    /**
     * Check if a markdown document has frontmatter.
     */
    public static boolean hasFrontmatter(String markdown) {
        if (markdown == null)
            return false;
        return FRONTMATTER_PATTERN.matcher(markdown).find();
    }

    /**
     * Strip frontmatter and return only the body content.
     */
    public static String stripFrontmatter(String markdown) {
        return extract(markdown).content();
    }

    // -----------------------------------------------------------------------
    // Simple YAML parser (key: value per line)
    // -----------------------------------------------------------------------

    private static final Pattern YAML_LINE = Pattern.compile(
            "^\\s*([a-zA-Z0-9_-]+)\\s*:\\s*(.+?)\\s*$", Pattern.MULTILINE);

    private static Map<String, String> parseSimpleYaml(String yaml) {
        Map<String, String> result = new LinkedHashMap<>();
        Matcher m = YAML_LINE.matcher(yaml);
        while (m.find()) {
            String key = m.group(1);
            String value = m.group(2);
            // Strip surrounding quotes
            if ((value.startsWith("\"") && value.endsWith("\"")) ||
                    (value.startsWith("'") && value.endsWith("'"))) {
                value = value.substring(1, value.length() - 1);
            }
            result.put(key, value);
        }
        return result;
    }
}
