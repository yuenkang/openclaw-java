package com.openclaw.agent.runtime;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Build a name-to-summary mapping from registered tools.
 * Mirrors {@code agents/tool-summaries.ts}.
 */
public final class ToolSummaryBuilder {

    private ToolSummaryBuilder() {
    }

    /**
     * Build a map of tool names (lowercased) to their summary descriptions.
     *
     * @param tools list of maps each containing at least {@code name} and
     *              optionally {@code description} / {@code label} keys.
     */
    public static Map<String, String> build(List<Map<String, Object>> tools) {
        Map<String, String> summaries = new LinkedHashMap<>();
        if (tools == null)
            return summaries;

        for (Map<String, Object> tool : tools) {
            if (tool == null)
                continue;
            Object nameObj = tool.get("name");
            if (!(nameObj instanceof String name) || name.isBlank())
                continue;

            String summary = firstNonBlankString(tool.get("description"), tool.get("label"));
            if (summary == null)
                continue;
            summaries.put(name.toLowerCase(), summary);
        }
        return summaries;
    }

    private static String firstNonBlankString(Object... values) {
        for (Object v : values) {
            if (v instanceof String s) {
                String trimmed = s.trim();
                if (!trimmed.isEmpty())
                    return trimmed;
            }
        }
        return null;
    }
}
