package com.openclaw.agent.autoreply;

import java.util.*;
import java.util.regex.*;

/**
 * Message context type definitions and simple {{Placeholder}} template
 * interpolation for inbound message context.
 * Mirrors {@code auto-reply/templating.ts}.
 */
public final class Templating {

    private Templating() {
    }

    private static final Pattern TEMPLATE_PATTERN = Pattern.compile("\\{\\{\\s*(\\w+)\\s*}}");

    /**
     * Apply simple {{Placeholder}} interpolation using inbound message context.
     */
    public static String applyTemplate(String template, Map<String, Object> ctx) {
        if (template == null || template.isEmpty())
            return "";
        Matcher m = TEMPLATE_PATTERN.matcher(template);
        StringBuilder sb = new StringBuilder();
        while (m.find()) {
            String key = m.group(1);
            Object value = ctx.get(key);
            m.appendReplacement(sb, Matcher.quoteReplacement(formatTemplateValue(value)));
        }
        m.appendTail(sb);
        return sb.toString();
    }

    @SuppressWarnings("unchecked")
    static String formatTemplateValue(Object value) {
        if (value == null)
            return "";
        if (value instanceof String s)
            return s;
        if (value instanceof Number || value instanceof Boolean)
            return value.toString();
        if (value instanceof List<?> list) {
            StringJoiner joiner = new StringJoiner(",");
            for (Object entry : list) {
                if (entry == null)
                    continue;
                if (entry instanceof String || entry instanceof Number || entry instanceof Boolean) {
                    joiner.add(entry.toString());
                }
            }
            return joiner.toString();
        }
        return "";
    }
}
