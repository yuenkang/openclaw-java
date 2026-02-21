package com.openclaw.autoreply;

import java.util.*;
import java.util.regex.Pattern;

/**
 * Tool metadata formatting for display (aggregate tool calls, path shortening,
 * etc.).
 * Mirrors {@code auto-reply/tool-meta.ts}.
 */
public final class ToolMeta {

    private ToolMeta() {
    }

    private static final Pattern PATH_RE = Pattern.compile("^~?(/[^\\s]+)+$");

    /** Shorten a home directory path (replace /Users/x with ~). */
    public static String shortenPath(String p) {
        if (p == null)
            return p;
        String home = System.getProperty("user.home");
        if (home != null && p.startsWith(home)) {
            return "~" + p.substring(home.length());
        }
        return p;
    }

    /** Shorten home paths embedded in a meta string. */
    public static String shortenMeta(String meta) {
        if (meta == null || meta.isEmpty())
            return meta;
        int colonIdx = meta.indexOf(':');
        if (colonIdx == -1)
            return shortenHomePaths(meta);
        String base = meta.substring(0, colonIdx);
        String rest = meta.substring(colonIdx);
        return shortenHomePaths(base) + rest;
    }

    private static String shortenHomePaths(String text) {
        String home = System.getProperty("user.home");
        if (home == null || text == null)
            return text;
        return text.replace(home, "~");
    }

    /** Check if a value looks like a file path. */
    public static boolean isPathLike(String value) {
        if (value == null || value.isEmpty())
            return false;
        if (value.contains(" "))
            return false;
        if (value.contains("://"))
            return false;
        if (value.contains("·"))
            return false;
        if (value.contains("&&") || value.contains("||"))
            return false;
        return PATH_RE.matcher(value).matches();
    }

    /** Wrap a value in markdown backticks if requested and not already quoted. */
    public static String maybeWrapMarkdown(String value, boolean markdown) {
        if (!markdown)
            return value;
        if (value.contains("`"))
            return value;
        return "`" + value + "`";
    }

    /** Split exec-style flags (elevated, pty) from the command body. */
    public record ExecFlags(List<String> flags, String body) {
    }

    public static ExecFlags splitExecFlags(String meta) {
        String[] parts = meta.split(" · ");
        List<String> flags = new ArrayList<>();
        List<String> bodyParts = new ArrayList<>();
        for (String part : parts) {
            String trimmed = part.trim();
            if (trimmed.isEmpty())
                continue;
            if ("elevated".equals(trimmed) || "pty".equals(trimmed)) {
                flags.add(trimmed);
            } else {
                bodyParts.add(trimmed);
            }
        }
        return new ExecFlags(flags, String.join(" · ", bodyParts));
    }

    /** Format meta for display, handling exec special-case (flag extraction). */
    public static String formatMetaForDisplay(String toolName, String meta, boolean markdown) {
        String normalized = (toolName != null ? toolName : "").trim().toLowerCase();
        if ("exec".equals(normalized) || "bash".equals(normalized)) {
            ExecFlags ef = splitExecFlags(meta);
            if (!ef.flags.isEmpty()) {
                if (ef.body.isEmpty())
                    return String.join(" · ", ef.flags);
                return String.join(" · ", ef.flags) + " · " + maybeWrapMarkdown(ef.body, markdown);
            }
        }
        return maybeWrapMarkdown(meta, markdown);
    }

    /**
     * Format an aggregate tool call display line.
     *
     * @param toolEmoji emoji for the tool
     * @param toolLabel label for the tool
     * @param metas     individual tool call meta strings
     * @param markdown  whether to use markdown formatting
     */
    public static String formatToolAggregate(String toolEmoji, String toolLabel,
            List<String> metas, boolean markdown) {
        List<String> filtered = new ArrayList<>();
        if (metas != null) {
            for (String m : metas) {
                if (m != null && !m.isBlank())
                    filtered.add(shortenMeta(m));
            }
        }
        String prefix = (toolEmoji != null ? toolEmoji + " " : "") + (toolLabel != null ? toolLabel : "tool");
        if (filtered.isEmpty())
            return prefix;

        // Separate non-path (raw) segments from path segments
        List<String> rawSegments = new ArrayList<>();
        Map<String, List<String>> grouped = new LinkedHashMap<>();
        for (String m : filtered) {
            if (!isPathLike(m) || m.contains("→")) {
                rawSegments.add(m);
                continue;
            }
            int lastSlash = m.lastIndexOf('/');
            if (lastSlash > 0) {
                String dir = m.substring(0, lastSlash);
                String base = m.substring(lastSlash + 1);
                grouped.computeIfAbsent(dir, k -> new ArrayList<>()).add(base);
            } else {
                grouped.computeIfAbsent(".", k -> new ArrayList<>()).add(m);
            }
        }

        List<String> segments = new ArrayList<>(rawSegments);
        for (Map.Entry<String, List<String>> entry : grouped.entrySet()) {
            String dir = entry.getKey();
            List<String> files = entry.getValue();
            String brace = files.size() > 1 ? "{" + String.join(", ", files) + "}" : files.get(0);
            segments.add(".".equals(dir) ? brace : dir + "/" + brace);
        }

        String meta = String.join("; ", segments);
        return prefix + ": " + formatMetaForDisplay(null, meta, markdown);
    }
}
