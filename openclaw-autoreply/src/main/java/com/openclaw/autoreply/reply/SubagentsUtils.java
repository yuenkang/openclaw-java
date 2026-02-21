package com.openclaw.autoreply.reply;

import java.util.Comparator;
import java.util.List;
import java.util.Map;

/**
 * Subagent run record formatting utilities — duration, age, label, status.
 * Mirrors {@code auto-reply/reply/subagents-utils.ts}.
 */
public final class SubagentsUtils {

    private SubagentsUtils() {
    }

    /** Format a duration in ms to a short human string (e.g. "3m12s"). */
    public static String formatDurationShort(Long valueMs) {
        if (valueMs == null || !Double.isFinite(valueMs) || valueMs <= 0)
            return "n/a";
        long totalSeconds = Math.round(valueMs / 1000.0);
        long hours = totalSeconds / 3600;
        long minutes = (totalSeconds % 3600) / 60;
        long seconds = totalSeconds % 60;
        if (hours > 0)
            return hours + "h" + minutes + "m";
        if (minutes > 0)
            return minutes + "m" + seconds + "s";
        return seconds + "s";
    }

    /** Format an age in ms to a short human string (e.g. "5m ago"). */
    public static String formatAgeShort(Long valueMs) {
        if (valueMs == null || !Double.isFinite(valueMs) || valueMs <= 0)
            return "n/a";
        long minutes = Math.round(valueMs / 60_000.0);
        if (minutes < 1)
            return "just now";
        if (minutes < 60)
            return minutes + "m ago";
        long hours = Math.round(minutes / 60.0);
        if (hours < 48)
            return hours + "h ago";
        long days = Math.round(hours / 24.0);
        return days + "d ago";
    }

    /**
     * Resolve a display label for a subagent run record.
     * 
     * @param entry    a map with possible keys: label, task
     * @param fallback default label if none found
     */
    public static String resolveSubagentLabel(Map<String, Object> entry, String fallback) {
        if (fallback == null)
            fallback = "subagent";
        String raw = getStr(entry, "label");
        if (raw == null || raw.isEmpty())
            raw = getStr(entry, "task");
        return (raw != null && !raw.isEmpty()) ? raw : fallback;
    }

    /** Format a run label, truncating to maxLength. */
    public static String formatRunLabel(Map<String, Object> entry, int maxLength) {
        if (maxLength <= 0)
            maxLength = 72;
        String raw = resolveSubagentLabel(entry, "subagent");
        if (raw.length() > maxLength) {
            return raw.substring(0, maxLength).stripTrailing() + "…";
        }
        return raw;
    }

    /** Format a run status (running, done, or specific status). */
    public static String formatRunStatus(Map<String, Object> entry) {
        if (entry.get("endedAt") == null)
            return "running";
        @SuppressWarnings("unchecked")
        Map<String, Object> outcome = (Map<String, Object>) entry.get("outcome");
        String status = outcome != null ? getStr(outcome, "status") : null;
        if (status == null || status.isEmpty())
            return "done";
        return "ok".equals(status) ? "done" : status;
    }

    /** Sort subagent runs by start time descending. */
    public static List<Map<String, Object>> sortSubagentRuns(List<Map<String, Object>> runs) {
        return runs.stream()
                .sorted(Comparator.comparingLong((Map<String, Object> r) -> {
                    Long s = getLong(r, "startedAt");
                    if (s == null)
                        s = getLong(r, "createdAt");
                    return s != null ? s : 0L;
                }).reversed())
                .toList();
    }

    private static String getStr(Map<String, Object> map, String key) {
        if (map == null)
            return null;
        Object v = map.get(key);
        if (v instanceof String s)
            return s.trim();
        return null;
    }

    private static Long getLong(Map<String, Object> map, String key) {
        if (map == null)
            return null;
        Object v = map.get(key);
        if (v instanceof Number n)
            return n.longValue();
        return null;
    }
}
