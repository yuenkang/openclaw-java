package com.openclaw.agent.autoreply.reply;

import java.util.*;
import java.util.function.Function;

/**
 * Chat history context — build context strings from history entries,
 * manage LRU-evicted history maps, and append / clear entries.
 * Mirrors {@code auto-reply/reply/history.ts}.
 */
public final class History {

    private History() {
    }

    public static final String HISTORY_CONTEXT_MARKER = "[Chat messages since your last reply - for context]";
    public static final int DEFAULT_GROUP_HISTORY_LIMIT = 50;
    public static final int MAX_HISTORY_KEYS = 1000;

    /** A single history entry. */
    public record HistoryEntry(
            String sender,
            String body,
            Long timestamp,
            String messageId) {
    }

    /**
     * Evict oldest keys from a history map when it exceeds MAX_HISTORY_KEYS.
     * Uses LinkedHashMap insertion order for LRU-like behavior.
     */
    public static <T> void evictOldHistoryKeys(LinkedHashMap<String, List<T>> historyMap, int maxKeys) {
        if (historyMap.size() <= maxKeys)
            return;
        int toDelete = historyMap.size() - maxKeys;
        Iterator<String> it = historyMap.keySet().iterator();
        for (int i = 0; i < toDelete && it.hasNext(); i++) {
            it.next();
            it.remove();
        }
    }

    public static <T> void evictOldHistoryKeys(LinkedHashMap<String, List<T>> historyMap) {
        evictOldHistoryKeys(historyMap, MAX_HISTORY_KEYS);
    }

    /**
     * Build final context with history + current message.
     */
    public static String buildHistoryContext(String historyText, String currentMessage, String lineBreak) {
        if (lineBreak == null)
            lineBreak = "\n";
        if (historyText == null || historyText.trim().isEmpty())
            return currentMessage;
        return String.join(lineBreak,
                HISTORY_CONTEXT_MARKER,
                historyText,
                "",
                Mentions.CURRENT_MESSAGE_MARKER,
                currentMessage);
    }

    /**
     * Append an entry to the keyed history list, with limit enforcement.
     */
    public static <T extends HistoryEntry> List<T> appendHistoryEntry(
            LinkedHashMap<String, List<T>> historyMap,
            String historyKey,
            T entry,
            int limit) {
        if (limit <= 0)
            return List.of();
        List<T> history = historyMap.getOrDefault(historyKey, new ArrayList<>());
        history.add(entry);
        while (history.size() > limit)
            history.remove(0);
        // Refresh insertion order
        historyMap.remove(historyKey);
        historyMap.put(historyKey, history);
        evictOldHistoryKeys(historyMap);
        return history;
    }

    /** Alias for appendHistoryEntry. */
    public static <T extends HistoryEntry> List<T> recordPendingHistoryEntry(
            LinkedHashMap<String, List<T>> historyMap,
            String historyKey, T entry, int limit) {
        return appendHistoryEntry(historyMap, historyKey, entry, limit);
    }

    /** Conditional append. */
    public static <T extends HistoryEntry> List<T> recordPendingHistoryEntryIfEnabled(
            LinkedHashMap<String, List<T>> historyMap,
            String historyKey, T entry, int limit) {
        if (entry == null || limit <= 0)
            return List.of();
        return recordPendingHistoryEntry(historyMap, historyKey, entry, limit);
    }

    /** Build context from pending history map. */
    public static String buildPendingHistoryContextFromMap(
            LinkedHashMap<String, List<HistoryEntry>> historyMap,
            String historyKey, int limit,
            String currentMessage,
            Function<HistoryEntry, String> formatEntry,
            String lineBreak) {
        if (limit <= 0)
            return currentMessage;
        List<HistoryEntry> entries = historyMap.getOrDefault(historyKey, List.of());
        return buildHistoryContextFromEntries(entries, currentMessage, formatEntry, lineBreak, false);
    }

    /** Build context from map, optionally appending a new entry first. */
    public static String buildHistoryContextFromMap(
            LinkedHashMap<String, List<HistoryEntry>> historyMap,
            String historyKey, int limit,
            HistoryEntry entry,
            String currentMessage,
            Function<HistoryEntry, String> formatEntry,
            String lineBreak,
            boolean excludeLast) {
        if (limit <= 0)
            return currentMessage;
        List<HistoryEntry> entries;
        if (entry != null) {
            entries = appendHistoryEntry(historyMap, historyKey, entry, limit);
        } else {
            entries = historyMap.getOrDefault(historyKey, List.of());
        }
        return buildHistoryContextFromEntries(entries, currentMessage, formatEntry, lineBreak, excludeLast);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static void clearHistoryEntries(
            LinkedHashMap<String, ? extends List<?>> historyMap,
            String historyKey) {
        ((LinkedHashMap) historyMap).put(historyKey, new ArrayList<>());
    }

    /** Conditional clear. */
    public static void clearHistoryEntriesIfEnabled(
            LinkedHashMap<String, ? extends List<?>> historyMap,
            String historyKey, int limit) {
        if (limit <= 0)
            return;
        clearHistoryEntries(historyMap, historyKey);
    }

    /** Build context from a list of entries. */
    public static String buildHistoryContextFromEntries(
            List<HistoryEntry> entries,
            String currentMessage,
            Function<HistoryEntry, String> formatEntry,
            String lineBreak,
            boolean excludeLast) {
        if (lineBreak == null)
            lineBreak = "\n";
        List<HistoryEntry> used = excludeLast
                ? entries.subList(0, Math.max(0, entries.size() - 1))
                : entries;
        if (used.isEmpty())
            return currentMessage;
        String lb = lineBreak;
        String historyText = String.join(lb, used.stream().map(formatEntry).toList());
        return buildHistoryContext(historyText, currentMessage, lb);
    }
}
