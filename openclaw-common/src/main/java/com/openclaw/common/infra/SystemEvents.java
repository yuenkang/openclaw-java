package com.openclaw.common.infra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Lightweight in-memory queue for human-readable system events that should be
 * prefixed to the next prompt. Events are session-scoped and ephemeral (not
 * persisted).
 * Corresponds to TypeScript's infra/system-events.ts.
 */
public final class SystemEvents {

    private SystemEvents() {
    }

    private static final int MAX_EVENTS = 20;

    /**
     * A system event with text and timestamp.
     */
    public record Event(String text, long ts) {
    }

    private static class SessionQueue {
        final List<Event> queue = new ArrayList<>();
        String lastText;
        String lastContextKey;
    }

    private static final Map<String, SessionQueue> queues = new ConcurrentHashMap<>();

    // ── Helpers ─────────────────────────────────────────────────────────

    private static String requireSessionKey(String key) {
        String trimmed = (key != null) ? key.trim() : "";
        if (trimmed.isEmpty()) {
            throw new IllegalArgumentException("system events require a sessionKey");
        }
        return trimmed;
    }

    private static String normalizeContextKey(String key) {
        if (key == null)
            return null;
        String trimmed = key.trim();
        return trimmed.isEmpty() ? null : trimmed.toLowerCase(Locale.ROOT);
    }

    // ── Public API ──────────────────────────────────────────────────────

    /**
     * Check if the context key has changed for a session.
     */
    public static boolean isContextChanged(String sessionKey, String contextKey) {
        String key = requireSessionKey(sessionKey);
        SessionQueue existing = queues.get(key);
        String normalized = normalizeContextKey(contextKey);
        String current = existing != null ? existing.lastContextKey : null;
        if (normalized == null && current == null)
            return false;
        if (normalized == null || current == null)
            return true;
        return !normalized.equals(current);
    }

    /**
     * Enqueue a system event for a session. Skips consecutive duplicates.
     */
    public static void enqueue(String text, String sessionKey, String contextKey) {
        String key = requireSessionKey(sessionKey);
        SessionQueue entry = queues.computeIfAbsent(key, k -> new SessionQueue());

        synchronized (entry) {
            String cleaned = text != null ? text.trim() : "";
            if (cleaned.isEmpty()) {
                return;
            }
            entry.lastContextKey = normalizeContextKey(contextKey);
            if (cleaned.equals(entry.lastText)) {
                return; // skip consecutive duplicates
            }
            entry.lastText = cleaned;
            entry.queue.add(new Event(cleaned, System.currentTimeMillis()));
            if (entry.queue.size() > MAX_EVENTS) {
                entry.queue.remove(0);
            }
        }
    }

    /**
     * Drain all event entries for a session, removing them from the queue.
     */
    public static List<Event> drainEntries(String sessionKey) {
        String key = requireSessionKey(sessionKey);
        SessionQueue entry = queues.remove(key);
        if (entry == null) {
            return Collections.emptyList();
        }
        synchronized (entry) {
            List<Event> out = new ArrayList<>(entry.queue);
            entry.queue.clear();
            entry.lastText = null;
            entry.lastContextKey = null;
            return out;
        }
    }

    /**
     * Drain all event texts for a session.
     */
    public static List<String> drain(String sessionKey) {
        return drainEntries(sessionKey).stream()
                .map(Event::text)
                .collect(Collectors.toList());
    }

    /**
     * Peek at current event texts without draining.
     */
    public static List<String> peek(String sessionKey) {
        String key = requireSessionKey(sessionKey);
        SessionQueue entry = queues.get(key);
        if (entry == null) {
            return Collections.emptyList();
        }
        synchronized (entry) {
            return entry.queue.stream().map(Event::text).collect(Collectors.toList());
        }
    }

    /**
     * Check if a session has pending events.
     */
    public static boolean hasEvents(String sessionKey) {
        String key = requireSessionKey(sessionKey);
        SessionQueue entry = queues.get(key);
        return entry != null && !entry.queue.isEmpty();
    }

    /**
     * Clear all queues (for testing).
     */
    public static void resetForTest() {
        queues.clear();
    }
}
