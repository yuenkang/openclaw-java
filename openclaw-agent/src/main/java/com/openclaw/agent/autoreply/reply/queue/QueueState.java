package com.openclaw.agent.autoreply.reply.queue;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * In-memory followup queue state with defaults and lifecycle.
 * Mirrors {@code auto-reply/reply/queue/state.ts}.
 */
public final class QueueState {

    private QueueState() {
    }

    public static final int DEFAULT_QUEUE_DEBOUNCE_MS = 1000;
    public static final int DEFAULT_QUEUE_CAP = 20;
    public static final String DEFAULT_QUEUE_DROP = QueueTypes.DROP_SUMMARIZE;

    /** Mutable state for a single followup queue. */
    public static final class FollowupQueueState {
        public final List<QueueTypes.FollowupRun> items = new ArrayList<>();
        public volatile boolean draining;
        public volatile long lastEnqueuedAt;
        public volatile String mode;
        public volatile int debounceMs;
        public volatile int cap;
        public volatile String dropPolicy;
        public volatile int droppedCount;
        public final List<String> summaryLines = new ArrayList<>();
        public volatile QueueTypes.FollowupRunConfig lastRun;
    }

    /** Global map of session key → queue state. */
    public static final Map<String, FollowupQueueState> FOLLOWUP_QUEUES = new ConcurrentHashMap<>();

    /**
     * Get or create the followup queue for the given key,
     * applying the supplied settings.
     */
    public static FollowupQueueState getFollowupQueue(String key, QueueTypes.QueueSettings settings) {
        FollowupQueueState existing = FOLLOWUP_QUEUES.get(key);
        if (existing != null) {
            existing.mode = settings.mode();
            if (settings.debounceMs() != null) {
                existing.debounceMs = Math.max(0, settings.debounceMs());
            }
            if (settings.cap() != null && settings.cap() > 0) {
                existing.cap = settings.cap();
            }
            if (settings.dropPolicy() != null) {
                existing.dropPolicy = settings.dropPolicy();
            }
            return existing;
        }

        FollowupQueueState created = new FollowupQueueState();
        created.mode = settings.mode();
        created.debounceMs = settings.debounceMs() != null
                ? Math.max(0, settings.debounceMs())
                : DEFAULT_QUEUE_DEBOUNCE_MS;
        created.cap = settings.cap() != null && settings.cap() > 0
                ? settings.cap()
                : DEFAULT_QUEUE_CAP;
        created.dropPolicy = settings.dropPolicy() != null
                ? settings.dropPolicy()
                : DEFAULT_QUEUE_DROP;
        FOLLOWUP_QUEUES.put(key, created);
        return created;
    }

    /**
     * Clear and remove the followup queue for the given key.
     *
     * @return number of items + dropped count that were cleared
     */
    public static int clearFollowupQueue(String key) {
        String cleaned = key != null ? key.trim() : "";
        if (cleaned.isEmpty())
            return 0;
        FollowupQueueState queue = FOLLOWUP_QUEUES.remove(cleaned);
        if (queue == null)
            return 0;
        int cleared = queue.items.size() + queue.droppedCount;
        queue.items.clear();
        queue.droppedCount = 0;
        queue.summaryLines.clear();
        queue.lastRun = null;
        queue.lastEnqueuedAt = 0;
        return cleared;
    }
}
