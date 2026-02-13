package com.openclaw.agent.autoreply.reply.queue;

import java.util.List;
import java.util.Objects;

/**
 * Enqueue followup runs with deduplication and drop-policy enforcement.
 * Mirrors {@code auto-reply/reply/queue/enqueue.ts}.
 */
public final class QueueEnqueue {

    private QueueEnqueue() {
    }

    /**
     * Check if a run is already queued (by message ID or prompt),
     * considering routing fields for cross-channel deduplication.
     */
    static boolean isRunAlreadyQueued(
            QueueTypes.FollowupRun run,
            List<QueueTypes.FollowupRun> items,
            boolean allowPromptFallback) {

        String messageId = run.messageId() != null ? run.messageId().trim() : "";
        for (var item : items) {
            if (!hasSameRouting(item, run))
                continue;
            if (!messageId.isEmpty()) {
                String itemId = item.messageId() != null ? item.messageId().trim() : "";
                if (messageId.equals(itemId))
                    return true;
            }
        }
        if (messageId.isEmpty() || !allowPromptFallback) {
            return !messageId.isEmpty(); // already checked by id
        }
        // Prompt fallback
        return items.stream()
                .anyMatch(item -> hasSameRouting(item, run) && Objects.equals(item.prompt(), run.prompt()));
    }

    private static boolean hasSameRouting(QueueTypes.FollowupRun a, QueueTypes.FollowupRun b) {
        return Objects.equals(a.originatingChannel(), b.originatingChannel())
                && Objects.equals(a.originatingTo(), b.originatingTo())
                && Objects.equals(a.originatingAccountId(), b.originatingAccountId())
                && Objects.equals(a.originatingThreadId(), b.originatingThreadId());
    }

    /**
     * Enqueue a followup run.
     *
     * @param key        session queue key
     * @param run        the followup run to enqueue
     * @param settings   resolved queue settings
     * @param dedupeMode "message-id" | "prompt" | "none"
     * @return true if enqueued, false if deduplicated or dropped
     */
    public static boolean enqueueFollowupRun(
            String key,
            QueueTypes.FollowupRun run,
            QueueTypes.QueueSettings settings,
            String dedupeMode) {

        if (dedupeMode == null)
            dedupeMode = QueueTypes.DEDUPE_MESSAGE_ID;
        QueueState.FollowupQueueState queue = QueueState.getFollowupQueue(key, settings);

        // Deduplication
        if (!QueueTypes.DEDUPE_NONE.equals(dedupeMode)) {
            boolean allowPrompt = QueueTypes.DEDUPE_PROMPT.equals(dedupeMode);
            if (isRunAlreadyQueued(run, queue.items, allowPrompt)) {
                return false;
            }
        }

        queue.lastEnqueuedAt = System.currentTimeMillis();
        queue.lastRun = run.run();

        // Enforce cap via drop policy
        if (queue.cap > 0 && queue.items.size() >= queue.cap) {
            String dropPolicy = queue.dropPolicy != null ? queue.dropPolicy : QueueState.DEFAULT_QUEUE_DROP;
            if (QueueTypes.DROP_NEW.equals(dropPolicy)) {
                queue.droppedCount++;
                String summary = run.summaryLine() != null && !run.summaryLine().isBlank()
                        ? run.summaryLine().trim()
                        : run.prompt().trim();
                queue.summaryLines.add(summary);
                return false;
            }
            // "old" or "summarize" — remove oldest
            if (!queue.items.isEmpty()) {
                QueueTypes.FollowupRun dropped = queue.items.remove(0);
                queue.droppedCount++;
                String summary = dropped.summaryLine() != null && !dropped.summaryLine().isBlank()
                        ? dropped.summaryLine().trim()
                        : dropped.prompt().trim();
                queue.summaryLines.add(summary);
            }
        }

        queue.items.add(run);
        return true;
    }

    /**
     * Convenience overload with default deduplication mode.
     */
    public static boolean enqueueFollowupRun(
            String key, QueueTypes.FollowupRun run, QueueTypes.QueueSettings settings) {
        return enqueueFollowupRun(key, run, settings, QueueTypes.DEDUPE_MESSAGE_ID);
    }

    /**
     * Get the current queue depth for a session key.
     */
    public static int getFollowupQueueDepth(String key) {
        String cleaned = key != null ? key.trim() : "";
        if (cleaned.isEmpty())
            return 0;
        QueueState.FollowupQueueState queue = QueueState.FOLLOWUP_QUEUES.get(cleaned);
        return queue != null ? queue.items.size() : 0;
    }
}
