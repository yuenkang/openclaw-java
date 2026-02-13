package com.openclaw.agent.autoreply.reply.queue;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Session queue cleanup — clears followup queues and command lanes.
 * Mirrors {@code auto-reply/reply/queue/cleanup.ts}.
 */
public final class QueueCleanup {

    private QueueCleanup() {
    }

    /** Result of clearing session queues. */
    public record ClearSessionQueueResult(
            int followupCleared,
            int laneCleared,
            List<String> keys) {
    }

    /**
     * Clear followup queues (and optionally command lanes) for the given keys.
     * Null/blank keys are skipped; duplicates are deduplicated.
     */
    public static ClearSessionQueueResult clearSessionQueues(List<String> keys) {
        Set<String> seen = new HashSet<>();
        int followupCleared = 0;
        int laneCleared = 0;
        List<String> clearedKeys = new ArrayList<>();

        for (String key : keys) {
            String cleaned = key != null ? key.trim() : "";
            if (cleaned.isEmpty() || seen.contains(cleaned))
                continue;
            seen.add(cleaned);
            clearedKeys.add(cleaned);
            followupCleared += QueueState.clearFollowupQueue(cleaned);
            // Lane clearing would go here:
            // laneCleared += clearCommandLane(resolveEmbeddedSessionLane(cleaned));
        }

        return new ClearSessionQueueResult(followupCleared, laneCleared, clearedKeys);
    }
}
