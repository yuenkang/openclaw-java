package com.openclaw.agent.autoreply.reply.queue;

/**
 * Normalization utilities for queue mode and drop policy strings.
 * Mirrors {@code auto-reply/reply/queue/normalize.ts}.
 */
public final class QueueNormalize {

    private QueueNormalize() {
    }

    /**
     * Normalize a raw queue mode string to one of the canonical mode values.
     *
     * @return canonical mode or {@code null} if unrecognized
     */
    public static String normalizeQueueMode(String raw) {
        if (raw == null || raw.isBlank())
            return null;
        String cleaned = raw.trim().toLowerCase();
        return switch (cleaned) {
            case "queue", "queued" -> QueueTypes.MODE_STEER;
            case "interrupt", "interrupts", "abort" -> QueueTypes.MODE_INTERRUPT;
            case "steer", "steering" -> QueueTypes.MODE_STEER;
            case "followup", "follow-ups", "followups" -> QueueTypes.MODE_FOLLOWUP;
            case "collect", "coalesce" -> QueueTypes.MODE_COLLECT;
            case "steer+backlog", "steer-backlog", "steer_backlog" -> QueueTypes.MODE_STEER_BACKLOG;
            default -> null;
        };
    }

    /**
     * Normalize a raw queue drop policy string.
     *
     * @return canonical drop policy or {@code null} if unrecognized
     */
    public static String normalizeQueueDropPolicy(String raw) {
        if (raw == null || raw.isBlank())
            return null;
        String cleaned = raw.trim().toLowerCase();
        return switch (cleaned) {
            case "old", "oldest" -> QueueTypes.DROP_OLD;
            case "new", "newest" -> QueueTypes.DROP_NEW;
            case "summarize", "summary" -> QueueTypes.DROP_SUMMARIZE;
            default -> null;
        };
    }
}
