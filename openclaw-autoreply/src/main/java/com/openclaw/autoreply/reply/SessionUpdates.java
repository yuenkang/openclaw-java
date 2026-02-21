package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

/**
 * Session update helpers — prepend system events to the inbound body,
 * ensure skill snapshots, and increment compaction counts.
 * Mirrors {@code auto-reply/reply/session-updates.ts}.
 */
public final class SessionUpdates {

    private static final Logger log = LoggerFactory.getLogger(SessionUpdates.class);

    private SessionUpdates() {
    }

    // --- System event timestamp formatting ---

    private static String formatUtcTimestamp(Instant instant) {
        return DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
                .withZone(ZoneOffset.UTC)
                .format(instant);
    }

    private static String formatZonedTimestamp(Instant instant, ZoneId zone) {
        ZonedDateTime zdt = instant.atZone(zone != null ? zone : ZoneId.systemDefault());
        return zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss z"));
    }

    private static String formatSystemEventTimestamp(long tsMs, Map<String, Object> cfg) {
        Instant instant = Instant.ofEpochMilli(tsMs);
        if (tsMs <= 0)
            return "unknown-time";

        String mode = resolveTimezoneMode(cfg);
        return switch (mode) {
            case "utc" -> formatUtcTimestamp(instant);
            case "local" -> formatZonedTimestamp(instant, ZoneId.systemDefault());
            default -> {
                try {
                    yield formatZonedTimestamp(instant, ZoneId.of(mode));
                } catch (Exception e) {
                    yield formatZonedTimestamp(instant, ZoneId.systemDefault());
                }
            }
        };
    }

    private static String resolveTimezoneMode(Map<String, Object> cfg) {
        // Full config lookup deferred — use local
        return "local";
    }

    /**
     * Compact a system event line — filter noise.
     */
    static String compactSystemEvent(String line) {
        if (line == null)
            return null;
        String trimmed = line.trim();
        if (trimmed.isEmpty())
            return null;
        String lower = trimmed.toLowerCase();
        if (lower.contains("reason periodic"))
            return null;
        if (lower.startsWith("read heartbeat.md"))
            return null;
        if (lower.contains("heartbeat poll") || lower.contains("heartbeat wake"))
            return null;
        if (trimmed.startsWith("Node:")) {
            return trimmed.replaceAll(" · last input [^·]+", "").trim();
        }
        return trimmed;
    }

    /**
     * Prepend system events to the inbound body.
     *
     * @param cfg              agent config
     * @param sessionKey       current session key
     * @param isMainSession    whether this is the main session
     * @param isNewSession     whether the session is new
     * @param prefixedBodyBase base body text with prefix
     * @return body with system events prepended
     */
    public static CompletableFuture<String> prependSystemEvents(
            Map<String, Object> cfg,
            String sessionKey,
            boolean isMainSession,
            boolean isNewSession,
            String prefixedBodyBase) {
        // Full system event drain deferred — return body unchanged
        return CompletableFuture.completedFuture(prefixedBodyBase);
    }

    /**
     * Ensure skill snapshot is fresh for the session.
     * Full integration deferred.
     */
    public static CompletableFuture<Map<String, Object>> ensureSkillSnapshot(
            Map<String, Object> sessionEntry,
            Map<String, Object> sessionStore,
            String sessionKey,
            String storePath,
            String sessionId,
            boolean isFirstTurnInSession,
            String workspaceDir,
            Map<String, Object> cfg) {
        Map<String, Object> result = new HashMap<>();
        result.put("sessionEntry", sessionEntry);
        result.put("skillsSnapshot", null);
        result.put("systemSent", sessionEntry != null
                ? sessionEntry.getOrDefault("systemSent", false)
                : false);
        return CompletableFuture.completedFuture(result);
    }

    /**
     * Increment compaction count in the session entry and persist.
     *
     * @return the new compaction count, or null if unavailable
     */
    public static CompletableFuture<Integer> incrementCompactionCount(
            Map<String, Object> sessionEntry,
            Map<String, Object> sessionStore,
            String sessionKey,
            String storePath,
            Long now,
            Integer tokensAfter) {
        if (sessionStore == null || sessionKey == null) {
            return CompletableFuture.completedFuture(null);
        }
        if (now == null)
            now = System.currentTimeMillis();

        @SuppressWarnings("unchecked")
        Map<String, Object> entry = sessionEntry != null ? sessionEntry
                : (Map<String, Object>) sessionStore.get(sessionKey);
        if (entry == null) {
            return CompletableFuture.completedFuture(null);
        }

        int current = entry.containsKey("compactionCount")
                ? ((Number) entry.get("compactionCount")).intValue()
                : 0;
        int nextCount = current + 1;
        entry.put("compactionCount", nextCount);
        entry.put("updatedAt", now);

        if (tokensAfter != null && tokensAfter > 0) {
            entry.put("totalTokens", tokensAfter);
            entry.remove("inputTokens");
            entry.remove("outputTokens");
        }

        sessionStore.put(sessionKey, entry);
        // Persistent store update deferred
        return CompletableFuture.completedFuture(nextCount);
    }
}
