package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.function.BooleanSupplier;

/**
 * Agent runner helpers — audio payload detection, verbose-level-aware
 * tool-result emission predicates, followup drain scheduling, and
 * typing signal dispatch.
 * Mirrors {@code auto-reply/reply/agent-runner-helpers.ts}.
 */
public final class AgentRunnerHelpers {

    private static final Logger log = LoggerFactory.getLogger(AgentRunnerHelpers.class);

    private AgentRunnerHelpers() {
    }

    // --- Audio payload detection ---

    private static boolean hasAudioMedia(List<String> urls) {
        if (urls == null || urls.isEmpty())
            return false;
        return urls.stream().anyMatch(AgentRunnerHelpers::isAudioFileName);
    }

    private static boolean isAudioFileName(String url) {
        if (url == null)
            return false;
        String lower = url.toLowerCase();
        return lower.endsWith(".mp3") || lower.endsWith(".wav") || lower.endsWith(".ogg")
                || lower.endsWith(".m4a") || lower.endsWith(".aac") || lower.endsWith(".flac")
                || lower.endsWith(".opus") || lower.endsWith(".webm");
    }

    /**
     * Check if a reply payload contains audio media.
     */
    public static boolean isAudioPayload(Map<String, Object> payload) {
        @SuppressWarnings("unchecked")
        List<String> urls = (List<String>) payload.get("mediaUrls");
        if (urls == null) {
            String single = (String) payload.get("mediaUrl");
            if (single != null)
                urls = List.of(single);
        }
        return hasAudioMedia(urls);
    }

    // --- Verbose-level tool emission predicates ---

    /**
     * Create a supplier that checks whether tool results should be emitted
     * in verbose mode. Re-reads session store each call for live updates.
     */
    public static BooleanSupplier createShouldEmitToolResult(
            String sessionKey, String storePath, String resolvedVerboseLevel) {
        String fallback = normalizeVerboseLevel(resolvedVerboseLevel);
        if (fallback == null)
            fallback = "off";
        String fb = fallback;
        return () -> {
            if (sessionKey == null || storePath == null) {
                return !"off".equals(fb);
            }
            // Full session store re-read deferred
            return !"off".equals(fb);
        };
    }

    /**
     * Create a supplier that checks whether tool output (full) should be emitted.
     */
    public static BooleanSupplier createShouldEmitToolOutput(
            String sessionKey, String storePath, String resolvedVerboseLevel) {
        String fallback = normalizeVerboseLevel(resolvedVerboseLevel);
        if (fallback == null)
            fallback = "off";
        String fb = fallback;
        return () -> {
            if (sessionKey == null || storePath == null) {
                return "full".equals(fb);
            }
            return "full".equals(fb);
        };
    }

    static String normalizeVerboseLevel(String raw) {
        if (raw == null)
            return null;
        String lower = raw.trim().toLowerCase();
        if (lower.isEmpty() || "false".equals(lower))
            return "off";
        if ("true".equals(lower) || "on".equals(lower))
            return "on";
        if ("full".equals(lower))
            return "full";
        if ("off".equals(lower))
            return "off";
        return null;
    }

    // --- Followup drain scheduling ---

    /**
     * Finalize with a followup drain schedule.
     */
    public static <T> T finalizeWithFollowup(T value, String queueKey,
            Runnable runFollowupTurn) {
        // scheduleFollowupDrain deferred
        if (runFollowupTurn != null) {
            runFollowupTurn.run();
        }
        return value;
    }

    // --- Typing signal dispatch ---

    /**
     * Signal typing if any payload has renderable content.
     */
    public static void signalTypingIfNeeded(
            List<Map<String, Object>> payloads,
            TypingMode.TypingSignaler typingSignals) {
        if (payloads == null || typingSignals == null)
            return;
        boolean shouldSignal = payloads.stream().anyMatch(payload -> {
            String text = (String) payload.get("text");
            if (text != null && !text.trim().isEmpty())
                return true;
            if (payload.get("mediaUrl") != null)
                return true;
            @SuppressWarnings("unchecked")
            List<String> urls = (List<String>) payload.get("mediaUrls");
            return urls != null && !urls.isEmpty();
        });
        if (shouldSignal) {
            typingSignals.signalRunStart();
        }
    }
}
