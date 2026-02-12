package com.openclaw.agent.tools.builtin;

import com.openclaw.common.config.OpenClawConfig;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Helpers for sessions_send tool — announce/reply context builders and
 * ping-pong turn config.
 * Corresponds to TypeScript sessions-send-helpers.ts.
 */
public class SessionsSendHelpers {

    public static final String ANNOUNCE_SKIP_TOKEN = "ANNOUNCE_SKIP";
    public static final String REPLY_SKIP_TOKEN = "REPLY_SKIP";
    private static final int DEFAULT_PING_PONG_TURNS = 5;
    private static final int MAX_PING_PONG_TURNS = 5;

    // --- Announce target resolution ---

    public record AnnounceTarget(String channel, String to, String accountId, String threadId) {
    }

    private static final Pattern TOPIC_RE = Pattern.compile(":topic:(\\d+)$");
    private static final Pattern THREAD_RE = Pattern.compile(":thread:(\\d+)$");

    /**
     * Parse a session key to extract an announce target (channel + to + threadId).
     */
    public static AnnounceTarget resolveAnnounceTargetFromKey(String sessionKey) {
        if (sessionKey == null || sessionKey.isBlank())
            return null;
        String[] rawParts = sessionKey.split(":");
        List<String> parts = new ArrayList<>();
        for (String p : rawParts) {
            if (!p.isEmpty())
                parts.add(p);
        }
        // Strip agent: prefix
        if (parts.size() >= 3 && "agent".equals(parts.get(0))) {
            parts = parts.subList(2, parts.size());
        }
        if (parts.size() < 3)
            return null;

        String channelRaw = parts.get(0);
        String kind = parts.get(1);
        if (!"group".equals(kind) && !"channel".equals(kind))
            return null;

        String restJoined = String.join(":", parts.subList(2, parts.size()));

        // Extract thread/topic ID
        String threadId = null;
        Matcher topicMatch = TOPIC_RE.matcher(restJoined);
        Matcher threadMatch = THREAD_RE.matcher(restJoined);
        Matcher match = topicMatch.find() ? topicMatch : (threadMatch.find() ? threadMatch : null);
        if (match != null) {
            threadId = match.group(1);
        }

        // Remove :topic:N or :thread:N suffix
        String id = match != null
                ? restJoined.replaceAll(":(topic|thread):\\d+$", "")
                : restJoined.trim();

        if (id.isEmpty() || channelRaw.isEmpty())
            return null;

        String channel = channelRaw.toLowerCase();
        String kindTarget;
        if ("discord".equals(channel) || "slack".equals(channel)) {
            kindTarget = "channel:" + id;
        } else {
            kindTarget = "channel".equals(kind) ? "channel:" + id : "group:" + id;
        }

        return new AnnounceTarget(channel, kindTarget, null, threadId);
    }

    // --- Agent-to-agent context builders ---

    public static String buildAgentToAgentMessageContext(
            String requesterSessionKey, String requesterChannel, String targetSessionKey) {
        List<String> lines = new ArrayList<>();
        lines.add("Agent-to-agent message context:");
        if (requesterSessionKey != null && !requesterSessionKey.isBlank()) {
            lines.add("Agent 1 (requester) session: " + requesterSessionKey + ".");
        }
        if (requesterChannel != null && !requesterChannel.isBlank()) {
            lines.add("Agent 1 (requester) channel: " + requesterChannel + ".");
        }
        lines.add("Agent 2 (target) session: " + targetSessionKey + ".");
        return String.join("\n", lines);
    }

    public static String buildAgentToAgentReplyContext(
            String requesterSessionKey, String requesterChannel,
            String targetSessionKey, String targetChannel,
            String currentRole, int turn, int maxTurns) {
        String currentLabel = "requester".equals(currentRole)
                ? "Agent 1 (requester)"
                : "Agent 2 (target)";
        List<String> lines = new ArrayList<>();
        lines.add("Agent-to-agent reply step:");
        lines.add("Current agent: " + currentLabel + ".");
        lines.add("Turn " + turn + " of " + maxTurns + ".");
        if (requesterSessionKey != null && !requesterSessionKey.isBlank()) {
            lines.add("Agent 1 (requester) session: " + requesterSessionKey + ".");
        }
        if (requesterChannel != null && !requesterChannel.isBlank()) {
            lines.add("Agent 1 (requester) channel: " + requesterChannel + ".");
        }
        lines.add("Agent 2 (target) session: " + targetSessionKey + ".");
        if (targetChannel != null && !targetChannel.isBlank()) {
            lines.add("Agent 2 (target) channel: " + targetChannel + ".");
        }
        lines.add("If you want to stop the ping-pong, reply exactly \"" + REPLY_SKIP_TOKEN + "\".");
        return String.join("\n", lines);
    }

    public static String buildAgentToAgentAnnounceContext(
            String requesterSessionKey, String requesterChannel,
            String targetSessionKey, String targetChannel,
            String originalMessage, String roundOneReply, String latestReply) {
        List<String> lines = new ArrayList<>();
        lines.add("Agent-to-agent announce step:");
        if (requesterSessionKey != null && !requesterSessionKey.isBlank()) {
            lines.add("Agent 1 (requester) session: " + requesterSessionKey + ".");
        }
        if (requesterChannel != null && !requesterChannel.isBlank()) {
            lines.add("Agent 1 (requester) channel: " + requesterChannel + ".");
        }
        lines.add("Agent 2 (target) session: " + targetSessionKey + ".");
        if (targetChannel != null && !targetChannel.isBlank()) {
            lines.add("Agent 2 (target) channel: " + targetChannel + ".");
        }
        lines.add("Original request: " + originalMessage);
        lines.add(roundOneReply != null ? "Round 1 reply: " + roundOneReply : "Round 1 reply: (not available).");
        lines.add(latestReply != null ? "Latest reply: " + latestReply : "Latest reply: (not available).");
        lines.add("If you want to remain silent, reply exactly \"" + ANNOUNCE_SKIP_TOKEN + "\".");
        lines.add("Any other reply will be posted to the target channel.");
        lines.add("After this reply, the agent-to-agent conversation is over.");
        return String.join("\n", lines);
    }

    public static boolean isAnnounceSkip(String text) {
        return text != null && ANNOUNCE_SKIP_TOKEN.equals(text.trim());
    }

    public static boolean isReplySkip(String text) {
        return text != null && REPLY_SKIP_TOKEN.equals(text.trim());
    }

    public static int resolvePingPongTurns(OpenClawConfig cfg) {
        if (cfg == null || cfg.getSession() == null)
            return DEFAULT_PING_PONG_TURNS;
        var a2a = cfg.getSession().getAgentToAgent();
        if (a2a == null)
            return DEFAULT_PING_PONG_TURNS;
        Integer raw = a2a.getMaxPingPongTurns();
        if (raw == null)
            return DEFAULT_PING_PONG_TURNS;
        return Math.max(0, Math.min(MAX_PING_PONG_TURNS, raw));
    }
}
