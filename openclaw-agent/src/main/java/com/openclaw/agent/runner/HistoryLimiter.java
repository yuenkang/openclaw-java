package com.openclaw.agent.runner;

import java.util.*;
import java.util.regex.Pattern;

/**
 * Conversation history limiting and DM history limit resolution.
 * Mirrors {@code agents/pi-embedded-runner/history.ts}.
 */
public final class HistoryLimiter {

    private HistoryLimiter() {
    }

    private static final Pattern THREAD_SUFFIX_RE = Pattern.compile("^(.*)(?::(?:thread|topic):\\d+)$",
            Pattern.CASE_INSENSITIVE);

    static String stripThreadSuffix(String value) {
        if (value == null)
            return value;
        var m = THREAD_SUFFIX_RE.matcher(value);
        return m.matches() ? m.group(1) : value;
    }

    /**
     * Limit conversation history to the last N user turns and their associated
     * responses.
     */
    public static List<Map<String, Object>> limitHistoryTurns(
            List<Map<String, Object>> messages, Integer limit) {
        if (limit == null || limit <= 0 || messages == null || messages.isEmpty()) {
            return messages;
        }
        int userCount = 0;
        int lastUserIndex = messages.size();

        for (int i = messages.size() - 1; i >= 0; i--) {
            Map<String, Object> msg = messages.get(i);
            if (msg != null && "user".equals(msg.get("role"))) {
                userCount++;
                if (userCount > limit) {
                    return messages.subList(lastUserIndex, messages.size());
                }
                lastUserIndex = i;
            }
        }
        return messages;
    }

    /**
     * Extract provider + user ID from a session key and look up dmHistoryLimit.
     * Session key format: "agent:xxx:provider:dm:userId:thread:123"
     */
    @SuppressWarnings("unchecked")
    public static Integer getDmHistoryLimitFromSessionKey(
            String sessionKey, Map<String, Object> config) {
        if (sessionKey == null || config == null)
            return null;

        String[] parts = sessionKey.split(":");
        List<String> filtered = new ArrayList<>();
        for (String p : parts) {
            if (!p.isEmpty())
                filtered.add(p);
        }

        // Strip "agent:xxx:" prefix
        List<String> providerParts = filtered.size() >= 3 && "agent".equals(filtered.get(0))
                ? filtered.subList(2, filtered.size())
                : filtered;
        if (providerParts.isEmpty())
            return null;

        String provider = providerParts.get(0).toLowerCase();
        String kind = providerParts.size() > 1 ? providerParts.get(1).toLowerCase() : "";
        if (!"dm".equals(kind))
            return null;

        String userIdRaw = providerParts.size() > 2
                ? String.join(":", providerParts.subList(2, providerParts.size()))
                : "";
        String userId = stripThreadSuffix(userIdRaw);

        // Resolve from config.channels.<provider>
        Object channelsObj = config.get("channels");
        if (!(channelsObj instanceof Map<?, ?> channels))
            return null;
        Object provObj = channels.get(provider);
        if (!(provObj instanceof Map<?, ?> provConfig))
            return null;

        // Per-DM override
        if (!userId.isEmpty()) {
            Object dmsObj = provConfig.get("dms");
            if (dmsObj instanceof Map<?, ?> dms) {
                Object userEntry = dms.get(userId);
                if (userEntry instanceof Map<?, ?> userMap) {
                    Object limit = userMap.get("historyLimit");
                    if (limit instanceof Number n)
                        return n.intValue();
                }
            }
        }

        // Provider default
        Object defaultLimit = provConfig.get("dmHistoryLimit");
        if (defaultLimit instanceof Number n)
            return n.intValue();
        return null;
    }
}
