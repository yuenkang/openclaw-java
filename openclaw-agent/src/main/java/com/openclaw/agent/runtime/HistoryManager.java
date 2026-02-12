package com.openclaw.agent.runtime;

import java.util.List;

import com.fasterxml.jackson.databind.JsonNode;
import com.openclaw.common.config.OpenClawConfig;

import lombok.extern.slf4j.Slf4j;

/**
 * Conversation history management utilities.
 * Corresponds to TypeScript's pi-embedded-runner/history.ts.
 *
 * <p>
 * Provides history turn limiting for DM sessions and per-channel
 * DM history limit resolution from config.
 * </p>
 */
@Slf4j
public final class HistoryManager {

    private HistoryManager() {
    }

    private static final java.util.regex.Pattern THREAD_SUFFIX_REGEX = java.util.regex.Pattern.compile(
            "^(.*)(?::(?:thread|topic):\\d+)$",
            java.util.regex.Pattern.CASE_INSENSITIVE);

    /**
     * Limits conversation history to the last N user turns (and their associated
     * assistant responses). This reduces token usage for long-running DM sessions.
     *
     * @param messages the full message list
     * @param limit    maximum number of user turns to keep; null or ≤0 means no
     *                 limit
     * @return trimmed message list
     */
    public static List<JsonNode> limitHistoryTurns(List<JsonNode> messages, Integer limit) {
        if (limit == null || limit <= 0 || messages == null || messages.isEmpty()) {
            return messages;
        }

        int userCount = 0;
        int lastUserIndex = messages.size();

        for (int i = messages.size() - 1; i >= 0; i--) {
            JsonNode msg = messages.get(i);
            if (msg != null && msg.has("role") && "user".equals(msg.get("role").asText())) {
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
     * Supports per-DM overrides and provider defaults.
     *
     * <p>
     * Session keys follow the pattern: {@code agent:AGENT_ID:PROVIDER:dm:USER_ID}
     * or {@code PROVIDER:dm:USER_ID}.
     * </p>
     *
     * @param sessionKey the session key
     * @param config     the OpenClaw configuration
     * @return the DM history limit, or null if not configured
     */
    public static Integer getDmHistoryLimitFromSessionKey(String sessionKey,
            OpenClawConfig config) {
        if (sessionKey == null || sessionKey.isBlank() || config == null) {
            return null;
        }

        String[] parts = sessionKey.split(":");
        // Filter out empty parts
        String[] filtered = java.util.Arrays.stream(parts)
                .filter(s -> !s.isEmpty())
                .toArray(String[]::new);

        // Skip "agent:AGENT_ID:" prefix if present
        String[] providerParts;
        if (filtered.length >= 3 && "agent".equals(filtered[0])) {
            providerParts = java.util.Arrays.copyOfRange(filtered, 2, filtered.length);
        } else {
            providerParts = filtered;
        }

        if (providerParts.length == 0) {
            return null;
        }

        String provider = providerParts[0].toLowerCase();
        String kind = providerParts.length > 1 ? providerParts[1].toLowerCase() : null;

        // Only apply DM history limits for DM sessions
        if (!"dm".equals(kind)) {
            return null;
        }

        // Build user ID from remaining parts
        String userIdRaw = providerParts.length > 2
                ? String.join(":", java.util.Arrays.copyOfRange(providerParts, 2, providerParts.length))
                : "";
        String userId = stripThreadSuffix(userIdRaw);

        // Resolve from config channels
        return resolveProviderDmLimit(config, provider, userId);
    }

    // ── Internals ─────────────────────────────────────────────────────

    private static String stripThreadSuffix(String value) {
        if (value == null || value.isEmpty()) {
            return value;
        }
        var matcher = THREAD_SUFFIX_REGEX.matcher(value);
        return matcher.matches() ? matcher.group(1) : value;
    }

    /**
     * Resolve DM history limit from channel config.
     * Checks per-user overrides first, then provider-level default.
     */
    private static Integer resolveProviderDmLimit(OpenClawConfig config,
            String provider, String userId) {
        // The TS version checks config.channels[provider].dmHistoryLimit
        // and config.channels[provider].dms[userId].historyLimit.
        // Since Java OpenClawConfig may not have a channels map yet,
        // we use config.getAdditionalProperties() or a dedicated getter.

        // For now, return null (no limit) until channels config is added.
        // TODO: wire up when OpenClawConfig.channels is available
        log.debug("dmHistoryLimit check: provider={} userId={} (not yet configured in Java config)",
                provider, userId);
        return null;
    }
}
