package com.openclaw.media;

import java.util.List;

/**
 * Media understanding scope — rule-based allow/deny decisions per
 * channel/session.
 * Corresponds to TypeScript's media-understanding/scope.ts.
 */
public final class MediaScope {

    private MediaScope() {
    }

    public enum ScopeDecision {
        ALLOW, DENY
    }

    /**
     * Scope rule match criteria.
     */
    public record ScopeRuleMatch(String channel, String chatType, String keyPrefix) {
    }

    /**
     * A single scope rule.
     */
    public record ScopeRule(String action, ScopeRuleMatch match) {
    }

    /**
     * Scope configuration — a default action plus ordered rules.
     */
    public record ScopeConfig(String defaultAction, List<ScopeRule> rules) {
    }

    // =========================================================================
    // Resolution
    // =========================================================================

    /**
     * Resolve whether media understanding is allowed for the given context.
     *
     * @param scope      scope configuration (may be null → allow)
     * @param sessionKey current session key
     * @param channel    current channel identifier
     * @param chatType   current chat type
     * @return ALLOW or DENY
     */
    public static ScopeDecision resolve(
            ScopeConfig scope, String sessionKey, String channel, String chatType) {

        if (scope == null) {
            return ScopeDecision.ALLOW;
        }

        String normalizedChannel = normalize(channel);
        String normalizedChatType = normalizeChatType(chatType);
        String normalizedSessionKey = normalize(sessionKey);
        if (normalizedSessionKey == null)
            normalizedSessionKey = "";

        if (scope.rules() != null) {
            for (ScopeRule rule : scope.rules()) {
                if (rule == null)
                    continue;
                ScopeDecision action = normalizeDecision(rule.action());
                if (action == null)
                    action = ScopeDecision.ALLOW;

                ScopeRuleMatch match = rule.match();
                if (match == null) {
                    return action;
                }

                String matchChannel = normalize(match.channel());
                String matchChatType = normalizeChatType(match.chatType());
                String matchPrefix = normalize(match.keyPrefix());

                if (matchChannel != null && !matchChannel.equals(normalizedChannel)) {
                    continue;
                }
                if (matchChatType != null && !matchChatType.equals(normalizedChatType)) {
                    continue;
                }
                if (matchPrefix != null && !normalizedSessionKey.startsWith(matchPrefix)) {
                    continue;
                }
                return action;
            }
        }

        ScopeDecision defaultDecision = normalizeDecision(scope.defaultAction());
        return defaultDecision != null ? defaultDecision : ScopeDecision.ALLOW;
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    static ScopeDecision normalizeDecision(String value) {
        if (value == null)
            return null;
        String trimmed = value.trim().toLowerCase();
        return switch (trimmed) {
            case "allow" -> ScopeDecision.ALLOW;
            case "deny" -> ScopeDecision.DENY;
            default -> null;
        };
    }

    static String normalize(String value) {
        if (value == null)
            return null;
        String trimmed = value.trim().toLowerCase();
        return trimmed.isEmpty() ? null : trimmed;
    }

    static String normalizeChatType(String raw) {
        if (raw == null)
            return null;
        String trimmed = raw.trim().toLowerCase();
        return trimmed.isEmpty() ? null : trimmed;
    }
}
