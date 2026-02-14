package com.openclaw.channel.telegram;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Telegram bot access control: allow-lists for DMs and groups.
 * Corresponds to TypeScript's telegram/bot-access.ts.
 */
@Slf4j
public class TelegramBotAccess {

    /**
     * Normalized allow-from list.
     */
    public record NormalizedAllowFrom(
            List<String> entries,
            List<String> entriesLower,
            boolean hasWildcard,
            boolean hasEntries) {
    }

    /**
     * Allow-from match result.
     */
    public record AllowFromMatch(
            boolean allowed,
            String matchKey,
            String matchSource) {
        public static AllowFromMatch denied() {
            return new AllowFromMatch(false, null, null);
        }

        public static AllowFromMatch allowed(String matchKey, String source) {
            return new AllowFromMatch(true, matchKey, source);
        }
    }

    /**
     * Normalize an allow-from list, stripping telegram/tg prefixes and detecting
     * wildcards.
     */
    public static NormalizedAllowFrom normalizeAllowFrom(List<String> allowFrom) {
        if (allowFrom == null || allowFrom.isEmpty()) {
            return new NormalizedAllowFrom(List.of(), List.of(), false, false);
        }

        List<String> entries = allowFrom.stream()
                .map(v -> String.valueOf(v).trim())
                .filter(v -> !v.isEmpty())
                .toList();

        boolean hasWildcard = entries.contains("*");

        List<String> normalized = entries.stream()
                .filter(v -> !"*".equals(v))
                .map(v -> v.replaceFirst("(?i)^(telegram|tg):", ""))
                .toList();

        List<String> normalizedLower = normalized.stream()
                .map(String::toLowerCase)
                .toList();

        return new NormalizedAllowFrom(normalized, normalizedLower, hasWildcard, !entries.isEmpty());
    }

    /**
     * Normalize allow-from combining static config and dynamic store entries.
     */
    public static NormalizedAllowFrom normalizeAllowFromWithStore(
            List<String> allowFrom, List<String> storeAllowFrom) {

        List<String> combined = new ArrayList<>();
        if (allowFrom != null)
            combined.addAll(allowFrom);
        if (storeAllowFrom != null)
            combined.addAll(storeAllowFrom);
        return normalizeAllowFrom(combined);
    }

    /**
     * Check if a sender is allowed by the allow-from list.
     */
    public static boolean isSenderAllowed(
            NormalizedAllowFrom allow, String senderId, String senderUsername) {

        if (!allow.hasEntries())
            return true;
        if (allow.hasWildcard())
            return true;

        if (senderId != null && allow.entries().contains(senderId))
            return true;

        if (senderUsername == null || senderUsername.isBlank())
            return false;

        String username = senderUsername.toLowerCase();
        return allow.entriesLower().stream()
                .anyMatch(entry -> entry.equals(username)
                        || entry.equals("@" + username));
    }

    /**
     * Resolve a full allow-from match with source information.
     */
    public static AllowFromMatch resolveSenderAllowMatch(
            NormalizedAllowFrom allow, String senderId, String senderUsername) {

        if (allow.hasWildcard()) {
            return AllowFromMatch.allowed("*", "wildcard");
        }
        if (!allow.hasEntries()) {
            return AllowFromMatch.denied();
        }
        if (senderId != null && allow.entries().contains(senderId)) {
            return AllowFromMatch.allowed(senderId, "id");
        }

        if (senderUsername == null || senderUsername.isBlank()) {
            return AllowFromMatch.denied();
        }

        String username = senderUsername.toLowerCase();
        Optional<String> match = allow.entriesLower().stream()
                .filter(entry -> entry.equals(username) || entry.equals("@" + username))
                .findFirst();

        return match.map(m -> AllowFromMatch.allowed(m, "username"))
                .orElse(AllowFromMatch.denied());
    }

    /**
     * Convenience: check if a sender is allowed based on config and message data.
     */
    @SuppressWarnings("unchecked")
    public static boolean isSenderAllowed(
            OpenClawConfig config, String accountId, Map<String, Object> message) {

        // Extract sender info from message
        Object from = message.get("from");
        if (!(from instanceof Map))
            return false;

        Map<String, Object> sender = (Map<String, Object>) from;
        String senderId = String.valueOf(sender.getOrDefault("id", ""));
        String senderUsername = (String) sender.get("username");

        // Resolve allow-from from config
        var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, accountId);
        if (tgConfig == null)
            return true; // no config = allow all

        List<String> allowFrom = TelegramBotHelpers.resolveAllowFrom(tgConfig);
        NormalizedAllowFrom normalized = normalizeAllowFrom(allowFrom);

        log.debug("Access check: senderId={} senderUsername={} allowFrom={} normalized={} hasEntries={} hasWildcard={}",
                senderId, senderUsername, allowFrom, normalized.entries(), normalized.hasEntries(),
                normalized.hasWildcard());

        boolean result = isSenderAllowed(normalized, senderId, senderUsername);
        log.debug("Access check result: {} for senderId={} senderUsername={}", result, senderId, senderUsername);
        return result;
    }

    @SafeVarargs
    public static <T> T firstDefined(T... values) {
        for (T value : values) {
            if (value != null)
                return value;
        }
        return null;
    }
}
