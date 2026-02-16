package com.openclaw.app.commands;

import com.openclaw.channel.telegram.TelegramBotHelpers;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Resolves sender authorization for commands.
 * Simplified Java version of TypeScript's {@code command-auth.ts}.
 * <p>
 * Logic:
 * 1. Merge ownerAllowFrom (from commands config) + allowFrom (from Telegram
 * account config)
 * 2. Empty list → everyone is authorized
 * 3. List contains "*" → everyone is authorized
 * 4. Otherwise → senderId must be in the merged list
 */
@Slf4j
public class CommandAuthorization {

    /**
     * Check if a sender is authorized to execute commands.
     *
     * @param senderId  the sender's Telegram user ID (may be null)
     * @param config    the current config
     * @param accountId the Telegram account ID (may be null)
     * @return true if the sender is authorized
     */
    public static boolean isAuthorizedSender(
            String senderId, OpenClawConfig config, String accountId) {

        List<String> allowList = resolveAllowList(config, accountId);

        // Empty list → no restrictions → everyone authorized
        if (allowList.isEmpty()) {
            return true;
        }

        // Wildcard → everyone authorized
        if (allowList.stream().anyMatch(e -> "*".equals(e.trim()))) {
            return true;
        }

        // No sender ID → cannot verify → deny
        if (senderId == null || senderId.isBlank()) {
            log.warn("No senderId provided but allowlist is configured — denying access");
            return false;
        }

        // Check if sender is in the allow list
        String trimmedSender = senderId.trim();
        boolean authorized = allowList.stream()
                .map(String::trim)
                .anyMatch(entry -> entry.equals(trimmedSender));

        if (!authorized) {
            log.info("Sender {} is not in allowlist (size={})", trimmedSender, allowList.size());
        }
        return authorized;
    }

    /**
     * Resolve the merged allow list from both commands config and Telegram channel
     * config.
     */
    public static List<String> resolveAllowList(OpenClawConfig config, String accountId) {
        Set<String> merged = new LinkedHashSet<>();

        // 1. From commands.ownerAllowFrom
        if (config != null && config.getCommands() != null) {
            var ownerAllowFrom = config.getCommands().getOwnerAllowFrom();
            if (ownerAllowFrom != null) {
                for (Object entry : ownerAllowFrom) {
                    String s = String.valueOf(entry).trim();
                    if (!s.isEmpty() && !"null".equals(s)) {
                        merged.add(s);
                    }
                }
            }
        }

        // 2. From telegram account config allowFrom
        if (config != null) {
            var tgConfig = TelegramBotHelpers.resolveTelegramAccountConfig(config, accountId);
            List<String> channelAllowFrom = TelegramBotHelpers.resolveAllowFrom(tgConfig);
            for (String entry : channelAllowFrom) {
                String s = String.valueOf(entry).trim();
                if (!s.isEmpty()) {
                    merged.add(s);
                }
            }
        }

        return new ArrayList<>(merged);
    }
}
