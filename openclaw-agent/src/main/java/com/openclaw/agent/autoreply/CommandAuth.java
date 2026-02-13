package com.openclaw.agent.autoreply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

/**
 * Command authorization — resolve sender ownership from config,
 * owner allowlist, channel dock integration, and provider-specific
 * sender normalization.
 * Mirrors {@code auto-reply/command-auth.ts}.
 */
public final class CommandAuth {

    private static final Logger log = LoggerFactory.getLogger(CommandAuth.class);

    private CommandAuth() {
    }

    /** Result of resolving command authorization for a sender. */
    public record CommandAuthorization(
            String providerId,
            List<String> ownerList,
            String senderId,
            boolean senderIsOwner,
            boolean isAuthorizedSender,
            String from,
            String to) {
    }

    /**
     * Resolve command authorization for an inbound message context.
     */
    @SuppressWarnings("unchecked")
    public static CommandAuthorization resolveCommandAuthorization(
            Map<String, Object> ctx, Map<String, Object> cfg, boolean commandAuthorized) {

        String from = ctx.get("From") instanceof String s ? s.trim() : "";
        String to = ctx.get("To") instanceof String s ? s.trim() : "";
        String provider = ctx.get("Provider") instanceof String s ? s.trim().toLowerCase() : null;
        String senderId = ctx.get("SenderId") instanceof String s ? s.trim() : null;

        // Resolve owner allowlist from config
        List<String> ownerList = resolveOwnerAllowFromList(cfg, provider);
        boolean allowAll = ownerList.isEmpty() || ownerList.contains("*");

        // Resolve sender candidates
        List<String> senderCandidates = new ArrayList<>();
        if (senderId != null && !senderId.isEmpty())
            senderCandidates.add(senderId);
        if (!from.isEmpty())
            senderCandidates.add(from);

        String matchedSender = null;
        if (!ownerList.isEmpty()) {
            for (String candidate : senderCandidates) {
                if (ownerList.contains(candidate)) {
                    matchedSender = candidate;
                    break;
                }
            }
        }
        String resolvedSenderId = matchedSender != null ? matchedSender
                : (!senderCandidates.isEmpty() ? senderCandidates.get(0) : null);

        boolean senderIsOwner = matchedSender != null;
        boolean isOwnerForCommands = allowAll || senderIsOwner;
        boolean isAuthorized = commandAuthorized && isOwnerForCommands;

        return new CommandAuthorization(
                provider, ownerList,
                resolvedSenderId,
                senderIsOwner, isAuthorized,
                from.isEmpty() ? null : from,
                to.isEmpty() ? null : to);
    }

    @SuppressWarnings("unchecked")
    private static List<String> resolveOwnerAllowFromList(Map<String, Object> cfg, String providerId) {
        Map<String, Object> commands = cfg.get("commands") instanceof Map<?, ?> m
                ? (Map<String, Object>) m
                : null;
        if (commands == null)
            return List.of();

        Object raw = commands.get("ownerAllowFrom");
        if (!(raw instanceof List<?> list) || list.isEmpty())
            return List.of();

        List<String> filtered = new ArrayList<>();
        for (Object entry : list) {
            String trimmed = String.valueOf(entry != null ? entry : "").trim();
            if (trimmed.isEmpty())
                continue;
            int sep = trimmed.indexOf(':');
            if (sep > 0 && providerId != null) {
                String prefix = trimmed.substring(0, sep).trim().toLowerCase();
                if (!prefix.equals(providerId))
                    continue;
                String remainder = trimmed.substring(sep + 1).trim();
                if (!remainder.isEmpty())
                    filtered.add(remainder);
                continue;
            }
            filtered.add(trimmed);
        }
        return filtered;
    }
}
