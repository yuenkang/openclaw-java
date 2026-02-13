package com.openclaw.common.config;

import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Group session key resolution and display name building.
 * Corresponds to TypeScript's sessions/group.ts.
 */
public final class SessionGroupKey {

    private SessionGroupKey() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    public record GroupKeyResolution(String key, String channel, String id, String chatType) {
    }

    // =========================================================================
    // Known group surfaces
    // =========================================================================

    /**
     * Known deliverable message channels that support group sessions.
     * Mirrors listDeliverableMessageChannels() + "webchat".
     */
    private static final Set<String> GROUP_SURFACES = Set.of(
            "discord", "slack", "telegram", "whatsapp", "signal",
            "msteams", "googlechat", "imessage", "webchat");

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Normalize a group label for use as a session-key token.
     */
    public static String normalizeGroupLabel(String raw) {
        String trimmed = (raw != null ? raw.trim().toLowerCase() : "");
        if (trimmed.isEmpty()) {
            return "";
        }
        String dashed = Pattern.compile("\\s+").matcher(trimmed).replaceAll("-");
        String cleaned = Pattern.compile("[^a-z0-9#@._+\\-]+").matcher(dashed).replaceAll("-");
        String result = Pattern.compile("-{2,}").matcher(cleaned).replaceAll("-");
        result = Pattern.compile("^[-.]+|[-.]+$").matcher(result).replaceAll("");
        return result;
    }

    /**
     * Shorten a group ID for display.
     */
    public static String shortenGroupId(String value) {
        String trimmed = (value != null ? value.trim() : "");
        if (trimmed.isEmpty()) {
            return "";
        }
        if (trimmed.length() <= 14) {
            return trimmed;
        }
        return trimmed.substring(0, 6) + "..." + trimmed.substring(trimmed.length() - 4);
    }

    /**
     * Build a human-readable group display name.
     */
    public static String buildGroupDisplayName(String provider, String subject,
            String groupChannel, String space, String id, String key) {
        String providerKey = (provider != null && !provider.trim().isEmpty())
                ? provider.trim().toLowerCase()
                : "group";

        String gc = groupChannel != null ? groupChannel.trim() : null;
        String sp = space != null ? space.trim() : null;
        String sub = subject != null ? subject.trim() : null;

        String detail;
        if (gc != null && !gc.isEmpty() && sp != null && !sp.isEmpty()) {
            detail = sp + (gc.startsWith("#") ? "" : "#") + gc;
        } else if (gc != null && !gc.isEmpty()) {
            detail = gc;
        } else if (sub != null && !sub.isEmpty()) {
            detail = sub;
        } else if (sp != null && !sp.isEmpty()) {
            detail = sp;
        } else {
            detail = "";
        }

        String fallbackId = (id != null && !id.trim().isEmpty()) ? id.trim() : key;
        String rawLabel = !detail.isEmpty() ? detail : fallbackId;

        String token = normalizeGroupLabel(rawLabel);
        if (token.isEmpty()) {
            token = normalizeGroupLabel(shortenGroupId(rawLabel));
        }
        if (groupChannel == null && token.startsWith("#")) {
            token = token.replaceFirst("^#+", "");
        }
        if (!token.isEmpty() && !token.startsWith("@") && !token.startsWith("#")
                && !token.startsWith("g-") && !token.contains("#")) {
            token = "g-" + token;
        }
        return !token.isEmpty() ? providerKey + ":" + token : providerKey;
    }

    /**
     * Resolve the group session key from message context fields.
     */
    public static GroupKeyResolution resolveGroupSessionKey(
            String from, String chatType, String providerHint) {
        String trimmedFrom = (from != null ? from.trim() : "");
        String normalizedChatType = null;
        if (chatType != null) {
            String lc = chatType.trim().toLowerCase();
            if ("channel".equals(lc)) {
                normalizedChatType = "channel";
            } else if ("group".equals(lc)) {
                normalizedChatType = "group";
            }
        }

        boolean isWhatsAppGroupId = trimmedFrom.toLowerCase().endsWith("@g.us");
        boolean looksLikeGroup = "group".equals(normalizedChatType)
                || "channel".equals(normalizedChatType)
                || trimmedFrom.contains(":group:")
                || trimmedFrom.contains(":channel:")
                || isWhatsAppGroupId;

        if (!looksLikeGroup) {
            return null;
        }

        List<String> parts = List.of(trimmedFrom.split(":")).stream()
                .filter(s -> !s.isEmpty()).toList();
        String head = !parts.isEmpty() ? parts.get(0).trim().toLowerCase() : "";
        boolean headIsSurface = !head.isEmpty() && GROUP_SURFACES.contains(head);

        String provider = headIsSurface
                ? head
                : (providerHint != null && !providerHint.trim().isEmpty()
                        ? providerHint.trim().toLowerCase()
                        : (isWhatsAppGroupId ? "whatsapp" : null));
        if (provider == null) {
            return null;
        }

        String second = parts.size() > 1 ? parts.get(1).trim().toLowerCase() : null;
        boolean secondIsKind = "group".equals(second) || "channel".equals(second);
        String kind = secondIsKind ? second
                : (trimmedFrom.contains(":channel:") || "channel".equals(normalizedChatType))
                        ? "channel"
                        : "group";

        String id;
        if (headIsSurface) {
            if (secondIsKind) {
                id = String.join(":", parts.subList(2, parts.size()));
            } else {
                id = String.join(":", parts.subList(1, parts.size()));
            }
        } else {
            id = trimmedFrom;
        }
        String finalId = id.trim().toLowerCase();
        if (finalId.isEmpty()) {
            return null;
        }

        return new GroupKeyResolution(
                provider + ":" + kind + ":" + finalId,
                provider,
                finalId,
                "channel".equals(kind) ? "channel" : "group");
    }
}
