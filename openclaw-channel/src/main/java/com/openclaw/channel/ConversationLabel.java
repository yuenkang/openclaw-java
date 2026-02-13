package com.openclaw.channel;

import java.util.Map;
import java.util.regex.Pattern;

/**
 * Conversation label resolution from message context.
 * Corresponds to TypeScript's channels/conversation-label.ts.
 */
public final class ConversationLabel {

    private ConversationLabel() {
    }

    private static final Pattern NUMERIC_ID = Pattern.compile("^[0-9]+$");

    /**
     * Resolve a human-readable conversation label from message context fields.
     *
     * @param ctx message context map (ConversationLabel, ThreadLabel, ChatType,
     *            SenderName, From, GroupChannel, GroupSubject, GroupSpace)
     * @return resolved label or null
     */
    public static String resolve(Map<String, String> ctx) {
        String explicit = trimOrNull(ctx.get("ConversationLabel"));
        if (explicit != null) {
            return explicit;
        }

        String threadLabel = trimOrNull(ctx.get("ThreadLabel"));
        if (threadLabel != null) {
            return threadLabel;
        }

        String chatType = ChatType.normalize(ctx.get("ChatType"));
        if ("direct".equals(chatType)) {
            String name = trimOrNull(ctx.get("SenderName"));
            return name != null ? name : trimOrNull(ctx.get("From"));
        }

        String base = firstNonEmpty(
                trimOrNull(ctx.get("GroupChannel")),
                trimOrNull(ctx.get("GroupSubject")),
                trimOrNull(ctx.get("GroupSpace")),
                trimOrNull(ctx.get("From")));
        if (base == null) {
            return null;
        }

        String id = extractConversationId(ctx.get("From"));
        if (id == null) {
            return base;
        }
        if (!shouldAppendId(id)) {
            return base;
        }
        if (base.equals(id) || base.contains(id)) {
            return base;
        }
        if (base.toLowerCase().contains(" id:")) {
            return base;
        }
        if (base.startsWith("#") || base.startsWith("@")) {
            return base;
        }
        return base + " id:" + id;
    }

    // =========================================================================
    // Internal
    // =========================================================================

    private static String extractConversationId(String from) {
        String trimmed = trimOrNull(from);
        if (trimmed == null) {
            return null;
        }
        String[] parts = trimmed.split(":");
        for (int i = parts.length - 1; i >= 0; i--) {
            if (!parts[i].isEmpty()) {
                return parts[i];
            }
        }
        return trimmed;
    }

    private static boolean shouldAppendId(String id) {
        if (NUMERIC_ID.matcher(id).matches()) {
            return true;
        }
        return id.contains("@g.us");
    }

    private static String trimOrNull(String value) {
        if (value == null)
            return null;
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private static String firstNonEmpty(String... values) {
        for (String v : values) {
            if (v != null)
                return v;
        }
        return null;
    }
}
