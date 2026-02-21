package com.openclaw.autoreply;

import java.util.regex.Pattern;

/**
 * Heartbeat prompt construction, HEARTBEAT_OK token stripping,
 * and heartbeat content emptiness detection.
 * Mirrors {@code auto-reply/heartbeat.ts}.
 */
public final class Heartbeat {

    private Heartbeat() {
    }

    public static final String HEARTBEAT_PROMPT = "Read HEARTBEAT.md if it exists (workspace context). Follow it strictly. "
            + "Do not infer or repeat old tasks from prior chats. "
            + "If nothing needs attention, reply HEARTBEAT_OK.";

    public static final String DEFAULT_HEARTBEAT_EVERY = "30m";
    public static final int DEFAULT_HEARTBEAT_ACK_MAX_CHARS = 300;

    private static final Pattern HEADERS_ONLY = Pattern.compile("^#+(\\s|$)");
    private static final Pattern EMPTY_LIST_ITEM = Pattern.compile("^[-*+]\\s*(\\[[ Xx]?]\\s*)?$");
    private static final Pattern HTML_TAG = Pattern.compile("<[^>]*>");
    private static final Pattern MARKUP_START = Pattern.compile("^[*`~_]+");
    private static final Pattern MARKUP_END = Pattern.compile("[*`~_]+$");

    /**
     * Check if HEARTBEAT.md content is effectively empty (no actionable tasks).
     */
    public static boolean isHeartbeatContentEffectivelyEmpty(String content) {
        if (content == null)
            return false;
        for (String line : content.split("\n")) {
            String trimmed = line.trim();
            if (trimmed.isEmpty())
                continue;
            if (HEADERS_ONLY.matcher(trimmed).find())
                continue;
            if (EMPTY_LIST_ITEM.matcher(trimmed).matches())
                continue;
            return false;
        }
        return true;
    }

    public static String resolveHeartbeatPrompt(String raw) {
        String trimmed = raw != null ? raw.trim() : "";
        return trimmed.isEmpty() ? HEARTBEAT_PROMPT : trimmed;
    }

    /** Result of stripping the HEARTBEAT_OK token. */
    public record StripResult(boolean shouldSkip, String text, boolean didStrip) {
    }

    /**
     * Strip HEARTBEAT_OK token from reply text, respecting mode and ack char
     * limits.
     */
    public static StripResult stripHeartbeatToken(String raw, String mode, Integer maxAckCharsRaw) {
        if (raw == null || raw.isBlank()) {
            return new StripResult(true, "", false);
        }
        String trimmed = raw.trim();
        String heartbeatToken = ReplyTokens.HEARTBEAT_TOKEN;
        int maxAckChars = maxAckCharsRaw != null && maxAckCharsRaw >= 0
                ? maxAckCharsRaw
                : DEFAULT_HEARTBEAT_ACK_MAX_CHARS;
        String resolvedMode = mode != null ? mode : "message";

        // Normalize lightweight markup
        String trimmedNormalized = stripMarkup(trimmed);
        boolean hasToken = trimmed.contains(heartbeatToken) || trimmedNormalized.contains(heartbeatToken);
        if (!hasToken) {
            return new StripResult(false, trimmed, false);
        }

        String[] strippedOriginal = stripTokenAtEdges(trimmed, heartbeatToken);
        String[] strippedNormalized = stripTokenAtEdges(trimmedNormalized, heartbeatToken);
        String pickedText;
        boolean pickedDidStrip;
        if ("true".equals(strippedOriginal[1]) && !strippedOriginal[0].isEmpty()) {
            pickedText = strippedOriginal[0];
            pickedDidStrip = true;
        } else {
            pickedText = strippedNormalized[0];
            pickedDidStrip = "true".equals(strippedNormalized[1]);
        }

        if (!pickedDidStrip) {
            return new StripResult(false, trimmed, false);
        }
        if (pickedText.isEmpty()) {
            return new StripResult(true, "", true);
        }

        String rest = pickedText.trim();
        if ("heartbeat".equals(resolvedMode) && rest.length() <= maxAckChars) {
            return new StripResult(true, "", true);
        }
        return new StripResult(false, rest, true);
    }

    private static String stripMarkup(String text) {
        String out = HTML_TAG.matcher(text).replaceAll(" ");
        out = out.replace("&nbsp;", " ");
        out = MARKUP_START.matcher(out).replaceFirst("");
        out = MARKUP_END.matcher(out).replaceFirst("");
        return out;
    }

    /**
     * Strip HEARTBEAT_OK at start/end edges, return [text, didStrip].
     */
    private static String[] stripTokenAtEdges(String raw, String token) {
        String text = raw.trim();
        if (text.isEmpty())
            return new String[] { "", "false" };
        if (!text.contains(token))
            return new String[] { text, "false" };

        boolean didStrip = false;
        boolean changed = true;
        while (changed) {
            changed = false;
            String next = text.trim();
            if (next.startsWith(token)) {
                text = next.substring(token.length()).stripLeading();
                didStrip = true;
                changed = true;
                continue;
            }
            if (next.endsWith(token)) {
                text = next.substring(0, Math.max(0, next.length() - token.length())).stripTrailing();
                didStrip = true;
                changed = true;
            }
        }
        String collapsed = text.replaceAll("\\s+", " ").trim();
        return new String[] { collapsed, didStrip ? "true" : "false" };
    }
}
