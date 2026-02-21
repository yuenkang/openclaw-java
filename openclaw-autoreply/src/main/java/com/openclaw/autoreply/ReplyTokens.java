package com.openclaw.autoreply;

import java.util.regex.Pattern;

/**
 * Token constants and silent-reply detection for auto-reply.
 * Mirrors {@code auto-reply/tokens.ts}.
 */
public final class ReplyTokens {

    private ReplyTokens() {
    }

    public static final String HEARTBEAT_TOKEN = "HEARTBEAT_OK";
    public static final String SILENT_REPLY_TOKEN = "NO_REPLY";

    /**
     * Check if a reply text is a silent reply (starts or ends with the NO_REPLY
     * token).
     */
    public static boolean isSilentReplyText(String text) {
        return isSilentReplyText(text, SILENT_REPLY_TOKEN);
    }

    public static boolean isSilentReplyText(String text, String token) {
        if (text == null || text.isEmpty())
            return false;
        String escaped = Pattern.quote(token);
        // Prefix: token at start followed by end or non-word char
        if (Pattern.compile("^\\s*" + escaped + "(?=$|\\W)").matcher(text).find()) {
            return true;
        }
        // Suffix: token at end preceded by word boundary
        return Pattern.compile("\\b" + escaped + "\\b\\W*$").matcher(text).find();
    }
}
