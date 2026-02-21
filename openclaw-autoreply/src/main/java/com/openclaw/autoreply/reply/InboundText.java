package com.openclaw.autoreply.reply;

/**
 * Newline normalization for inbound message text.
 * Mirrors {@code auto-reply/reply/inbound-text.ts}.
 */
public final class InboundText {

    private InboundText() {
    }

    /** Normalize \\r\\n, \\r, and literal \\n escapes to \\n. */
    public static String normalizeInboundTextNewlines(String input) {
        if (input == null)
            return "";
        return input.replace("\r\n", "\n")
                .replace("\r", "\n")
                .replace("\\n", "\n");
    }
}
