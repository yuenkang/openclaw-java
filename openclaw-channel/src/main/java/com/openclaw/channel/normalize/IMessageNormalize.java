package com.openclaw.channel.normalize;

import java.util.regex.Pattern;

/**
 * iMessage messaging target detection.
 * Corresponds to TypeScript's channels/plugins/normalize/imessage.ts.
 * Note: normalizeIMessageMessagingTarget depends on external
 * normalizeIMessageHandle;
 * only the self-contained looksLikeIMessageTargetId is fully implemented here.
 */
public final class IMessageNormalize {

    private IMessageNormalize() {
    }

    private static final Pattern SERVICE_PREFIX = Pattern.compile(
            "^(imessage:|sms:|auto:)", Pattern.CASE_INSENSITIVE);
    private static final Pattern CHAT_TARGET_PREFIX = Pattern.compile(
            "^(chat_id:|chatid:|chat:|chat_guid:|chatguid:|guid:|chat_identifier:|chatidentifier:|chatident:)",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern PHONE_LIKE = Pattern.compile("^\\+?\\d{3,}$");

    /**
     * Check if a raw string looks like an iMessage target ID.
     */
    public static boolean looksLikeTargetId(String raw) {
        if (raw == null)
            return false;
        String trimmed = raw.trim();
        if (trimmed.isEmpty())
            return false;

        if (SERVICE_PREFIX.matcher(trimmed).find())
            return true;
        if (CHAT_TARGET_PREFIX.matcher(trimmed).find())
            return true;
        if (trimmed.contains("@"))
            return true;
        return PHONE_LIKE.matcher(trimmed).matches();
    }
}
