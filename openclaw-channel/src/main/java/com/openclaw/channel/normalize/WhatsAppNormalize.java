package com.openclaw.channel.normalize;

import java.util.regex.Pattern;

/**
 * WhatsApp messaging target detection.
 * Corresponds to TypeScript's channels/plugins/normalize/whatsapp.ts.
 * Note: normalizeWhatsAppMessagingTarget depends on external
 * normalizeWhatsAppTarget;
 * only the self-contained looksLikeWhatsAppTargetId is fully implemented here.
 */
public final class WhatsAppNormalize {

    private WhatsAppNormalize() {
    }

    private static final Pattern WA_PREFIX = Pattern.compile(
            "^whatsapp:", Pattern.CASE_INSENSITIVE);
    private static final Pattern PHONE_LIKE = Pattern.compile("^\\+?\\d{3,}$");

    /**
     * Check if a raw string looks like a WhatsApp target ID.
     */
    public static boolean looksLikeTargetId(String raw) {
        if (raw == null)
            return false;
        String trimmed = raw.trim();
        if (trimmed.isEmpty())
            return false;

        if (WA_PREFIX.matcher(trimmed).find())
            return true;
        if (trimmed.contains("@"))
            return true;
        return PHONE_LIKE.matcher(trimmed).matches();
    }
}
