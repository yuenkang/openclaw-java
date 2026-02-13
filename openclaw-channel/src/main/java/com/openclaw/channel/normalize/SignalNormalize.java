package com.openclaw.channel.normalize;

import java.util.regex.Pattern;

/**
 * Signal messaging target normalization.
 * Corresponds to TypeScript's channels/plugins/normalize/signal.ts.
 */
public final class SignalNormalize {

    private SignalNormalize() {
    }

    private static final Pattern UUID_PATTERN = Pattern.compile(
            "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern UUID_COMPACT = Pattern.compile(
            "^[0-9a-f]{32}$", Pattern.CASE_INSENSITIVE);
    private static final Pattern SIGNAL_PREFIX = Pattern.compile(
            "^signal:", Pattern.CASE_INSENSITIVE);
    private static final Pattern PHONE_LIKE = Pattern.compile("^\\+?\\d{3,}$");

    /**
     * Normalize a raw Signal messaging target.
     *
     * @return normalized target like "group:id", "username:name", or raw UUID
     */
    public static String normalizeMessagingTarget(String raw) {
        if (raw == null)
            return null;
        String trimmed = raw.trim();
        if (trimmed.isEmpty())
            return null;

        String normalized = trimmed;
        if (normalized.toLowerCase().startsWith("signal:")) {
            normalized = normalized.substring("signal:".length()).trim();
        }
        if (normalized.isEmpty())
            return null;

        String lower = normalized.toLowerCase();
        if (lower.startsWith("group:")) {
            String id = normalized.substring("group:".length()).trim();
            return id.isEmpty() ? null : ("group:" + id).toLowerCase();
        }
        if (lower.startsWith("username:")) {
            String id = normalized.substring("username:".length()).trim();
            return id.isEmpty() ? null : ("username:" + id).toLowerCase();
        }
        if (lower.startsWith("u:")) {
            String id = normalized.substring("u:".length()).trim();
            return id.isEmpty() ? null : ("username:" + id).toLowerCase();
        }
        if (lower.startsWith("uuid:")) {
            String id = normalized.substring("uuid:".length()).trim();
            return id.isEmpty() ? null : id.toLowerCase();
        }
        return normalized.toLowerCase();
    }

    /**
     * Check if a raw string looks like a Signal target ID.
     */
    public static boolean looksLikeTargetId(String raw) {
        if (raw == null)
            return false;
        String trimmed = raw.trim();
        if (trimmed.isEmpty())
            return false;

        if (Pattern.compile("^(signal:)?(group:|username:|u:)", Pattern.CASE_INSENSITIVE)
                .matcher(trimmed).find()) {
            return true;
        }
        if (Pattern.compile("^(signal:)?uuid:", Pattern.CASE_INSENSITIVE).matcher(trimmed).find()) {
            String stripped = SIGNAL_PREFIX.matcher(trimmed).replaceFirst("");
            stripped = stripped.replaceFirst("(?i)^uuid:", "").trim();
            if (stripped.isEmpty())
                return false;
            return UUID_PATTERN.matcher(stripped).matches() || UUID_COMPACT.matcher(stripped).matches();
        }
        if (UUID_PATTERN.matcher(trimmed).matches() || UUID_COMPACT.matcher(trimmed).matches()) {
            return true;
        }
        return PHONE_LIKE.matcher(trimmed).matches();
    }
}
