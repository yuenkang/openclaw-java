package com.openclaw.channel;

/**
 * Status issue shared utilities.
 * Corresponds to TypeScript's channels/plugins/status-issues/shared.ts.
 */
public final class StatusIssueUtils {

    private StatusIssueUtils() {
    }

    /**
     * Trim and return a string, or null if blank.
     */
    public static String asString(Object value) {
        if (value instanceof String s) {
            String trimmed = s.trim();
            return trimmed.isEmpty() ? null : trimmed;
        }
        return null;
    }

    /**
     * Format match metadata for diagnostic messages.
     */
    public static String formatMatchMetadata(Object matchKey, Object matchSource) {
        String key = null;
        if (matchKey instanceof String) {
            key = ((String) matchKey).trim();
            if (key.isEmpty())
                key = null;
        } else if (matchKey instanceof Number) {
            key = matchKey.toString();
        }

        String source = asString(matchSource);

        StringBuilder sb = new StringBuilder();
        if (key != null) {
            sb.append("matchKey=").append(key);
        }
        if (source != null) {
            if (!sb.isEmpty())
                sb.append(" ");
            sb.append("matchSource=").append(source);
        }
        return sb.isEmpty() ? null : sb.toString();
    }

    /**
     * Append match metadata to a message.
     */
    public static String appendMatchMetadata(String message, Object matchKey, Object matchSource) {
        String meta = formatMatchMetadata(matchKey, matchSource);
        return meta != null ? message + " (" + meta + ")" : message;
    }
}
