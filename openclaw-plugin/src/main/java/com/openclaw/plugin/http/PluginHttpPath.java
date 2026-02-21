package com.openclaw.plugin.http;

/**
 * Plugin HTTP path normalization utilities.
 * Corresponds to TypeScript's plugins/http-path.ts.
 */
public final class PluginHttpPath {

    private PluginHttpPath() {
    }

    /**
     * Normalize a plugin HTTP path, ensuring it starts with '/'.
     */
    public static String normalize(String path, String fallback) {
        String trimmed = path != null ? path.trim() : "";
        if (trimmed.isEmpty()) {
            String fallbackTrimmed = fallback != null ? fallback.trim() : "";
            if (fallbackTrimmed.isEmpty())
                return null;
            return fallbackTrimmed.startsWith("/") ? fallbackTrimmed : "/" + fallbackTrimmed;
        }
        return trimmed.startsWith("/") ? trimmed : "/" + trimmed;
    }
}
