package com.openclaw.common.config;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Cache utility functions for config TTL and file modification checks.
 * Corresponds to TypeScript's cache-utils.ts.
 */
public final class ConfigCacheUtils {

    private ConfigCacheUtils() {
    }

    /**
     * Resolve cache TTL from an environment variable string or fall back to
     * default.
     *
     * @param envValue     raw environment variable value (e.g. "5000"), nullable
     * @param defaultTtlMs default TTL in milliseconds
     * @return resolved TTL in milliseconds (always >= 0)
     */
    public static long resolveCacheTtlMs(String envValue, long defaultTtlMs) {
        if (envValue != null && !envValue.isEmpty()) {
            try {
                long parsed = Long.parseLong(envValue);
                if (parsed >= 0)
                    return parsed;
            } catch (NumberFormatException ignored) {
            }
        }
        return defaultTtlMs;
    }

    /**
     * Check if caching is enabled (TTL > 0).
     */
    public static boolean isCacheEnabled(long ttlMs) {
        return ttlMs > 0;
    }

    /**
     * Get the last modified time of a file in milliseconds, or null if unavailable.
     */
    public static Long getFileMtimeMs(String filePath) {
        try {
            return Files.getLastModifiedTime(Path.of(filePath)).toMillis();
        } catch (Exception e) {
            return null;
        }
    }
}
