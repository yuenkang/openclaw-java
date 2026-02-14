package com.openclaw.gateway.outbound;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Resolves messaging targets using channel directory lookups and caching.
 * Corresponds to TypeScript's target-resolver.ts.
 */
@Slf4j
public class OutboundTargetResolver {

    /** Cache TTL: 30 minutes. */
    private static final long CACHE_TTL_MS = 30 * 60 * 1000L;

    /** Directory cache: channel:accountId:kind → (entries, expiry). */
    private final Map<String, CachedEntries> directoryCache = new ConcurrentHashMap<>();

    public enum TargetResolveKind {
        USER, GROUP, CHANNEL
    }

    /**
     * Resolved messaging target.
     */
    public record ResolvedMessagingTarget(
            String to,
            TargetResolveKind kind,
            String display,
            String source // "normalized" or "directory"
    ) {
    }

    /**
     * Result of resolving a messaging target.
     */
    public record ResolveResult(
            boolean ok,
            ResolvedMessagingTarget target,
            Exception error) {
        public static ResolveResult success(ResolvedMessagingTarget target) {
            return new ResolveResult(true, target, null);
        }

        public static ResolveResult failure(Exception error) {
            return new ResolveResult(false, null, error);
        }

        public static ResolveResult failure(String message) {
            return new ResolveResult(false, null, new RuntimeException(message));
        }
    }

    /**
     * Resolve a channel target from raw input.
     * Tries direct normalization first, then directory lookup.
     */
    public ResolveResult resolveChannelTarget(
            OpenClawConfig cfg,
            String channel,
            String input,
            String accountId) {

        if (input == null || input.isBlank()) {
            return ResolveResult.failure("Target is required");
        }

        String trimmed = input.trim();

        // Try direct ID normalization (numeric, prefixed, etc.)
        if (looksLikeTargetId(channel, trimmed)) {
            String normalized = normalizeTargetForProvider(channel, trimmed);
            return ResolveResult.success(new ResolvedMessagingTarget(
                    normalized,
                    detectTargetKind(channel, trimmed, null),
                    null,
                    "normalized"));
        }

        // For now, assume raw input is usable directly
        return ResolveResult.success(new ResolvedMessagingTarget(
                trimmed,
                TargetResolveKind.USER,
                null,
                "normalized"));
    }

    /**
     * Detect whether input looks like a direct target ID (numeric, @-prefixed,
     * etc.).
     */
    private boolean looksLikeTargetId(String channel, String raw) {
        if (raw == null || raw.isBlank())
            return false;

        return switch (channel != null ? channel.toLowerCase() : "") {
            case "telegram" -> raw.matches("^-?\\d+$") || raw.startsWith("@");
            case "discord" -> raw.matches("^\\d{17,19}$");
            case "whatsapp" -> raw.matches("^\\+?\\d[\\d\\s-]+@?.*$");
            case "slack" -> raw.startsWith("#") || raw.startsWith("@") || raw.matches("^[CDGU]\\w+$");
            default -> true; // Default: assume it's usable
        };
    }

    /**
     * Normalize a target ID for the specific provider.
     */
    private String normalizeTargetForProvider(String channel, String raw) {
        if (channel == null)
            return raw;
        return switch (channel.toLowerCase()) {
            case "telegram" -> raw.startsWith("@") ? raw : raw;
            case "whatsapp" -> raw.replaceAll("[\\s-]", "");
            default -> raw;
        };
    }

    /**
     * Detect the kind of target based on channel and raw input.
     */
    private TargetResolveKind detectTargetKind(String channel, String raw,
            TargetResolveKind preferred) {
        if (preferred != null)
            return preferred;

        if (channel == null)
            return TargetResolveKind.USER;
        return switch (channel.toLowerCase()) {
            case "telegram" -> {
                if (raw.startsWith("-100"))
                    yield TargetResolveKind.CHANNEL;
                if (raw.startsWith("-"))
                    yield TargetResolveKind.GROUP;
                yield TargetResolveKind.USER;
            }
            case "discord" -> TargetResolveKind.CHANNEL; // Discord targets are usually channels
            case "slack" -> raw.startsWith("#") || raw.startsWith("C")
                    ? TargetResolveKind.CHANNEL
                    : TargetResolveKind.USER;
            default -> TargetResolveKind.USER;
        };
    }

    /**
     * Format a target for display purposes.
     */
    public static String formatTargetDisplay(String channel, String target, String display) {
        if (display != null && !display.isBlank()) {
            return display + " (" + target + ")";
        }
        return target;
    }

    /**
     * Reset the directory cache, optionally for a specific channel/account.
     */
    public void resetCache(String channel, String accountId) {
        if (channel == null) {
            directoryCache.clear();
            return;
        }
        String prefix = buildCacheKey(channel, accountId != null ? accountId : "", "");
        directoryCache.keySet().removeIf(k -> k.startsWith(prefix));
    }

    private String buildCacheKey(String channel, String accountId, String kind) {
        return channel + ":" + (accountId != null ? accountId : "") + ":" + kind;
    }

    /** Cached directory entries with expiry. */
    private record CachedEntries(List<Map<String, Object>> entries, long expiresAt) {
        boolean isExpired() {
            return System.currentTimeMillis() > expiresAt;
        }
    }
}
