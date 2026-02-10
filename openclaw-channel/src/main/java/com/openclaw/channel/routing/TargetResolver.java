package com.openclaw.channel.routing;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.time.Duration;
import java.util.Optional;

/**
 * Resolves messaging targets (users, groups) for channels.
 * Implements directory lookup caching (30-min TTL).
 * Corresponds to TypeScript's infra/outbound/target-resolver.ts.
 */
@Slf4j
public class TargetResolver {

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ResolvedTarget {
        private String targetId;
        private String channelId;
        private String displayName;
        private String targetType; // "user" | "group" | "channel"
    }

    @FunctionalInterface
    public interface DirectoryLookup {
        Optional<ResolvedTarget> lookup(String channelId, String query);
    }

    private final Cache<String, ResolvedTarget> cache;
    private DirectoryLookup directoryLookup;

    public TargetResolver() {
        this.cache = Caffeine.newBuilder()
                .expireAfterWrite(Duration.ofMinutes(30))
                .maximumSize(1000)
                .build();
    }

    public void setDirectoryLookup(DirectoryLookup lookup) {
        this.directoryLookup = lookup;
    }

    /**
     * Resolve a target, using cache first then directory lookup.
     */
    public Optional<ResolvedTarget> resolve(String channelId, String targetQuery) {
        String cacheKey = channelId + ":" + targetQuery;

        // Check cache
        ResolvedTarget cached = cache.getIfPresent(cacheKey);
        if (cached != null) {
            return Optional.of(cached);
        }

        // Direct ID (starts with @, #, or is numeric)
        if (targetQuery.startsWith("@") || targetQuery.startsWith("#") || targetQuery.matches("\\d+")) {
            ResolvedTarget direct = ResolvedTarget.builder()
                    .targetId(targetQuery.replaceFirst("^[@#]", ""))
                    .channelId(channelId)
                    .targetType(targetQuery.startsWith("#") ? "group" : "user")
                    .build();
            cache.put(cacheKey, direct);
            return Optional.of(direct);
        }

        // Directory lookup
        if (directoryLookup != null) {
            Optional<ResolvedTarget> result = directoryLookup.lookup(channelId, targetQuery);
            result.ifPresent(t -> cache.put(cacheKey, t));
            return result;
        }

        return Optional.empty();
    }

    /**
     * Invalidate cache for a specific target.
     */
    public void invalidate(String channelId, String targetQuery) {
        cache.invalidate(channelId + ":" + targetQuery);
    }

    public void invalidateAll() {
        cache.invalidateAll();
    }
}
