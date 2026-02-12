package com.openclaw.agent.runtime;

import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * AWS Bedrock model discovery with caching.
 * Corresponds to TypeScript agents/bedrock-discovery.ts.
 *
 * <p>
 * Discovers available foundation models from the AWS Bedrock API,
 * filtering by provider, streaming support, and text modalities.
 * </p>
 */
@Slf4j
public final class BedrockDiscovery {

    private static final int DEFAULT_REFRESH_INTERVAL_SECONDS = 3600;
    private static final int DEFAULT_CONTEXT_WINDOW = 32_000;
    private static final int DEFAULT_MAX_TOKENS = 4_096;

    private static final Map<String, CacheEntry> discoveryCache = new ConcurrentHashMap<>();
    private static volatile boolean hasLoggedError = false;

    private BedrockDiscovery() {
    }

    /**
     * Model definition discovered from Bedrock.
     */
    public record ModelDefinition(
            String id, String name, boolean reasoning,
            List<String> input, int contextWindow, int maxTokens) {
    }

    /**
     * Bedrock discovery configuration.
     */
    public record DiscoveryConfig(
            String region,
            List<String> providerFilter,
            int refreshIntervalSeconds,
            int defaultContextWindow,
            int defaultMaxTokens) {
        public static DiscoveryConfig withDefaults(String region) {
            return new DiscoveryConfig(region, List.of(),
                    DEFAULT_REFRESH_INTERVAL_SECONDS, DEFAULT_CONTEXT_WINDOW, DEFAULT_MAX_TOKENS);
        }
    }

    private record CacheEntry(long expiresAt, List<ModelDefinition> value) {
    }

    /**
     * Discover models from Bedrock (with caching).
     *
     * @param config Discovery configuration
     * @return List of discovered model definitions
     */
    public static CompletableFuture<List<ModelDefinition>> discoverModels(DiscoveryConfig config) {
        return CompletableFuture.supplyAsync(() -> {
            String cacheKey = buildCacheKey(config);
            long now = System.currentTimeMillis();

            // Check cache
            CacheEntry cached = discoveryCache.get(cacheKey);
            if (cached != null && cached.value != null && cached.expiresAt > now) {
                return cached.value;
            }

            // TODO: integrate with AWS SDK BedrockClient.listFoundationModels
            // For now, return empty list with logging
            log.debug("discoverBedrockModels: stub for region={}", config.region);

            List<ModelDefinition> models = List.of();

            // Cache the result
            if (config.refreshIntervalSeconds > 0) {
                discoveryCache.put(cacheKey, new CacheEntry(
                        now + config.refreshIntervalSeconds * 1000L, models));
            }

            return models;
        });
    }

    /**
     * Infer if a model supports reasoning from its ID/name.
     */
    static boolean inferReasoningSupport(String modelId, String modelName) {
        String haystack = ((modelId != null ? modelId : "") + " " +
                (modelName != null ? modelName : "")).toLowerCase();
        return haystack.contains("reasoning") || haystack.contains("thinking");
    }

    /**
     * Normalize provider filter list.
     */
    static List<String> normalizeProviderFilter(List<String> filter) {
        if (filter == null || filter.isEmpty())
            return List.of();
        return filter.stream()
                .map(s -> s.trim().toLowerCase())
                .filter(s -> !s.isEmpty())
                .distinct()
                .sorted()
                .toList();
    }

    private static String buildCacheKey(DiscoveryConfig config) {
        return config.region + ":" +
                normalizeProviderFilter(config.providerFilter) + ":" +
                config.refreshIntervalSeconds + ":" +
                config.defaultContextWindow + ":" +
                config.defaultMaxTokens;
    }

    /**
     * Reset cache (for testing).
     */
    public static void resetCache() {
        discoveryCache.clear();
        hasLoggedError = false;
    }
}
