package com.openclaw.memory;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * Memory type definitions.
 * Corresponds to TypeScript's memory/types.ts.
 */
public final class MemoryTypes {

    private MemoryTypes() {
    }

    public enum MemorySource {
        MEMORY, SESSIONS
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MemorySearchResult {
        private String path;
        private int startLine;
        private int endLine;
        private double score;
        private String snippet;
        private MemorySource source;
        private String citation;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MemoryEmbeddingProbeResult {
        private boolean ok;
        private String error;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MemorySyncProgressUpdate {
        private int completed;
        private int total;
        private String label;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MemoryProviderStatus {
        private String backend; // "builtin" or "qmd"
        private String provider;
        private String model;
        private String requestedProvider;
        private Integer files;
        private Integer chunks;
        private Boolean dirty;
        private String workspaceDir;
        private String dbPath;
        private List<String> extraPaths;
        private List<MemorySource> sources;
        private List<SourceCount> sourceCounts;
        private CacheStatus cache;
        private FtsStatus fts;
        private FallbackStatus fallback;
        private VectorStatus vector;
        private BatchStatus batch;
        private Map<String, Object> custom;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SourceCount {
        private MemorySource source;
        private int files;
        private int chunks;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CacheStatus {
        private boolean enabled;
        private Integer entries;
        private Integer maxEntries;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class FtsStatus {
        private boolean enabled;
        private boolean available;
        private String error;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class FallbackStatus {
        private String from;
        private String reason;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class VectorStatus {
        private boolean enabled;
        private Boolean available;
        private String extensionPath;
        private String loadError;
        private Integer dims;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class BatchStatus {
        private boolean enabled;
        private int failures;
        private int limit;
        private boolean wait;
        private int concurrency;
        private long pollIntervalMs;
        private long timeoutMs;
        private String lastError;
        private String lastProvider;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SearchOptions {
        private Integer maxResults;
        private Double minScore;
        private String sessionKey;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ReadFileParams {
        private String relPath;
        private Integer from;
        private Integer lines;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ReadFileResult {
        private String text;
        private String path;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SyncParams {
        private String reason;
        private Boolean force;
    }
}
