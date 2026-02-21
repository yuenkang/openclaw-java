package com.openclaw.memory;

import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * Interface for memory search operations.
 * Corresponds to TypeScript's MemorySearchManager in memory/types.ts.
 */
public interface MemorySearchManager {

    /**
     * Search memory for chunks matching a query.
     */
    CompletableFuture<List<MemoryTypes.MemorySearchResult>> search(
            String query, MemoryTypes.SearchOptions opts);

    /**
     * Read a specific file from the memory index.
     */
    CompletableFuture<MemoryTypes.ReadFileResult> readFile(MemoryTypes.ReadFileParams params);

    /**
     * Get the current status of the memory provider.
     */
    MemoryTypes.MemoryProviderStatus status();

    /**
     * Synchronize / rebuild the index.
     */
    CompletableFuture<Void> sync(MemoryTypes.SyncParams params);

    /**
     * Probe whether embedding APIs are available.
     */
    CompletableFuture<MemoryTypes.MemoryEmbeddingProbeResult> probeEmbeddingAvailability();

    /**
     * Probe whether vector search is available.
     */
    CompletableFuture<Boolean> probeVectorAvailability();

    /**
     * Close the manager, releasing resources.
     */
    void close();
}
