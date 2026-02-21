package com.openclaw.memory;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Built-in memory index manager using file-system keyword search.
 * Corresponds to TypeScript's memory/manager.ts (MemoryIndexManager).
 *
 * <p>
 * This is a simplified Java implementation that provides:
 * <ul>
 * <li>File-system scanning of memory directories (MEMORY.md, memory/*.md)</li>
 * <li>Keyword-based search with snippet extraction</li>
 * <li>File read with line range support</li>
 * </ul>
 *
 * <p>
 * Vector search and SQLite-based indexing are deferred to a future phase.
 * </p>
 */
@Slf4j
public class MemoryIndexManager implements MemorySearchManager {

    private static final int DEFAULT_MAX_RESULTS = 6;
    private static final int DEFAULT_SNIPPET_LINES = 5;
    private static final int MAX_FILE_SIZE = 512 * 1024; // 512KB per file

    /** Cached instances keyed by agent workspace path. */
    private static final Map<String, MemoryIndexManager> INSTANCES = new ConcurrentHashMap<>();

    private final String workspaceDir;
    private final List<String> extraPaths;
    private final Map<String, List<String>> fileCache = new ConcurrentHashMap<>();
    private volatile long lastSyncMs = 0;

    private MemoryIndexManager(String workspaceDir, List<String> extraPaths) {
        this.workspaceDir = workspaceDir;
        this.extraPaths = extraPaths != null ? extraPaths : List.of();
    }

    /**
     * Get or create a MemoryIndexManager for the given config and agent.
     */
    public static MemoryIndexManager get(OpenClawConfig cfg, String agentId) {
        // Resolve workspace dir from config or fall back to user.home
        String wsDir = resolveWorkspaceDir(cfg, agentId);
        return INSTANCES.computeIfAbsent(wsDir, k -> {
            MemoryIndexManager mgr = new MemoryIndexManager(wsDir, List.of());
            log.info("Created MemoryIndexManager for workspace: {}", wsDir);
            return mgr;
        });
    }

    /**
     * Get or create a MemoryIndexManager with explicit workspace dir.
     */
    public static MemoryIndexManager get(String workspaceDir) {
        return INSTANCES.computeIfAbsent(workspaceDir, k -> {
            MemoryIndexManager mgr = new MemoryIndexManager(workspaceDir, List.of());
            log.info("Created MemoryIndexManager for workspace: {}", workspaceDir);
            return mgr;
        });
    }

    @Override
    public CompletableFuture<List<MemoryTypes.MemorySearchResult>> search(
            String query, MemoryTypes.SearchOptions opts) {
        return CompletableFuture.supplyAsync(() -> {
            if (query == null || query.isBlank())
                return List.of();

            int maxResults = DEFAULT_MAX_RESULTS;
            if (opts != null && opts.getMaxResults() != null && opts.getMaxResults() > 0) {
                maxResults = opts.getMaxResults();
            }

            ensureCachePopulated();
            String[] terms = query.toLowerCase().split("\\s+");
            List<ScoredResult> scored = new ArrayList<>();

            for (Map.Entry<String, List<String>> entry : fileCache.entrySet()) {
                String filePath = entry.getKey();
                List<String> lines = entry.getValue();

                // Scan each line for keyword matches
                for (int i = 0; i < lines.size(); i++) {
                    String line = lines.get(i).toLowerCase();
                    int matchCount = 0;
                    for (String term : terms) {
                        if (line.contains(term))
                            matchCount++;
                    }
                    if (matchCount == 0)
                        continue;

                    double score = (double) matchCount / terms.length;
                    int start = Math.max(0, i - 1);
                    int end = Math.min(lines.size(), i + DEFAULT_SNIPPET_LINES);
                    String snippet = String.join("\n", lines.subList(start, end));

                    scored.add(new ScoredResult(filePath, start + 1, end, score, snippet));
                }
            }

            // Sort by score desc, take top N
            scored.sort((a, b) -> Double.compare(b.score, a.score));
            // Deduplicate overlapping snippets in same file
            List<ScoredResult> deduped = deduplicateOverlapping(scored);

            return deduped.stream()
                    .limit(maxResults)
                    .map(r -> MemoryTypes.MemorySearchResult.builder()
                            .path(r.path)
                            .startLine(r.startLine)
                            .endLine(r.endLine)
                            .score(r.score)
                            .snippet(r.snippet)
                            .source(MemoryTypes.MemorySource.MEMORY)
                            .build())
                    .collect(Collectors.toList());
        });
    }

    @Override
    public CompletableFuture<MemoryTypes.ReadFileResult> readFile(MemoryTypes.ReadFileParams params) {
        return CompletableFuture.supplyAsync(() -> {
            Path resolved = Path.of(workspaceDir).resolve(params.getRelPath()).normalize();
            if (!Files.isRegularFile(resolved)) {
                return MemoryTypes.ReadFileResult.builder()
                        .text("File not found: " + params.getRelPath())
                        .path(resolved.toString())
                        .build();
            }
            try {
                List<String> allLines = Files.readAllLines(resolved);
                int from = params.getFrom() != null ? Math.max(0, params.getFrom() - 1) : 0;
                int lineCount = params.getLines() != null ? params.getLines() : allLines.size();
                int to = Math.min(allLines.size(), from + lineCount);
                String text = String.join("\n", allLines.subList(from, to));
                return MemoryTypes.ReadFileResult.builder()
                        .text(text)
                        .path(resolved.toString())
                        .build();
            } catch (IOException e) {
                return MemoryTypes.ReadFileResult.builder()
                        .text("Error reading file: " + e.getMessage())
                        .path(resolved.toString())
                        .build();
            }
        });
    }

    @Override
    public MemoryTypes.MemoryProviderStatus status() {
        ensureCachePopulated();
        return MemoryTypes.MemoryProviderStatus.builder()
                .backend("builtin")
                .provider("keyword")
                .files(fileCache.size())
                .chunks(fileCache.values().stream().mapToInt(List::size).sum())
                .workspaceDir(workspaceDir)
                .sources(List.of(MemoryTypes.MemorySource.MEMORY))
                .fts(MemoryTypes.FtsStatus.builder()
                        .enabled(true)
                        .available(true)
                        .build())
                .vector(MemoryTypes.VectorStatus.builder()
                        .enabled(false)
                        .available(false)
                        .loadError("Vector search not yet implemented in Java")
                        .build())
                .build();
    }

    @Override
    public CompletableFuture<Void> sync(MemoryTypes.SyncParams params) {
        return CompletableFuture.runAsync(() -> {
            log.info("Syncing memory index for workspace: {}", workspaceDir);
            fileCache.clear();
            scanFiles();
            lastSyncMs = System.currentTimeMillis();
            log.info("Memory index synced: {} files", fileCache.size());
        });
    }

    @Override
    public CompletableFuture<MemoryTypes.MemoryEmbeddingProbeResult> probeEmbeddingAvailability() {
        return CompletableFuture.completedFuture(
                MemoryTypes.MemoryEmbeddingProbeResult.builder()
                        .ok(false)
                        .error("Embedding not yet implemented in Java builtin provider")
                        .build());
    }

    @Override
    public CompletableFuture<Boolean> probeVectorAvailability() {
        return CompletableFuture.completedFuture(false);
    }

    @Override
    public void close() {
        fileCache.clear();
        INSTANCES.remove(workspaceDir);
        log.debug("Closed MemoryIndexManager for: {}", workspaceDir);
    }

    // --- Internal helpers ---

    private void ensureCachePopulated() {
        if (fileCache.isEmpty() || System.currentTimeMillis() - lastSyncMs > 60_000) {
            scanFiles();
            lastSyncMs = System.currentTimeMillis();
        }
    }

    private void scanFiles() {
        Path ws = Path.of(workspaceDir);
        if (!Files.isDirectory(ws)) {
            log.debug("Workspace dir does not exist: {}", workspaceDir);
            return;
        }

        // Scan: MEMORY.md, memory.md, memory/**/*.md
        scanSingleFile(ws.resolve("MEMORY.md"));
        scanSingleFile(ws.resolve("memory.md"));
        Path memoryDir = ws.resolve("memory");
        if (Files.isDirectory(memoryDir)) {
            try (Stream<Path> files = Files.walk(memoryDir, 5)) {
                files.filter(Files::isRegularFile)
                        .filter(p -> p.toString().endsWith(".md"))
                        .forEach(this::scanSingleFile);
            } catch (IOException e) {
                log.debug("Error scanning memory dir: {}", e.getMessage());
            }
        }

        // Scan extra paths
        for (String extra : extraPaths) {
            scanSingleFile(Path.of(extra));
        }
    }

    private void scanSingleFile(Path path) {
        if (!Files.isRegularFile(path))
            return;
        try {
            long size = Files.size(path);
            if (size > MAX_FILE_SIZE) {
                log.debug("Skipping large file: {} ({}KB)", path, size / 1024);
                return;
            }
            List<String> lines = Files.readAllLines(path);
            fileCache.put(path.toAbsolutePath().toString(), lines);
        } catch (IOException e) {
            log.trace("Could not read {}: {}", path, e.getMessage());
        }
    }

    private static List<ScoredResult> deduplicateOverlapping(List<ScoredResult> results) {
        List<ScoredResult> deduped = new ArrayList<>();
        Map<String, List<int[]>> seen = new HashMap<>(); // path -> list of [start, end]

        for (ScoredResult r : results) {
            List<int[]> ranges = seen.computeIfAbsent(r.path, k -> new ArrayList<>());
            boolean overlaps = false;
            for (int[] range : ranges) {
                if (r.startLine <= range[1] && r.endLine >= range[0]) {
                    overlaps = true;
                    break;
                }
            }
            if (!overlaps) {
                deduped.add(r);
                ranges.add(new int[] { r.startLine, r.endLine });
            }
        }
        return deduped;
    }

    private static String resolveWorkspaceDir(OpenClawConfig cfg, String agentId) {
        // Try to resolve from config agents defaults workspace
        if (cfg != null && cfg.getAgents() != null && cfg.getAgents().getDefaults() != null) {
            String ws = cfg.getAgents().getDefaults().getWorkspace();
            if (ws != null && !ws.isBlank()) {
                return ws.startsWith("~") ? ws.replace("~", System.getProperty("user.home")) : ws;
            }
        }
        // Fall back to user home
        return System.getProperty("user.home");
    }

    // --- Internal scoring record ---
    private record ScoredResult(String path, int startLine, int endLine, double score, String snippet) {
    }
}
