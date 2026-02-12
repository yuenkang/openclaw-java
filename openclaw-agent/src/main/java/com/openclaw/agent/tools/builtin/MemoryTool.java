package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Stream;

/**
 * Memory tool — keyword search over MEMORY.md and memory/*.md files.
 * Corresponds to TypeScript's tools/memory-tool.ts.
 */
@Slf4j
public class MemoryTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final int DEFAULT_MAX_RESULTS = 5;
    private static final int MAX_SNIPPET_CHARS = 2000;

    @Override
    public String getName() {
        return "memory_search";
    }

    @Override
    public String getDescription() {
        return "Mandatory recall step: semantically search MEMORY.md + memory/*.md " +
                "before answering questions about prior work, decisions, dates, people, " +
                "preferences, or todos; returns top snippets with path + lines.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode properties = schema.putObject("properties");

        ObjectNode query = properties.putObject("query");
        query.put("type", "string");
        query.put("description", "Search query");

        ObjectNode maxResults = properties.putObject("maxResults");
        maxResults.put("type", "number");
        maxResults.put("description", "Maximum results (default: 5)");

        ObjectNode minScore = properties.putObject("minScore");
        minScore.put("type", "number");
        minScore.put("description", "Minimum relevance score (0-1)");

        schema.putArray("required").add("query");
        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> doExecute(context));
    }

    private ToolResult doExecute(ToolContext context) {
        try {
            JsonNode params = context.getParameters();
            String queryStr = ToolParamUtils.readStringParam(params, "query");
            if (queryStr == null || queryStr.isBlank()) {
                return ToolResult.fail("'query' is required");
            }

            Integer maxResults = ToolParamUtils.readIntegerParam(params, "maxResults");
            int limit = maxResults != null ? maxResults : DEFAULT_MAX_RESULTS;

            String cwd = context.getCwd();
            if (cwd == null)
                cwd = System.getProperty("user.dir");
            Path cwdPath = Path.of(cwd);

            List<MemoryHit> results = searchMemoryFiles(cwdPath, queryStr, limit);

            ObjectNode output = MAPPER.createObjectNode();
            var resultsArray = output.putArray("results");
            for (MemoryHit hit : results) {
                ObjectNode node = resultsArray.addObject();
                node.put("path", hit.path);
                node.put("snippet", hit.snippet);
                node.put("startLine", hit.startLine);
                node.put("endLine", hit.endLine);
                node.put("score", hit.score);
            }

            return ToolResult.ok(ToolParamUtils.toJsonString(output), output);

        } catch (Exception e) {
            log.error("memory_search error: {}", e.getMessage(), e);
            return ToolResult.fail("Memory search error: " + e.getMessage());
        }
    }

    private List<MemoryHit> searchMemoryFiles(Path cwd, String query, int limit) {
        List<MemoryHit> results = new ArrayList<>();
        String lowerQuery = query.toLowerCase();

        List<Path> memoryFiles = new ArrayList<>();
        Path memoryMd = cwd.resolve("MEMORY.md");
        if (Files.exists(memoryMd))
            memoryFiles.add(memoryMd);
        Path memoryDir = cwd.resolve("memory");
        if (Files.isDirectory(memoryDir)) {
            try (Stream<Path> files = Files.list(memoryDir)) {
                files.filter(p -> p.toString().endsWith(".md")).forEach(memoryFiles::add);
            } catch (IOException e) {
                log.warn("Failed to list memory dir: {}", e.getMessage());
            }
        }

        for (Path file : memoryFiles) {
            try {
                List<String> lines = Files.readAllLines(file, StandardCharsets.UTF_8);
                String relativePath = cwd.relativize(file).toString();
                for (int i = 0; i < lines.size(); i++) {
                    if (lines.get(i).toLowerCase().contains(lowerQuery)) {
                        int start = Math.max(0, i - 3);
                        int end = Math.min(lines.size(), i + 4);
                        StringBuilder snippet = new StringBuilder();
                        for (int j = start; j < end; j++)
                            snippet.append(lines.get(j)).append("\n");
                        String snip = snippet.toString();
                        if (snip.length() > MAX_SNIPPET_CHARS)
                            snip = snip.substring(0, MAX_SNIPPET_CHARS) + "…";
                        double score = computeScore(lines.get(i), lowerQuery);
                        results.add(new MemoryHit(relativePath, snip, start + 1, end, score));
                        if (results.size() >= limit * 3)
                            break;
                    }
                }
            } catch (IOException e) {
                log.warn("Failed to read {}: {}", file, e.getMessage());
            }
        }

        results.sort((a, b) -> Double.compare(b.score, a.score));
        if (results.size() > limit)
            results = results.subList(0, limit);
        return results;
    }

    private double computeScore(String line, String query) {
        String lower = line.toLowerCase();
        int idx = 0, count = 0;
        while ((idx = lower.indexOf(query, idx)) != -1) {
            count++;
            idx += query.length();
        }
        return Math.min(1.0, 0.5 + count * 0.15);
    }

    private static class MemoryHit {
        final String path;
        final String snippet;
        final int startLine;
        final int endLine;
        final double score;

        MemoryHit(String path, String snippet, int startLine, int endLine, double score) {
            this.path = path;
            this.snippet = snippet;
            this.startLine = startLine;
            this.endLine = endLine;
            this.score = score;
        }
    }
}
