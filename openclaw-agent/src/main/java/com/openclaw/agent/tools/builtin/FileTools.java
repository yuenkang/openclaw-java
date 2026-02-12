package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Stream;

/**
 * File system tools: read, write, list, search.
 * Provides basic file operations for the agent.
 */
@Slf4j
public class FileTools {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /** Read a file and return its contents. */
    public static AgentTool readFile() {
        return new AgentTool() {
            @Override
            public String getName() {
                return "read_file";
            }

            @Override
            public String getDescription() {
                return "Read the contents of a file at the given path.";
            }

            @Override
            public JsonNode getParameterSchema() {
                ObjectNode schema = MAPPER.createObjectNode();
                schema.put("type", "object");
                ObjectNode props = schema.putObject("properties");
                props.putObject("path").put("type", "string").put("description", "Absolute file path");
                schema.putArray("required").add("path");
                return schema;
            }

            @Override
            public CompletableFuture<ToolResult> execute(ToolContext context) {
                return CompletableFuture.supplyAsync(() -> {
                    String path = context.getParameters().path("path").asText("");
                    try {
                        String content = Files.readString(Path.of(path), StandardCharsets.UTF_8);
                        if (content.length() > 100_000) {
                            content = content.substring(0, 100_000) + "\n... (truncated)";
                        }
                        return ToolResult.ok(content);
                    } catch (IOException e) {
                        return ToolResult.fail("Failed to read file: " + e.getMessage());
                    }
                });
            }
        };
    }

    /** Write content to a file, creating parent directories as needed. */
    public static AgentTool writeFile() {
        return new AgentTool() {
            @Override
            public String getName() {
                return "write_file";
            }

            @Override
            public String getDescription() {
                return "Write content to a file at the specified path. Creates parent directories if needed. The path must be a complete file path including filename (e.g. /tmp/hello.py), not a directory.";
            }

            @Override
            public JsonNode getParameterSchema() {
                ObjectNode schema = MAPPER.createObjectNode();
                schema.put("type", "object");
                ObjectNode props = schema.putObject("properties");
                props.putObject("path").put("type", "string")
                        .put("description", "Absolute file path including filename, e.g. /home/user/hello.py");
                props.putObject("content").put("type", "string").put("description", "File content to write");
                schema.putArray("required").add("path").add("content");
                return schema;
            }

            @Override
            public CompletableFuture<ToolResult> execute(ToolContext context) {
                return CompletableFuture.supplyAsync(() -> {
                    String path = context.getParameters().path("path").asText("");
                    String content = context.getParameters().path("content").asText("");
                    try {
                        Path filePath = Path.of(path);
                        if (Files.isDirectory(filePath)) {
                            return ToolResult
                                    .fail("Cannot write to directory: " + path + ". Please specify a file path.");
                        }
                        // Ensure parent directories exist before writing.
                        // Note: Files.exists() follows symlinks, so /tmp (symlink to /private/tmp on
                        // macOS)
                        // is correctly detected as existing. Calling Files.createDirectories() directly
                        // on a symlink directory would throw FileAlreadyExistsException.
                        if (filePath.getParent() != null && !Files.exists(filePath.getParent())) {
                            Files.createDirectories(filePath.getParent());
                        }
                        Files.writeString(filePath, content, StandardCharsets.UTF_8);
                        return ToolResult.ok("Written " + content.length() + " chars to " + path);
                    } catch (IOException e) {
                        return ToolResult.fail("Failed to write file: " + e.getMessage());
                    }
                });
            }
        };
    }

    /** List files in a directory. */
    public static AgentTool listDir() {
        return new AgentTool() {
            @Override
            public String getName() {
                return "list_dir";
            }

            @Override
            public String getDescription() {
                return "List files and directories in the given path.";
            }

            @Override
            public JsonNode getParameterSchema() {
                ObjectNode schema = MAPPER.createObjectNode();
                schema.put("type", "object");
                ObjectNode props = schema.putObject("properties");
                props.putObject("path").put("type", "string").put("description", "Directory path");
                schema.putArray("required").add("path");
                return schema;
            }

            @Override
            public CompletableFuture<ToolResult> execute(ToolContext context) {
                return CompletableFuture.supplyAsync(() -> {
                    String path = context.getParameters().path("path").asText("");
                    try (Stream<Path> entries = Files.list(Path.of(path))) {
                        StringBuilder sb = new StringBuilder();
                        entries.sorted().forEach(p -> {
                            String type = Files.isDirectory(p) ? "[DIR]  " : "[FILE] ";
                            sb.append(type).append(p.getFileName()).append("\n");
                        });
                        return ToolResult.ok(sb.toString());
                    } catch (IOException e) {
                        return ToolResult.fail("Failed to list directory: " + e.getMessage());
                    }
                });
            }
        };
    }

    /** Search file contents for a pattern (grep-like). */
    public static AgentTool grepSearch() {
        return new AgentTool() {
            @Override
            public String getName() {
                return "grep_search";
            }

            @Override
            public String getDescription() {
                return "Search for a text pattern in files under the given directory. " +
                        "Returns matching lines with file paths and line numbers. " +
                        "Use include glob to filter by file extension (e.g. \"*.java\").";
            }

            @Override
            public JsonNode getParameterSchema() {
                ObjectNode schema = MAPPER.createObjectNode();
                schema.put("type", "object");
                ObjectNode props = schema.putObject("properties");
                props.putObject("query").put("type", "string")
                        .put("description", "Search query (plain text or regex)");
                props.putObject("path").put("type", "string")
                        .put("description", "Directory to search in");
                props.putObject("include").put("type", "string")
                        .put("description", "Optional file glob pattern, e.g. *.java");
                schema.putArray("required").add("query").add("path");
                return schema;
            }

            @Override
            public CompletableFuture<ToolResult> execute(ToolContext context) {
                return CompletableFuture.supplyAsync(() -> {
                    String query = context.getParameters().path("query").asText("");
                    String searchPath = context.getParameters().path("path").asText("");
                    String include = context.getParameters().path("include").asText("");

                    if (query.isBlank() || searchPath.isBlank()) {
                        return ToolResult.fail("query and path are required");
                    }

                    Path dir = Path.of(searchPath);
                    if (!Files.exists(dir)) {
                        return ToolResult.fail("Path does not exist: " + searchPath);
                    }

                    // Build glob matcher if include is specified
                    PathMatcher globMatcher = null;
                    if (!include.isBlank()) {
                        globMatcher = FileSystems.getDefault()
                                .getPathMatcher("glob:" + include);
                    }

                    StringBuilder sb = new StringBuilder();
                    int matchCount = 0;
                    final int MAX_MATCHES = 50;

                    try {
                        PathMatcher finalGlobMatcher = globMatcher;
                        try (Stream<Path> walk = Files.walk(dir, 10)) {
                            var files = walk
                                    .filter(Files::isRegularFile)
                                    .filter(p -> {
                                        if (finalGlobMatcher != null) {
                                            return finalGlobMatcher.matches(p.getFileName());
                                        }
                                        return true;
                                    })
                                    .sorted()
                                    .toList();

                            for (Path file : files) {
                                if (matchCount >= MAX_MATCHES)
                                    break;
                                try {
                                    var lines = Files.readAllLines(file, StandardCharsets.UTF_8);
                                    for (int i = 0; i < lines.size(); i++) {
                                        if (matchCount >= MAX_MATCHES)
                                            break;
                                        if (lines.get(i).contains(query)) {
                                            sb.append(file).append(":")
                                                    .append(i + 1).append(": ")
                                                    .append(lines.get(i).strip())
                                                    .append("\n");
                                            matchCount++;
                                        }
                                    }
                                } catch (Exception e) {
                                    // Skip binary / unreadable files
                                }
                            }
                        }
                    } catch (IOException e) {
                        return ToolResult.fail("Search failed: " + e.getMessage());
                    }

                    if (matchCount == 0) {
                        return ToolResult.ok("No matches found for: " + query);
                    }
                    if (matchCount >= MAX_MATCHES) {
                        sb.append("\n... (capped at ").append(MAX_MATCHES).append(" matches)");
                    }
                    return ToolResult.ok(sb.toString());
                });
            }
        };
    }
}
