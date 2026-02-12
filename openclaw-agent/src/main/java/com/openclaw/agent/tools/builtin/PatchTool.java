package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.agent.tools.AgentTool;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Patch tool — apply text replacements to files.
 * Corresponds to parts of TypeScript's bash-tools.exec.ts editFile logic.
 */
@Slf4j
public class PatchTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Override
    public String getName() {
        return "patch";
    }

    @Override
    public String getDescription() {
        return "Apply a text patch/replacement to a file. Provide the file path, the exact text to find, and the replacement text.";
    }

    @Override
    public JsonNode getParameterSchema() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("file_path", Map.of(
                "type", "string",
                "description", "Absolute path to the file to patch"));
        props.put("old_text", Map.of(
                "type", "string",
                "description", "Exact text to find in the file (must be unique)"));
        props.put("new_text", Map.of(
                "type", "string",
                "description", "Replacement text"));

        Map<String, Object> schema = Map.of(
                "type", "object",
                "properties", props,
                "required", List.of("file_path", "old_text", "new_text"));
        return MAPPER.valueToTree(schema);
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        JsonNode args = context.getParameters();
        String filePath = args.get("file_path").asText();
        String oldText = args.get("old_text").asText();
        String newText = args.get("new_text").asText();

        Path path = Path.of(filePath);
        if (!Files.exists(path)) {
            return CompletableFuture.completedFuture(ToolResult.fail("File not found: " + filePath));
        }
        if (!Files.isRegularFile(path)) {
            return CompletableFuture.completedFuture(ToolResult.fail("Not a regular file: " + filePath));
        }

        try {
            String content = Files.readString(path);

            // Count occurrences
            int count = countOccurrences(content, oldText);
            if (count == 0) {
                return CompletableFuture.completedFuture(
                        ToolResult.fail("old_text not found in file. Ensure exact match including whitespace."));
            }
            if (count > 1) {
                return CompletableFuture.completedFuture(
                        ToolResult.fail(
                                "old_text found " + count + " times. Must be unique. Add context to disambiguate."));
            }

            // Apply replacement
            String newContent = content.replace(oldText, newText);
            Files.writeString(path, newContent);

            int linesChanged = newText.split("\n", -1).length - oldText.split("\n", -1).length;
            String msg = String.format("Patched %s (%s%d lines)", filePath,
                    linesChanged >= 0 ? "+" : "", linesChanged);
            log.info(msg);

            return CompletableFuture.completedFuture(ToolResult.ok(msg));
        } catch (IOException e) {
            return CompletableFuture.completedFuture(ToolResult.fail("IO error: " + e.getMessage()));
        }
    }

    private int countOccurrences(String text, String search) {
        int count = 0, idx = 0;
        while ((idx = text.indexOf(search, idx)) != -1) {
            count++;
            idx += search.length();
        }
        return count;
    }
}
