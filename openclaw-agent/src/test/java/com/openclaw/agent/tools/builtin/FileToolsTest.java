package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

class FileToolsTest {

    private final ObjectMapper mapper = new ObjectMapper();

    @TempDir
    Path tempDir;

    private Path testFile;

    @BeforeEach
    void setUp() throws Exception {
        testFile = tempDir.resolve("test.txt");
        Files.writeString(testFile, "hello world\nfoo bar\nbaz qux\n", StandardCharsets.UTF_8);

        // Create a subdirectory with another file for grep testing
        Path subDir = tempDir.resolve("sub");
        Files.createDirectories(subDir);
        Files.writeString(subDir.resolve("nested.java"), "public class Nested {\n}\n", StandardCharsets.UTF_8);
    }

    @Test
    void readFile_returnsContent() throws Exception {
        AgentTool tool = FileTools.readFile();
        ObjectNode params = mapper.createObjectNode();
        params.put("path", testFile.toString());

        AgentTool.ToolResult result = tool.execute(
                AgentTool.ToolContext.builder().parameters(params).build()).get();

        assertTrue(result.isSuccess());
        assertTrue(result.getOutput().contains("hello world"));
    }

    @Test
    void readFile_nonExistent_fails() throws Exception {
        AgentTool tool = FileTools.readFile();
        ObjectNode params = mapper.createObjectNode();
        params.put("path", tempDir.resolve("nope.txt").toString());

        AgentTool.ToolResult result = tool.execute(
                AgentTool.ToolContext.builder().parameters(params).build()).get();

        assertFalse(result.isSuccess());
        assertTrue(result.getError().contains("Failed to read"));
    }

    @Test
    void writeFile_createsFile() throws Exception {
        AgentTool tool = FileTools.writeFile();
        Path newFile = tempDir.resolve("output.txt");
        ObjectNode params = mapper.createObjectNode();
        params.put("path", newFile.toString());
        params.put("content", "written by test");

        AgentTool.ToolResult result = tool.execute(
                AgentTool.ToolContext.builder().parameters(params).build()).get();

        assertTrue(result.isSuccess());
        assertTrue(Files.exists(newFile));
        assertEquals("written by test", Files.readString(newFile));
    }

    @Test
    void writeFile_createsParentDirs() throws Exception {
        AgentTool tool = FileTools.writeFile();
        Path deepFile = tempDir.resolve("a/b/c/deep.txt");
        ObjectNode params = mapper.createObjectNode();
        params.put("path", deepFile.toString());
        params.put("content", "deep content");

        AgentTool.ToolResult result = tool.execute(
                AgentTool.ToolContext.builder().parameters(params).build()).get();

        assertTrue(result.isSuccess());
        assertTrue(Files.exists(deepFile));
    }

    @Test
    void listDir_listsContents() throws Exception {
        AgentTool tool = FileTools.listDir();
        ObjectNode params = mapper.createObjectNode();
        params.put("path", tempDir.toString());

        AgentTool.ToolResult result = tool.execute(
                AgentTool.ToolContext.builder().parameters(params).build()).get();

        assertTrue(result.isSuccess());
        assertTrue(result.getOutput().contains("test.txt"));
        assertTrue(result.getOutput().contains("[DIR]"));
        assertTrue(result.getOutput().contains("sub"));
    }

    @Test
    void grepSearch_findsMatches() throws Exception {
        AgentTool tool = FileTools.grepSearch();
        ObjectNode params = mapper.createObjectNode();
        params.put("query", "foo bar");
        params.put("path", tempDir.toString());

        AgentTool.ToolResult result = tool.execute(
                AgentTool.ToolContext.builder().parameters(params).build()).get();

        assertTrue(result.isSuccess());
        assertTrue(result.getOutput().contains("foo bar"));
        assertTrue(result.getOutput().contains(":2:"), "Should include line number");
    }

    @Test
    void grepSearch_withInclude_filtersFiles() throws Exception {
        AgentTool tool = FileTools.grepSearch();
        ObjectNode params = mapper.createObjectNode();
        params.put("query", "class");
        params.put("path", tempDir.toString());
        params.put("include", "*.java");

        AgentTool.ToolResult result = tool.execute(
                AgentTool.ToolContext.builder().parameters(params).build()).get();

        assertTrue(result.isSuccess());
        assertTrue(result.getOutput().contains("Nested"), "Should find in .java file");
        assertFalse(result.getOutput().contains("test.txt"), "Should not search .txt files");
    }

    @Test
    void grepSearch_noMatches_returnsMessage() throws Exception {
        AgentTool tool = FileTools.grepSearch();
        ObjectNode params = mapper.createObjectNode();
        params.put("query", "nonexistent_string_xyz");
        params.put("path", tempDir.toString());

        AgentTool.ToolResult result = tool.execute(
                AgentTool.ToolContext.builder().parameters(params).build()).get();

        assertTrue(result.isSuccess());
        assertTrue(result.getOutput().contains("No matches found"));
    }
}
