package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import lombok.extern.slf4j.Slf4j;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * Command execution tool.
 * Corresponds to TypeScript's bash-tools.exec.ts.
 */
@Slf4j
public class ExecTool implements AgentTool {

    private static final int DEFAULT_TIMEOUT_SECONDS = 120;
    private static final int MAX_OUTPUT_LENGTH = 50_000;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public String getName() {
        return "exec";
    }

    @Override
    public String getDescription() {
        return "Execute a shell command and return stdout/stderr. " +
                "Use for running builds, tests, git commands, file operations, etc.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = objectMapper.createObjectNode();
        schema.put("type", "object");

        ObjectNode properties = schema.putObject("properties");

        ObjectNode command = properties.putObject("command");
        command.put("type", "string");
        command.put("description", "The shell command to execute");

        ObjectNode cwd = properties.putObject("cwd");
        cwd.put("type", "string");
        cwd.put("description", "Working directory for the command");

        ObjectNode timeout = properties.putObject("timeout");
        timeout.put("type", "integer");
        timeout.put("description", "Timeout in seconds (default: 120)");

        schema.putArray("required").add("command");
        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> {
            JsonNode params = context.getParameters();

            String command = params.path("command").asText("");
            if (command.isBlank()) {
                return ToolResult.fail("command is required");
            }

            String cwd = params.has("cwd") ? params.get("cwd").asText() : context.getCwd();
            int timeout = params.has("timeout") ? params.get("timeout").asInt() : DEFAULT_TIMEOUT_SECONDS;

            log.debug("Executing: {} (cwd={})", command, cwd);

            try {
                ProcessBuilder pb = new ProcessBuilder("/bin/sh", "-c", command);
                if (cwd != null) {
                    pb.directory(new File(cwd));
                }
                pb.redirectErrorStream(true);

                Process process = pb.start();

                // Read output
                String output;
                try (InputStream is = process.getInputStream()) {
                    output = new String(is.readAllBytes(), StandardCharsets.UTF_8);
                }

                boolean finished = process.waitFor(timeout, TimeUnit.SECONDS);
                if (!finished) {
                    process.destroyForcibly();
                    return ToolResult.fail("Command timed out after " + timeout + "s");
                }

                int exitCode = process.exitValue();

                // Truncate if too long
                if (output.length() > MAX_OUTPUT_LENGTH) {
                    output = output.substring(0, MAX_OUTPUT_LENGTH) + "\n... (truncated)";
                }

                if (exitCode == 0) {
                    return ToolResult.ok(output);
                } else {
                    return ToolResult.fail("Exit code " + exitCode + ":\n" + output);
                }

            } catch (Exception e) {
                log.error("Exec failed: {}", e.getMessage(), e);
                return ToolResult.fail("Execution error: " + e.getMessage());
            }
        });
    }
}
