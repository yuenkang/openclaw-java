package com.openclaw.agent.prompt;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class SystemPromptBuilderTest {

    @Test
    void build_withAllParams_containsAllSections() {
        String prompt = SystemPromptBuilder.build(
                SystemPromptBuilder.SystemPromptParams.builder()
                        .agentName("TestBot")
                        .agentDescription("A test assistant")
                        .modelId("anthropic/claude-sonnet-4-5")
                        .workspaceDir("/tmp/workspace")
                        .toolNames(List.of("exec", "read_file", "write_file"))
                        .extraSystemPrompt("Always respond in JSON.")
                        .build());

        assertNotNull(prompt);
        assertTrue(prompt.contains("TestBot"), "Should contain agent name");
        assertTrue(prompt.contains("A test assistant"), "Should contain description");
        assertTrue(prompt.contains("anthropic/claude-sonnet-4-5"), "Should contain model ID");
        assertTrue(prompt.contains("/tmp/workspace"), "Should contain workspace dir");
        assertTrue(prompt.contains("`exec`"), "Should list tools");
        assertTrue(prompt.contains("`read_file`"), "Should list tools");
        assertTrue(prompt.contains("Always respond in JSON."), "Should contain extra prompt");
    }

    @Test
    void build_withMinimalParams_usesDefaults() {
        String prompt = SystemPromptBuilder.build(
                SystemPromptBuilder.SystemPromptParams.builder().build());

        assertNotNull(prompt);
        assertTrue(prompt.contains("Assistant"), "Should use default agent name");
        assertTrue(prompt.contains("Runtime Information"), "Should include runtime section");
        assertFalse(prompt.contains("Workspace"), "Should not include workspace when null");
        assertFalse(prompt.contains("Available Tools"), "Should not include tools when empty");
    }

    @Test
    void build_withToolNames_listsAllTools() {
        String prompt = SystemPromptBuilder.build(
                SystemPromptBuilder.SystemPromptParams.builder()
                        .toolNames(List.of("grep_search", "list_dir"))
                        .build());

        assertTrue(prompt.contains("Available Tools"));
        assertTrue(prompt.contains("`grep_search`"));
        assertTrue(prompt.contains("`list_dir`"));
    }

    @Test
    void build_runtimeSection_containsOsInfo() {
        String prompt = SystemPromptBuilder.build(
                SystemPromptBuilder.SystemPromptParams.builder().build());

        assertTrue(prompt.contains("Runtime Information"));
        assertTrue(prompt.contains(System.getProperty("os.name")));
        assertTrue(prompt.contains("Java"));
    }
}
