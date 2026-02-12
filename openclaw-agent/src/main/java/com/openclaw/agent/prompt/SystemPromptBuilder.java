package com.openclaw.agent.prompt;

import java.util.*;

/**
 * Builds system prompts for the agent.
 * Simplified version corresponding to TypeScript's system-prompt.ts
 * buildAgentSystemPrompt.
 *
 * <p>
 * The system prompt contains sections for identity, runtime information,
 * workspace context, and available tools.
 * </p>
 */
public class SystemPromptBuilder {

    /**
     * Build a system prompt with the given parameters.
     */
    public static String build(SystemPromptParams params) {
        List<String> sections = new ArrayList<>();

        // --- Identity ---
        sections.add(buildIdentitySection(params));

        // --- Runtime ---
        sections.add(buildRuntimeSection(params));

        // --- Workspace ---
        if (params.workspaceDir != null) {
            sections.add(buildWorkspaceSection(params));
        }

        // --- Tools ---
        if (params.toolNames != null && !params.toolNames.isEmpty()) {
            sections.add(buildToolsSection(params));
        }

        // --- Extra (user-provided system prompt appended last) ---
        if (params.extraSystemPrompt != null && !params.extraSystemPrompt.isBlank()) {
            sections.add(params.extraSystemPrompt.trim());
        }

        return String.join("\n\n", sections);
    }

    // --- Section Builders ---

    private static String buildIdentitySection(SystemPromptParams params) {
        String name = params.agentName != null ? params.agentName : "Assistant";
        String model = params.modelId != null ? params.modelId : "unknown";

        StringBuilder sb = new StringBuilder();
        sb.append("You are ").append(name).append(", a helpful AI assistant");
        if (params.agentDescription != null) {
            sb.append(". ").append(params.agentDescription);
        }
        sb.append(".\n");
        sb.append("You are powered by ").append(model).append(".\n");

        // Communication style
        sb.append("\n## Communication\n");
        sb.append("- Be concise and precise in your responses.\n");
        sb.append("- Use markdown formatting for code, lists, and structured content.\n");
        sb.append("- If you are unsure about something, say so rather than guessing.\n");
        sb.append("- When making changes, explain your reasoning briefly.\n");

        return sb.toString();
    }

    private static String buildRuntimeSection(SystemPromptParams params) {
        StringBuilder sb = new StringBuilder();
        sb.append("## Runtime Information\n");
        sb.append("- OS: ").append(System.getProperty("os.name")).append(" ").append(System.getProperty("os.arch"))
                .append("\n");
        sb.append("- Java: ").append(System.getProperty("java.version")).append("\n");
        sb.append("- Runtime: OpenClaw Gateway (Java)").append("\n");
        if (params.modelId != null) {
            sb.append("- Model: ").append(params.modelId).append("\n");
        }
        return sb.toString();
    }

    private static String buildWorkspaceSection(SystemPromptParams params) {
        StringBuilder sb = new StringBuilder();
        sb.append("## Workspace\n");
        sb.append("- Working directory: `").append(params.workspaceDir).append("`\n");
        sb.append("- All file paths should be absolute.\n");
        sb.append("- You have access to read, write, and execute files in the workspace.\n");
        return sb.toString();
    }

    private static String buildToolsSection(SystemPromptParams params) {
        StringBuilder sb = new StringBuilder();
        sb.append("## Available Tools\n");
        sb.append("You have access to the following tools:\n\n");
        for (String toolName : params.toolNames) {
            sb.append("- `").append(toolName).append("`\n");
        }
        sb.append("\nUse tools when they would help accomplish the task. ");
        sb.append("Prefer using specific tools over running shell commands when possible.\n");
        return sb.toString();
    }

    /**
     * Parameters for building a system prompt.
     */
    public static class SystemPromptParams {
        private String agentName;
        private String agentDescription;
        private String modelId;
        private String workspaceDir;
        private String extraSystemPrompt;
        private List<String> toolNames;

        public static Builder builder() {
            return new Builder();
        }

        public static class Builder {
            private final SystemPromptParams params = new SystemPromptParams();

            public Builder agentName(String val) {
                params.agentName = val;
                return this;
            }

            public Builder agentDescription(String val) {
                params.agentDescription = val;
                return this;
            }

            public Builder modelId(String val) {
                params.modelId = val;
                return this;
            }

            public Builder workspaceDir(String val) {
                params.workspaceDir = val;
                return this;
            }

            public Builder extraSystemPrompt(String val) {
                params.extraSystemPrompt = val;
                return this;
            }

            public Builder toolNames(List<String> val) {
                params.toolNames = val;
                return this;
            }

            public SystemPromptParams build() {
                return params;
            }
        }
    }
}
