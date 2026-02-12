package com.openclaw.agent.prompt;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

/**
 * Builds system prompts for the agent.
 * Corresponds to TypeScript's system-prompt.ts buildAgentSystemPrompt.
 *
 * <p>
 * Supports three prompt modes:
 * <ul>
 * <li>{@code FULL} — all sections (main agent)</li>
 * <li>{@code MINIMAL} — tooling + workspace + runtime only (subagents)</li>
 * <li>{@code NONE} — identity line only</li>
 * </ul>
 */
public class SystemPromptBuilder {

    /** Controls which hardcoded sections are included. */
    public enum PromptMode {
        FULL, MINIMAL, NONE
    }

    /**
     * Build a system prompt with the given parameters.
     */
    public static String build(SystemPromptParams params) {
        PromptMode mode = params.promptMode != null ? params.promptMode : PromptMode.FULL;

        if (mode == PromptMode.NONE) {
            String name = params.agentName != null ? params.agentName : "Assistant";
            return "You are " + name + ", a helpful AI assistant.";
        }

        boolean isMinimal = mode == PromptMode.MINIMAL;
        List<String> sections = new ArrayList<>();

        // --- Identity (full only) ---
        if (!isMinimal) {
            sections.add(buildIdentitySection(params));
        }

        // --- Runtime ---
        sections.add(buildRuntimeSection(params));

        // --- Time (full only) ---
        if (!isMinimal) {
            sections.add(buildTimeSection(params));
        }

        // --- Workspace ---
        if (params.workspaceDir != null) {
            sections.add(buildWorkspaceSection(params));
        }

        // --- Tools ---
        if (params.toolNames != null && !params.toolNames.isEmpty()) {
            sections.add(buildToolsSection(params));
        }

        // --- Skills (full or minimal) ---
        if (params.skillsPrompt != null && !params.skillsPrompt.isBlank()) {
            sections.add(buildSkillsSection(params, isMinimal));
        }

        // --- Memory (full only) ---
        if (!isMinimal) {
            sections.add(buildMemorySection(params));
        }

        // --- Docs (full only) ---
        if (!isMinimal && params.docsPath != null && !params.docsPath.isBlank()) {
            sections.add(buildDocsSection(params));
        }

        // --- User identity (full only) ---
        if (!isMinimal && params.ownerLine != null && !params.ownerLine.isBlank()) {
            sections.add(buildUserIdentitySection(params));
        }

        // --- Extra (user-provided system prompt appended last) ---
        if (params.extraSystemPrompt != null && !params.extraSystemPrompt.isBlank()) {
            sections.add(params.extraSystemPrompt.trim());
        }

        return String.join("\n\n", sections);
    }

    // =========================================================================
    // Section Builders
    // =========================================================================

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
        sb.append("- OS: ").append(System.getProperty("os.name"))
                .append(" ").append(System.getProperty("os.arch")).append("\n");
        sb.append("- Java: ").append(System.getProperty("java.version")).append("\n");
        sb.append("- Runtime: OpenClaw Gateway (Java)").append("\n");
        if (params.modelId != null) {
            sb.append("- Model: ").append(params.modelId).append("\n");
        }
        if (params.thinkLevel != null) {
            sb.append("- Thinking: ").append(params.thinkLevel).append("\n");
        }
        return sb.toString();
    }

    private static String buildTimeSection(SystemPromptParams params) {
        StringBuilder sb = new StringBuilder();
        sb.append("## Current Time\n");
        try {
            ZoneId zone = params.timezone != null
                    ? ZoneId.of(params.timezone)
                    : ZoneId.systemDefault();
            ZonedDateTime now = ZonedDateTime.now(zone);
            sb.append("- ").append(now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss zzz")))
                    .append("\n");
        } catch (Exception e) {
            ZonedDateTime now = ZonedDateTime.now();
            sb.append("- ").append(now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss zzz")))
                    .append("\n");
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

    private static String buildSkillsSection(SystemPromptParams params, boolean isMinimal) {
        StringBuilder sb = new StringBuilder();
        sb.append("## Skills\n");
        if (isMinimal) {
            sb.append("Skills are available. Use the file reading tool to inspect skill files when needed.\n");
        }
        sb.append(params.skillsPrompt.trim()).append("\n");
        return sb.toString();
    }

    private static String buildMemorySection(SystemPromptParams params) {
        StringBuilder sb = new StringBuilder();
        sb.append("## Memory\n");
        sb.append("You have access to a persistent memory system. ");
        sb.append("Use the memory tool to store and retrieve information across sessions.\n");
        sb.append("- Store important decisions, user preferences, and project context.\n");
        sb.append("- Search memory before asking questions that may have been answered before.\n");
        return sb.toString();
    }

    private static String buildDocsSection(SystemPromptParams params) {
        StringBuilder sb = new StringBuilder();
        sb.append("## Documentation\n");
        sb.append("Project documentation is available at: `").append(params.docsPath).append("`\n");
        sb.append("Consult documentation before making significant changes.\n");
        return sb.toString();
    }

    private static String buildUserIdentitySection(SystemPromptParams params) {
        StringBuilder sb = new StringBuilder();
        sb.append("## User\n");
        sb.append(params.ownerLine.trim()).append("\n");
        return sb.toString();
    }

    // =========================================================================
    // Parameters
    // =========================================================================

    /**
     * Parameters for building a system prompt.
     */
    public static class SystemPromptParams {
        String agentName;
        String agentDescription;
        String modelId;
        String workspaceDir;
        String extraSystemPrompt;
        List<String> toolNames;
        // --- New fields ---
        PromptMode promptMode;
        String timezone;
        String skillsPrompt;
        String ownerLine;
        String thinkLevel;
        String docsPath;

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

            public Builder promptMode(PromptMode val) {
                params.promptMode = val;
                return this;
            }

            public Builder timezone(String val) {
                params.timezone = val;
                return this;
            }

            public Builder skillsPrompt(String val) {
                params.skillsPrompt = val;
                return this;
            }

            public Builder ownerLine(String val) {
                params.ownerLine = val;
                return this;
            }

            public Builder thinkLevel(String val) {
                params.thinkLevel = val;
                return this;
            }

            public Builder docsPath(String val) {
                params.docsPath = val;
                return this;
            }

            public SystemPromptParams build() {
                return params;
            }
        }
    }
}
