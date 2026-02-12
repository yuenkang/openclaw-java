package com.openclaw.agent.tools;

import com.openclaw.common.config.OpenClawConfig;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Factory for assembling the core coding agent tools.
 * Corresponds to TypeScript's pi-tools.ts createOpenClawCodingTools.
 *
 * <p>
 * This factory:
 * </p>
 * <ul>
 * <li>Gathers base coding tools (read/write/edit)</li>
 * <li>Adds exec and process tools</li>
 * <li>Adds OpenClaw extension tools (sessions, gateway, web, etc.)</li>
 * <li>Applies tool policy filtering</li>
 * <li>Normalizes tool schemas for provider compatibility</li>
 * </ul>
 */
@Slf4j
public class CodingToolFactory {

    @Data
    @Builder
    public static class CodingToolOptions {
        /** Current session key */
        private String sessionKey;
        /** Working directory */
        private String workspaceDir;
        /** Agent directory */
        private String agentDir;
        /** Agent account ID */
        private String agentAccountId;
        /** Model provider (anthropic, openai, google) */
        private String modelProvider;
        /** Model ID */
        private String modelId;
        /** Configuration */
        private OpenClawConfig config;
        /** Whether to include sandbox tools only */
        private boolean sandboxed;
        /** Sandbox root directory */
        private String sandboxRoot;
        /** Whether sender is owner */
        @Builder.Default
        private boolean senderIsOwner = true;
    }

    /**
     * Create the full set of coding tools based on options.
     */
    public static List<AgentTool> createCodingTools(CodingToolOptions options) {
        List<AgentTool> tools = new ArrayList<>();

        // 1. Gather existing registered tools from ToolRegistry
        // (In practice, the caller may pre-populate with read/write/edit/exec)
        // Here we add extension tools from OpenClawToolFactory
        tools.addAll(OpenClawToolFactory.createTools(
                OpenClawToolFactory.OpenClawToolOptions.builder()
                        .sessionKey(options.getSessionKey())
                        .workspaceDir(options.getWorkspaceDir())
                        .agentDir(options.getAgentDir())
                        .agentAccountId(options.getAgentAccountId())
                        .sandboxed(options.isSandboxed())
                        .sandboxRoot(options.getSandboxRoot())
                        .config(options.getConfig())
                        .build()));

        // 2. Apply tool policy filtering
        ToolPolicy policy = ToolPolicy.fromConfig(options.getConfig());
        tools = filterToolsByPolicy(tools, policy);

        log.info("Assembled {} coding tools for session {}", tools.size(), options.getSessionKey());
        return Collections.unmodifiableList(tools);
    }

    /**
     * Create tools and register them all into a ToolRegistry.
     */
    public static ToolRegistry createToolRegistry(CodingToolOptions options) {
        ToolRegistry registry = new ToolRegistry();
        List<AgentTool> tools = createCodingTools(options);
        registry.registerAll(tools);
        return registry;
    }

    /**
     * Create tools, register them, and merge with existing additional tools.
     */
    public static ToolRegistry createToolRegistry(CodingToolOptions options,
            Collection<AgentTool> additionalTools) {
        ToolRegistry registry = createToolRegistry(options);
        if (additionalTools != null) {
            registry.registerAll(additionalTools);
        }
        return registry;
    }

    /**
     * Filter tools by a ToolPolicy.
     */
    public static List<AgentTool> filterToolsByPolicy(List<AgentTool> tools, ToolPolicy policy) {
        if (policy == null || policy == ToolPolicy.ALLOW_ALL) {
            return tools;
        }
        List<AgentTool> filtered = new ArrayList<>();
        for (AgentTool tool : tools) {
            if (policy.isAllowed(tool.getName())) {
                filtered.add(tool);
            } else {
                log.debug("Tool '{}' filtered by policy", tool.getName());
            }
        }
        return filtered;
    }
}
