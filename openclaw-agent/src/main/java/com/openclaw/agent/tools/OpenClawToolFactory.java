package com.openclaw.agent.tools;

import com.openclaw.agent.tools.builtin.GatewayTool;
import com.openclaw.agent.tools.builtin.SessionStatusTool;
import com.openclaw.common.config.OpenClawConfig;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Factory for OpenClaw extension tools (sessions, gateway, web, etc.).
 * Corresponds to TypeScript's openclaw-tools.ts createOpenClawTools.
 *
 * <p>
 * Produces a list of tools that are complementary to the base coding tools
 * (read/write/edit/exec). These tools handle gateway management, session
 * status, web search/fetch, and future extensions like browser/canvas/cron.
 * </p>
 */
@Slf4j
public class OpenClawToolFactory {

    @Data
    @Builder
    public static class OpenClawToolOptions {
        /** Current session key */
        private String sessionKey;
        /** Working directory */
        private String workspaceDir;
        /** Agent directory */
        private String agentDir;
        /** Agent account ID */
        private String agentAccountId;
        /** Whether running in sandbox */
        private boolean sandboxed;
        /** Sandbox root directory */
        private String sandboxRoot;
        /** Configuration */
        private OpenClawConfig config;
    }

    /**
     * Create all OpenClaw extension tools.
     */
    public static List<AgentTool> createTools(OpenClawToolOptions options) {
        List<AgentTool> tools = new ArrayList<>();

        // --- Session Status ---
        tools.add(SessionStatusTool.create(
                options.getSessionKey(),
                options.getConfig()));

        // --- Gateway ---
        tools.add(GatewayTool.create(
                options.getSessionKey(),
                options.getConfig()));

        // --- Web Search (already exists as WebSearchTool, added by caller) ---
        // The existing WebSearchTool in the tools package should be registered
        // by the caller or via CodingToolFactory.

        // --- Future tools (stubs / placeholders) ---
        // Browser, Canvas, Cron, Nodes, Image, Message, TTS,
        // Sessions List/History/Send/Spawn, Agents List
        // These will be implemented as separate tool classes.

        log.debug("Created {} OpenClaw extension tools", tools.size());
        return tools;
    }
}
