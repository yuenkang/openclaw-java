package com.openclaw.agent.tools;

import com.openclaw.agent.tools.builtin.*;
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
        /** Current channel id (for message tool context) */
        private String currentChannelId;
        /** Current channel provider (e.g. discord, telegram) */
        private String currentChannelProvider;
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

        // --- Message ---
        tools.add(new MessageTool(
                options.getCurrentChannelId(),
                options.getCurrentChannelProvider(),
                false));

        // --- Memory ---
        tools.add(new MemoryTool());

        // --- Web Fetch ---
        tools.add(new WebFetchTool());

        // --- Image ---
        tools.add(new ImageTool());

        // --- Cron ---
        tools.add(new CronTool());

        // --- Agents List ---
        tools.add(new AgentsListTool());

        // --- Nodes ---
        tools.add(new NodesTool());

        // --- Canvas ---
        tools.add(new CanvasTool());

        // --- TTS ---
        tools.add(new TtsTool());

        // --- Session Management ---
        tools.add(new SessionsListTool());
        tools.add(new SessionsHistoryTool());
        tools.add(new SessionsSendTool(
                options.getSessionKey(),
                options.getCurrentChannelProvider(),
                options.isSandboxed()));
        tools.add(new SessionsSpawnTool(
                options.getSessionKey(),
                options.getCurrentChannelProvider(),
                options.isSandboxed()));

        log.debug("Created {} OpenClaw extension tools", tools.size());
        return tools;
    }
}
