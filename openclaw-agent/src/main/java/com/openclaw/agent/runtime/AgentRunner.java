package com.openclaw.agent.runtime;

import com.openclaw.agent.hooks.InternalHookRegistry;
import com.openclaw.agent.hooks.InternalHookRegistry.HookEvent;
import com.openclaw.agent.hooks.InternalHookRegistry.HookEventType;
import com.openclaw.agent.models.ModelProvider;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.agent.prompt.SystemPromptBuilder;
import com.openclaw.agent.skills.SkillLoader;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolRegistry;
import com.openclaw.common.config.OpenClawConfig;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Core agent execution engine.
 * Corresponds to TypeScript's pi-embedded-runner.ts.
 *
 * <p>
 * Runs a multi-turn agent loop: user message → LLM → tool calls → LLM → ... →
 * final response.
 * </p>
 */
@Slf4j
public class AgentRunner {

    /**
     * Listener for streaming agent execution events.
     * Allows real-time notification of LLM output and tool execution.
     */
    public interface AgentEventListener {
        /** Called when the LLM produces text output. */
        void onDelta(String text);

        /** Called when a tool execution starts. */
        void onToolStart(String toolName, String toolId);

        /** Called when a tool execution completes. */
        void onToolEnd(String toolName, String toolId, String result, boolean success);

        /** Called when the agent run completes successfully. */
        void onComplete(String finalMessage);

        /** Called when the agent run fails. */
        void onError(String error);
    }

    /** No-op listener for when streaming is not needed. */
    public static final AgentEventListener NOOP_LISTENER = new AgentEventListener() {
        public void onDelta(String text) {
        }

        public void onToolStart(String toolName, String toolId) {
        }

        public void onToolEnd(String toolName, String toolId, String result, boolean success) {
        }

        public void onComplete(String finalMessage) {
        }

        public void onError(String error) {
        }
    };

    private final ModelProviderRegistry modelRegistry;
    private final ToolRegistry toolRegistry;
    private final int maxTurns;
    private final InternalHookRegistry hookRegistry;
    private final SubagentRegistry subagentRegistry;

    public AgentRunner(ModelProviderRegistry modelRegistry, ToolRegistry toolRegistry) {
        this(modelRegistry, toolRegistry, 25, null, null);
    }

    public AgentRunner(ModelProviderRegistry modelRegistry, ToolRegistry toolRegistry, int maxTurns) {
        this(modelRegistry, toolRegistry, maxTurns, null, null);
    }

    public AgentRunner(ModelProviderRegistry modelRegistry, ToolRegistry toolRegistry, int maxTurns,
            InternalHookRegistry hookRegistry, SubagentRegistry subagentRegistry) {
        this.modelRegistry = modelRegistry;
        this.toolRegistry = toolRegistry;
        this.maxTurns = maxTurns;
        this.hookRegistry = hookRegistry;
        this.subagentRegistry = subagentRegistry;
    }

    /**
     * Run a single agent turn (may involve multiple LLM calls with tool use).
     */
    public CompletableFuture<AgentResult> run(AgentRunContext context) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                return executeLoop(context);
            } catch (Exception e) {
                log.error("Agent run failed: {}", e.getMessage(), e);
                return AgentResult.builder()
                        .success(false)
                        .error(e.getMessage())
                        .build();
            }
        });
    }

    private AgentResult executeLoop(AgentRunContext context) {
        List<ModelProvider.ChatMessage> messages = new ArrayList<>(context.getMessages());
        ModelProvider provider = modelRegistry.resolve(context.getModelId());

        if (provider == null) {
            return AgentResult.builder()
                    .success(false)
                    .error("Model provider not found for: " + context.getModelId())
                    .build();
        }

        // --- Hook: agent run start ---
        if (hookRegistry != null) {
            hookRegistry.trigger(new HookEvent(
                    HookEventType.AGENT, "run:start",
                    context.getSessionKey(),
                    Map.of(
                            "runId", context.getRunId() != null ? context.getRunId() : "unknown",
                            "modelId", context.getModelId() != null ? context.getModelId() : "")));
        }

        // Auto-build system prompt if not provided
        if (context.getSystemPrompt() == null || context.getSystemPrompt().isBlank()) {
            // Resolve skills prompt via SkillLoader
            String skillsPrompt = null;
            if (context.getConfig() != null) {
                try {
                    skillsPrompt = SkillLoader.resolveSkillsPromptForRun(
                            context.getCwd(), context.getConfig());
                } catch (Exception e) {
                    log.debug("Skills prompt generation failed: {}", e.getMessage());
                }
            }

            String autoPrompt = SystemPromptBuilder.build(
                    SystemPromptBuilder.SystemPromptParams.builder()
                            .modelId(context.getModelId())
                            .workspaceDir(context.getCwd())
                            .toolNames(new ArrayList<>(toolRegistry.getToolNames()))
                            .skillsPrompt(skillsPrompt)
                            .build());
            context.setSystemPrompt(autoPrompt);
        }

        List<AgentEvent> events = new ArrayList<>();
        int turns = 0;

        while (turns < maxTurns) {
            turns++;

            // Check cancellation
            if (context.isCancelled()) {
                return AgentResult.builder()
                        .success(false)
                        .error("Cancelled")
                        .events(events)
                        .build();
            }

            // --- Compaction: prune history if approaching context limit ---
            if (context.isCompactionEnabled() && context.getMaxTokens() > 0) {
                int currentTokens = CompactionService.estimateMessagesTokens(messages);
                int contextBudget = context.getMaxTokens() * 4; // rough context window
                if (currentTokens > contextBudget * 0.8) {
                    log.info("History nearing context limit ({}/{} tokens), compacting",
                            currentTokens, contextBudget);
                    try {
                        CompactionService.CompactionResult cr = CompactionService.compact(
                                provider, context.getModelId(), messages,
                                contextBudget, contextBudget / 4, null).join();
                        messages = new ArrayList<>(cr.messages());
                        log.info("Compacted: dropped {} tokens, {} remaining",
                                cr.compactedTokens(), cr.remainingTokens());
                    } catch (Exception e) {
                        log.warn("Compaction failed, continuing with full history: {}", e.getMessage());
                    }
                }
            }

            // Call LLM with streaming
            ModelProvider.ChatRequest request = ModelProvider.ChatRequest.builder()
                    .model(context.getModelId())
                    .messages(messages)
                    .maxTokens(context.getMaxTokens())
                    .temperature(context.getTemperature())
                    .tools(toolRegistry.toDefinitions())
                    .systemPrompt(context.getSystemPrompt())
                    .stream(true)
                    .build();

            // Stream listener bridges ModelProvider.StreamListener → AgentEventListener
            ModelProvider.StreamListener streamListener = new ModelProvider.StreamListener() {
                @Override
                public void onText(String delta) {
                    if (context.getListener() != null) {
                        context.getListener().onDelta(delta);
                    }
                }
            };

            ModelProvider.ChatResponse response;
            try {
                response = provider.chatStream(request, streamListener).join();
            } catch (Exception e) {
                // Wrap as FailoverError if classifiable
                FailoverError fe = FailoverError.coerceToFailoverError(
                        e, null, context.getModelId(), null);
                String errorMsg = fe != null
                        ? String.format("LLM call failed [%s]: %s", fe.getReason(), fe.getMessage())
                        : "LLM call failed: " + e.getMessage();
                return AgentResult.builder()
                        .success(false)
                        .error(errorMsg)
                        .events(events)
                        .build();
            }

            String messageContent = response.getMessage() != null ? response.getMessage().getContent() : null;
            events.add(AgentEvent.builder()
                    .type("message")
                    .content(messageContent)
                    .usage(response.getUsage())
                    .build());

            // Check if there are tool calls
            if (response.getToolUses() == null || response.getToolUses().isEmpty()) {
                // No tool calls — final response
                return AgentResult.builder()
                        .success(true)
                        .finalMessage(response.getMessage() != null ? response.getMessage().getContent() : "")
                        .events(events)
                        .turns(turns)
                        .build();
            }

            // Add assistant message with tool uses
            messages.add(ModelProvider.ChatMessage.builder()
                    .role("assistant")
                    .content(response.getMessage() != null ? response.getMessage().getContent() : "")
                    .toolUses(response.getToolUses())
                    .build());

            // Execute each tool call
            for (ModelProvider.ToolUse toolUse : response.getToolUses()) {
                events.add(AgentEvent.builder()
                        .type("tool_start")
                        .toolName(toolUse.getName())
                        .toolId(toolUse.getId())
                        .build());
                if (context.getListener() != null) {
                    context.getListener().onToolStart(toolUse.getName(), toolUse.getId());
                }

                AgentTool.ToolResult toolResult = executeTool(toolUse, context);

                events.add(AgentEvent.builder()
                        .type("tool_end")
                        .toolName(toolUse.getName())
                        .toolId(toolUse.getId())
                        .content(toolResult.isSuccess() ? toolResult.getOutput() : toolResult.getError())
                        .build());
                if (context.getListener() != null) {
                    context.getListener().onToolEnd(toolUse.getName(), toolUse.getId(),
                            toolResult.isSuccess() ? toolResult.getOutput() : toolResult.getError(),
                            toolResult.isSuccess());
                }

                // Add tool result as message
                messages.add(ModelProvider.ChatMessage.builder()
                        .role("tool")
                        .content(toolResult.isSuccess() ? toolResult.getOutput() : "Error: " + toolResult.getError())
                        .toolUseId(toolUse.getId())
                        .build());
            }
        }

        AgentResult maxTurnsResult = AgentResult.builder()
                .success(false)
                .error("Max turns exceeded (" + maxTurns + ")")
                .events(events)
                .turns(turns)
                .build();
        triggerRunEnd(context, maxTurnsResult);
        return maxTurnsResult;
    }

    private void triggerRunEnd(AgentRunContext context, AgentResult result) {
        if (hookRegistry != null) {
            hookRegistry.trigger(new HookEvent(
                    HookEventType.AGENT, "run:end",
                    context.getSessionKey(),
                    Map.of(
                            "runId", context.getRunId() != null ? context.getRunId() : "unknown",
                            "success", String.valueOf(result.isSuccess()),
                            "turns", String.valueOf(result.getTurns()))));
        }
    }

    private AgentTool.ToolResult executeTool(ModelProvider.ToolUse toolUse, AgentRunContext context) {
        Optional<AgentTool> tool = toolRegistry.get(toolUse.getName());
        if (tool.isEmpty()) {
            return AgentTool.ToolResult.fail("Unknown tool: " + toolUse.getName());
        }

        // Hook: tool execution start
        if (hookRegistry != null) {
            hookRegistry.trigger(new HookEvent(
                    HookEventType.AGENT, "tool:execute:start",
                    context.getSessionKey(),
                    Map.of(
                            "toolName", toolUse.getName(),
                            "toolId", toolUse.getId() != null ? toolUse.getId() : "")));
        }

        try {
            com.fasterxml.jackson.databind.ObjectMapper mapper = new com.fasterxml.jackson.databind.ObjectMapper();
            com.fasterxml.jackson.databind.JsonNode params = mapper.valueToTree(toolUse.getInput());

            AgentTool.ToolContext toolCtx = AgentTool.ToolContext.builder()
                    .parameters(params)
                    .sessionKey(context.getSessionKey())
                    .cwd(context.getCwd())
                    .config(context.getConfig())
                    .build();

            AgentTool.ToolResult result = tool.get().execute(toolCtx).join();

            // Hook: tool execution end
            if (hookRegistry != null) {
                hookRegistry.trigger(new HookEvent(
                        HookEventType.AGENT, "tool:execute:end",
                        context.getSessionKey(),
                        Map.of(
                                "toolName", toolUse.getName(),
                                "toolId", toolUse.getId() != null ? toolUse.getId() : "",
                                "success", String.valueOf(result.isSuccess()))));
            }

            return result;
        } catch (Exception e) {
            log.error("Tool {} execution failed: {}", toolUse.getName(), e.getMessage(), e);

            // Hook: tool execution end (error)
            if (hookRegistry != null) {
                hookRegistry.trigger(new HookEvent(
                        HookEventType.AGENT, "tool:execute:end",
                        context.getSessionKey(),
                        Map.of(
                                "toolName", toolUse.getName(),
                                "toolId", toolUse.getId() != null ? toolUse.getId() : "",
                                "success", "false",
                                "error", e.getMessage() != null ? e.getMessage() : "unknown")));
            }

            return AgentTool.ToolResult.fail("Execution error: " + e.getMessage());
        }
    }

    // --- Data types ---

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AgentRunContext {
        private String runId;
        private String sessionKey;
        private String agentId;
        private String modelId;
        private String systemPrompt;
        private List<ModelProvider.ChatMessage> messages;
        private String cwd;
        private int maxTokens;
        private double temperature;
        private OpenClawConfig config;
        private volatile boolean cancelled;
        /** Enable automatic compaction when history nears context limit. */
        @Builder.Default
        private boolean compactionEnabled = false;
        @Builder.Default
        private AgentEventListener listener = NOOP_LISTENER;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AgentResult {
        private boolean success;
        private String finalMessage;
        private String error;
        private List<AgentEvent> events;
        private int turns;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AgentEvent {
        private String type; // "message" | "tool_start" | "tool_end"
        private String content;
        private String toolName;
        private String toolId;
        private ModelProvider.Usage usage;
    }
}
