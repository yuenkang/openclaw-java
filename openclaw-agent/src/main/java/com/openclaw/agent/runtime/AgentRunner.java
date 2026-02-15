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
     * Run a single agent turn asynchronously.
     */
    public CompletableFuture<AgentResult> runAsync(AgentRunContext context) {
        return CompletableFuture.supplyAsync(() -> run(context));
    }

    /**
     * Run a single agent turn synchronously (may involve multiple LLM calls with
     * tool use).
     */
    public AgentResult run(AgentRunContext context) {
        try {
            // If userMessage is set or imageContentParts are present, prepend as user
            // message
            boolean hasText = context.getUserMessage() != null && !context.getUserMessage().isBlank();
            boolean hasImage = context.getImageContentParts() != null && !context.getImageContentParts().isEmpty();
            if (hasText || hasImage) {
                if (context.getMessages() == null) {
                    context.setMessages(new ArrayList<>());
                }
                // Support multimodal messages (text + images)
                if (hasImage) {
                    context.getMessages().add(ModelProvider.ChatMessage.builder()
                            .role("user")
                            .contentParts(context.getImageContentParts())
                            .build());
                } else {
                    context.getMessages().add(ModelProvider.ChatMessage.builder()
                            .role("user")
                            .content(context.getUserMessage())
                            .build());
                }
            }
            AgentResult result = executeLoop(context);
            // Notify listener of completion
            if (context.getListener() != null) {
                if (result.isSuccess()) {
                    context.getListener().onComplete(result.getFinalMessage());
                } else {
                    context.getListener().onError(result.getError());
                }
            }
            return result;
        } catch (Exception e) {
            log.error("Agent run failed: {}", e.getMessage(), e);
            String errorMsg = e.getMessage() != null ? e.getMessage() : e.toString();
            ErrorClassifier.FailoverReason reason = ErrorClassifier.classifyFailoverReason(errorMsg);
            if (context.getListener() != null) {
                context.getListener().onError(errorMsg);
            }
            return AgentResult.builder()
                    .success(false)
                    .error(errorMsg)
                    .errorKind(reason)
                    .build();
        }
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

        // Build tool-aware system prompt (always include tool definitions)
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

        if (context.getSystemPrompt() != null && !context.getSystemPrompt().isBlank()) {
            // Combine custom system prompt (persona/channel hints) with auto-generated
            // tool definitions so the model knows about available tools
            context.setSystemPrompt(context.getSystemPrompt() + "\n\n" + autoPrompt);
        } else {
            context.setSystemPrompt(autoPrompt);
        }

        List<AgentEvent> events = new ArrayList<>();
        int turns = 0;
        // Aggregate usage across all LLM calls
        ModelProvider.Usage totalUsage = new ModelProvider.Usage(0, 0, 0, 0);

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
                // Classify error for failover / user messaging
                String rawError = e.getMessage() != null ? e.getMessage() : e.toString();
                ErrorClassifier.FailoverReason reason = ErrorClassifier.classifyFailoverReason(rawError);
                String errorMsg = String.format("LLM call failed [%s]: %s", reason.name(), rawError);
                return AgentResult.builder()
                        .success(false)
                        .error(errorMsg)
                        .errorKind(reason)
                        .events(events)
                        .build();
            }

            String messageContent = response.getMessage() != null ? response.getMessage().getContent() : null;
            events.add(AgentEvent.builder()
                    .type("message")
                    .content(messageContent)
                    .usage(response.getUsage())
                    .build());

            // Accumulate usage from this turn
            if (response.getUsage() != null) {
                totalUsage.setInputTokens(totalUsage.getInputTokens() + response.getUsage().getInputTokens());
                totalUsage.setOutputTokens(totalUsage.getOutputTokens() + response.getUsage().getOutputTokens());
                totalUsage
                        .setCacheReadTokens(totalUsage.getCacheReadTokens() + response.getUsage().getCacheReadTokens());
                totalUsage.setCacheWriteTokens(
                        totalUsage.getCacheWriteTokens() + response.getUsage().getCacheWriteTokens());
            }

            // Check if there are tool calls
            if (response.getToolUses() == null || response.getToolUses().isEmpty()) {
                // No tool calls — final response
                return AgentResult.builder()
                        .success(true)
                        .finalMessage(response.getMessage() != null ? response.getMessage().getContent() : "")
                        .reasoningContent(
                                response.getMessage() != null ? response.getMessage().getReasoningContent() : null)
                        .events(events)
                        .turns(turns)
                        .totalUsage(totalUsage)
                        .build();
            }

            // Add assistant message with tool uses
            messages.add(ModelProvider.ChatMessage.builder()
                    .role("assistant")
                    .content(response.getMessage() != null ? response.getMessage().getContent() : "")
                    .reasoningContent(
                            response.getMessage() != null ? response.getMessage().getReasoningContent() : null)
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
                ModelProvider.ChatMessage.ChatMessageBuilder toolMsg = ModelProvider.ChatMessage.builder()
                        .role("tool")
                        .toolUseId(toolUse.getId());
                if (toolResult.getContentParts() != null && !toolResult.getContentParts().isEmpty()) {
                    // Multimodal tool result (e.g. screenshot image)
                    toolMsg.contentParts(toolResult.getContentParts());
                    // Also set text content as fallback
                    toolMsg.content(
                            toolResult.isSuccess() ? toolResult.getOutput() : "Error: " + toolResult.getError());
                } else {
                    toolMsg.content(
                            toolResult.isSuccess() ? toolResult.getOutput() : "Error: " + toolResult.getError());
                }
                messages.add(toolMsg.build());
            }
        }

        AgentResult maxTurnsResult = AgentResult.builder()
                .success(false)
                .error("Max turns exceeded (" + maxTurns + ")")
                .events(events)
                .turns(turns)
                .totalUsage(totalUsage)
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
        /**
         * Provider id (e.g. "anthropic", "openai"). Extracted from modelId if not set.
         */
        private String provider;
        private String systemPrompt;
        /** User message to prepend to messages list on run start. */
        private String userMessage;
        /** Multimodal content parts for user message (text + images). */
        private List<ModelProvider.ContentPart> imageContentParts;
        private List<ModelProvider.ChatMessage> messages;
        private String cwd;
        private int maxTokens;
        @Builder.Default
        private int maxTurns = 25;
        private double temperature;
        private OpenClawConfig config;
        private volatile boolean cancelled;
        /** Enable automatic compaction when history nears context limit. */
        @Builder.Default
        private boolean compactionEnabled = false;
        @Builder.Default
        private AgentEventListener listener = NOOP_LISTENER;

        public static final AgentEventListener NOOP_LISTENER = AgentRunner.NOOP_LISTENER;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AgentResult {
        private boolean success;
        private String finalMessage;
        /**
         * Reasoning/thinking content from the final LLM response (for thinking models).
         */
        private String reasoningContent;
        private String error;
        /** Classified error kind for failover / user messaging. */
        private ErrorClassifier.FailoverReason errorKind;
        private List<AgentEvent> events;
        private int turns;
        /** Aggregated token usage across all LLM calls. */
        private ModelProvider.Usage totalUsage;
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
