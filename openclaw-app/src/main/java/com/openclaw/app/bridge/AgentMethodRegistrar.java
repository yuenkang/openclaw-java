package com.openclaw.app.bridge;

import com.fasterxml.jackson.databind.JsonNode;
import com.openclaw.agent.hooks.InternalHookRegistry;
import com.openclaw.agent.models.ModelProvider;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.agent.runtime.AgentRunner;
import com.openclaw.agent.runtime.SubagentRegistry;
import com.openclaw.agent.tools.ToolRegistry;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.gateway.session.SessionStore;
import com.openclaw.gateway.websocket.GatewayConnection;
import com.openclaw.gateway.websocket.GatewayMethodRouter;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Registers agent-related RPC methods (agent.run, agent.message).
 * Lives in the app module to bridge gateway and agent modules.
 */
@Slf4j
@Component
public class AgentMethodRegistrar {

    private final GatewayMethodRouter methodRouter;
    private final SessionStore sessionStore;
    private final AgentRunner agentRunner;
    private final ConfigService configService;
    private final InternalHookRegistry hookRegistry;
    private final SubagentRegistry subagentRegistry;

    public AgentMethodRegistrar(
            GatewayMethodRouter methodRouter,
            SessionStore sessionStore,
            ModelProviderRegistry modelProviderRegistry,
            ToolRegistry toolRegistry,
            ConfigService configService) {
        this.methodRouter = methodRouter;
        this.sessionStore = sessionStore;
        this.configService = configService;

        // Create application-level hook registry
        this.hookRegistry = new InternalHookRegistry();

        // Create subagent registry with persistence
        String stateDir = System.getProperty("user.dir") + "/.openclaw";
        this.subagentRegistry = new SubagentRegistry(
                stateDir + "/subagents/runs.json");
        this.subagentRegistry.restoreFromDisk();

        // Inject into AgentRunner
        this.agentRunner = new AgentRunner(
                modelProviderRegistry, toolRegistry, 25,
                hookRegistry, subagentRegistry);
    }

    @PostConstruct
    public void registerMethods() {
        methodRouter.registerMethod("agent.run", this::handleAgentRun);
        methodRouter.registerMethod("agent.message", this::handleAgentMessage);
        log.info("Registered 2 agent RPC methods (hooks={}, subagent={})",
                hookRegistry != null, subagentRegistry != null);
    }

    @PreDestroy
    public void shutdown() {
        if (subagentRegistry != null) {
            subagentRegistry.shutdown();
        }
    }

    /**
     * agent.run — Full agent execution with model, system prompt, and messages.
     *
     * Params:
     * modelId: string (required) — e.g. "anthropic/claude-sonnet-4-5"
     * messages: array (required) — list of {role, content}
     * systemPrompt: string (optional)
     * sessionKey: string (optional)
     * cwd: string (optional)
     * maxTokens: int (optional, default 4096)
     * temperature: double (optional, default 0.7)
     */
    private CompletableFuture<Object> handleAgentRun(JsonNode params, GatewayConnection conn) {
        String modelId = getTextParam(params, "modelId", null);
        if (modelId == null) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("modelId is required"));
        }

        List<ModelProvider.ChatMessage> messages = parseMessages(params);
        if (messages.isEmpty()) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("messages array is required and must not be empty"));
        }

        OpenClawConfig config = configService.loadConfig();

        // Generate run ID
        String runId = UUID.randomUUID().toString().substring(0, 8);

        AgentRunner.AgentRunContext context = AgentRunner.AgentRunContext.builder()
                .runId(runId)
                .modelId(resolveModel(modelId, config))
                .messages(messages)
                .systemPrompt(getTextParam(params, "systemPrompt", null))
                .sessionKey(getTextParam(params, "sessionKey", null))
                .cwd(getTextParam(params, "cwd", System.getProperty("user.dir")))
                .maxTokens(getIntParam(params, "maxTokens", 4096))
                .temperature(getDoubleParam(params, "temperature", 0.7))
                .config(config)
                .build();

        // Track run in session if sessionKey provided
        String sessionKey = context.getSessionKey();

        if (sessionKey != null) {
            sessionStore.findBySessionKey(sessionKey).ifPresent(session -> {
                sessionStore.startRun(session.getSessionId(), runId);
            });
        }

        return agentRunner.runAsync(context)
                .thenApply(result -> {
                    // End run tracking
                    if (sessionKey != null) {
                        sessionStore.findBySessionKey(sessionKey)
                                .ifPresent(session -> sessionStore.endRun(session.getSessionId()));
                    }
                    return (Object) result;
                });
    }

    /**
     * agent.message — Simplified: send a single user message to the default model.
     *
     * Params:
     * message: string (required) — the user message
     * sessionKey: string (optional)
     * modelId: string (optional, uses config default)
     */
    private CompletableFuture<Object> handleAgentMessage(JsonNode params, GatewayConnection conn) {
        String message = getTextParam(params, "message", null);
        if (message == null || message.isBlank()) {
            return CompletableFuture.failedFuture(
                    new IllegalArgumentException("message is required"));
        }

        OpenClawConfig config = configService.loadConfig();
        String modelId = getTextParam(params, "modelId", null);
        if (modelId == null) {
            modelId = config.getModel() != null ? config.getModel() : "anthropic/claude-sonnet-4-5";
        }

        List<ModelProvider.ChatMessage> messages = List.of(
                ModelProvider.ChatMessage.builder()
                        .role("user")
                        .content(message)
                        .build());

        AgentRunner.AgentRunContext context = AgentRunner.AgentRunContext.builder()
                .modelId(resolveModel(modelId, config))
                .messages(messages)
                .cwd(getTextParam(params, "cwd", System.getProperty("user.dir")))
                .maxTokens(getIntParam(params, "maxTokens", 4096))
                .temperature(getDoubleParam(params, "temperature", 0.7))
                .sessionKey(getTextParam(params, "sessionKey", null))
                .config(config)
                .build();

        return agentRunner.runAsync(context).thenApply(result -> (Object) result);
    }

    // --- Helpers ---

    private String resolveModel(String modelId, OpenClawConfig config) {
        if (config.getModelAliases() != null && config.getModelAliases().containsKey(modelId)) {
            return config.getModelAliases().get(modelId);
        }
        return modelId;
    }

    private List<ModelProvider.ChatMessage> parseMessages(JsonNode params) {
        List<ModelProvider.ChatMessage> messages = new ArrayList<>();
        if (params.has("messages") && params.get("messages").isArray()) {
            for (JsonNode msgNode : params.get("messages")) {
                messages.add(ModelProvider.ChatMessage.builder()
                        .role(msgNode.has("role") ? msgNode.get("role").asText() : "user")
                        .content(msgNode.has("content") ? msgNode.get("content").asText() : "")
                        .build());
            }
        }
        return messages;
    }

    private String getTextParam(JsonNode params, String key, String defaultValue) {
        return params.has(key) && !params.get(key).isNull()
                ? params.get(key).asText()
                : defaultValue;
    }

    private int getIntParam(JsonNode params, String key, int defaultValue) {
        return params.has(key) ? params.get(key).asInt(defaultValue) : defaultValue;
    }

    private double getDoubleParam(JsonNode params, String key, double defaultValue) {
        return params.has(key) ? params.get(key).asDouble(defaultValue) : defaultValue;
    }
}
