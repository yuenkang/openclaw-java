package com.openclaw.app.bridge;

import com.openclaw.agent.models.ModelProvider;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.agent.runtime.AgentRunner;
import com.openclaw.agent.runtime.AgentRunner.AgentEventListener;
import com.openclaw.agent.tools.ToolRegistry;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.gateway.methods.ChatAgentBridge;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Implements ChatAgentBridge, bridging the gateway's ChatMethodHandler
 * to the agent module's AgentRunner.
 *
 * <p>
 * Lives in the app module because it needs access to both gateway and agent
 * classes.
 * </p>
 */
@Slf4j
@Component
public class ChatAgentBridgeImpl implements ChatAgentBridge {

    private final AgentRunner agentRunner;
    private final ConfigService configService;

    public ChatAgentBridgeImpl(
            ModelProviderRegistry modelProviderRegistry,
            ToolRegistry toolRegistry,
            ConfigService configService) {
        this.agentRunner = new AgentRunner(modelProviderRegistry, toolRegistry);
        this.configService = configService;
    }

    @Override
    public CompletableFuture<ChatRunResult> runChat(ChatRunRequest request) {
        OpenClawConfig config = configService.loadConfig();

        // Resolve model alias
        String modelId = request.modelId();
        if (config.getModelAliases() != null && config.getModelAliases().containsKey(modelId)) {
            modelId = config.getModelAliases().get(modelId);
        }
        if ("default".equals(modelId)) {
            modelId = config.getModel() != null ? config.getModel() : "anthropic/claude-sonnet-4-5";
        }

        // Convert simple messages to ModelProvider.ChatMessage
        List<ModelProvider.ChatMessage> messages = new ArrayList<>();
        for (Map<String, String> msg : request.messages()) {
            messages.add(ModelProvider.ChatMessage.builder()
                    .role(msg.getOrDefault("role", "user"))
                    .content(msg.getOrDefault("content", ""))
                    .build());
        }

        // Wire up event listener bridge
        AgentEventListener agentListener = new AgentEventListener() {
            @Override
            public void onDelta(String text) {
                if (request.listener() != null)
                    request.listener().onDelta(text);
            }

            @Override
            public void onToolStart(String toolName, String toolId) {
                if (request.listener() != null)
                    request.listener().onToolStart(toolName, toolId);
            }

            @Override
            public void onToolEnd(String toolName, String toolId, String result, boolean success) {
                if (request.listener() != null)
                    request.listener().onToolEnd(toolName, toolId, result, success);
            }

            @Override
            public void onComplete(String finalMessage) {
                if (request.listener() != null)
                    request.listener().onComplete(finalMessage);
            }

            @Override
            public void onError(String error) {
                if (request.listener() != null)
                    request.listener().onError(error);
            }
        };

        AgentRunner.AgentRunContext context = AgentRunner.AgentRunContext.builder()
                .sessionKey(request.sessionKey())
                .modelId(modelId)
                .messages(messages)
                .systemPrompt(request.systemPrompt())
                .cwd(request.cwd())
                .maxTokens(request.maxTokens())
                .temperature(request.temperature())
                .config(config)
                .listener(agentListener)
                .build();

        return agentRunner.runAsync(context)
                .thenApply(result -> new ChatRunResult(
                        result.isSuccess(),
                        result.getFinalMessage(),
                        result.getError()));
    }
}
