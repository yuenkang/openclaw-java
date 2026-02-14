package com.openclaw.app.bridge;

import com.openclaw.agent.autoreply.reply.GetReply;
import com.openclaw.agent.models.ModelProvider;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.agent.runtime.AgentRunner;
import com.openclaw.agent.tools.ToolRegistry;
import com.openclaw.channel.telegram.TelegramBotMessageDispatch;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.gateway.methods.ChatAgentBridge;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Wires the Telegram channel's ReplyPipeline to the auto-reply pipeline
 * (GetReply.getReplyFromConfig) and injects the ChatRunner (wrapping
 * AgentRunner) into the auto-reply system.
 *
 * <p>
 * This lives in openclaw-app because it bridges channel ↔ gateway ↔ agent
 * modules.
 * </p>
 */
@Slf4j
@Component
public class TelegramAgentWiring {

    private final ChatAgentBridge chatAgentBridge;
    private final AgentRunner agentRunner;
    private final ConfigService configService;

    public TelegramAgentWiring(
            ChatAgentBridge chatAgentBridge,
            ModelProviderRegistry modelProviderRegistry,
            ToolRegistry toolRegistry,
            ConfigService configService) {
        this.chatAgentBridge = chatAgentBridge;
        this.agentRunner = new AgentRunner(modelProviderRegistry, toolRegistry);
        this.configService = configService;
    }

    @PostConstruct
    public void wire() {
        log.info("Wiring Telegram auto-reply pipeline");

        // 1. Inject ChatRunner into GetReply (so getReplyFromConfig can invoke the LLM)
        GetReply.setChatRunner(this::runChat);

        // 2. Wire ReplyPipeline: dispatch → GetReply.getReplyFromConfig
        TelegramBotMessageDispatch.setReplyPipeline(this::getReply);

        // 3. Keep legacy AgentInvoker as fallback
        TelegramBotMessageDispatch.setAgentInvoker(this::invokeAgentLegacy);

        log.info("Telegram auto-reply pipeline wired successfully");
    }

    // =========================================================================
    // ReplyPipeline implementation → GetReply.getReplyFromConfig
    // =========================================================================

    private CompletableFuture<List<Map<String, Object>>> getReply(
            Map<String, Object> msgCtx,
            Map<String, Object> opts,
            OpenClawConfig config) {
        return GetReply.getReplyFromConfig(msgCtx, opts, config);
    }

    // =========================================================================
    // ChatRunner implementation → AgentRunner
    // =========================================================================

    private CompletableFuture<String> runChat(
            String sessionKey,
            String userMessage,
            String systemPrompt,
            String modelId,
            OpenClawConfig config) {

        // Resolve model alias
        String resolvedModelId = modelId;
        if (config.getModelAliases() != null && config.getModelAliases().containsKey(modelId)) {
            resolvedModelId = config.getModelAliases().get(modelId);
        }
        if ("default".equals(resolvedModelId)) {
            resolvedModelId = config.getModel() != null ? config.getModel()
                    : "anthropic/claude-sonnet-4-5";
        }

        AgentRunner.AgentRunContext context = AgentRunner.AgentRunContext.builder()
                .sessionKey(sessionKey)
                .modelId(resolvedModelId)
                .userMessage(userMessage)
                .systemPrompt(systemPrompt)
                .maxTokens(4096)
                .temperature(0.7)
                .config(config)
                .build();

        return agentRunner.runAsync(context)
                .thenApply(result -> {
                    if (result.isSuccess() && result.getFinalMessage() != null) {
                        return result.getFinalMessage();
                    }
                    if (result.getError() != null) {
                        return "⚠️ " + result.getError();
                    }
                    return "No response generated.";
                });
    }

    // =========================================================================
    // Legacy AgentInvoker fallback
    // =========================================================================

    private CompletableFuture<String> invokeAgentLegacy(
            String sessionKey, String userText, OpenClawConfig config) {

        String modelId = "default";
        if (config.getModel() != null && !config.getModel().isBlank()) {
            modelId = config.getModel();
        }

        ChatAgentBridge.ChatRunRequest request = ChatAgentBridge.ChatRunRequest.builder()
                .sessionKey(sessionKey)
                .modelId(modelId)
                .messages(List.of(Map.of("role", "user", "content", userText)))
                .maxTokens(4096)
                .temperature(0.7)
                .build();

        return chatAgentBridge.runChat(request)
                .thenApply(result -> {
                    if (result.success() && result.finalMessage() != null) {
                        return result.finalMessage();
                    }
                    if (result.error() != null) {
                        return "Error: " + result.error();
                    }
                    return "No response generated.";
                });
    }
}
