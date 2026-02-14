package com.openclaw.app.bridge;

import com.openclaw.agent.autoreply.reply.GetReply;
import com.openclaw.agent.models.ModelProvider;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.agent.runtime.AgentRunner;
import com.openclaw.agent.tools.ToolRegistry;
import com.openclaw.channel.telegram.TelegramBotMessageDispatch;
import com.openclaw.common.config.AgentDirs;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.common.config.SessionPaths;
import com.openclaw.gateway.methods.ChatAgentBridge;
import com.openclaw.gateway.session.SessionPersistence;
import com.openclaw.gateway.session.TranscriptStore;
import com.openclaw.gateway.session.UsageTracker;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
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

        // 4. Wire command handler for /clear, /usage, etc.
        TelegramBotMessageDispatch.setCommandHandler(this::handleCommand);

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
        String modelIdResolved = modelId;
        if (config.getModelAliases() != null && config.getModelAliases().containsKey(modelId)) {
            modelIdResolved = config.getModelAliases().get(modelId);
        }
        if ("default".equals(modelIdResolved)) {
            modelIdResolved = config.getModel() != null ? config.getModel()
                    : "anthropic/claude-sonnet-4-5";
        }
        final String resolvedModelId = modelIdResolved;

        // --- Transcript persistence: derive session ID and load history ---
        String agentId = AgentDirs.DEFAULT_AGENT_ID;
        String sessionId = resolveOrCreateSessionId(sessionKey, agentId);
        Path transcriptPath = SessionPaths.resolveSessionTranscriptPath(sessionId, agentId);

        // Load previous conversation history from transcript file
        List<ModelProvider.ChatMessage> historyMessages = loadTranscriptHistory(transcriptPath);
        log.debug("Loaded {} history messages for session {}", historyMessages.size(), sessionKey);

        AgentRunner.AgentRunContext context = AgentRunner.AgentRunContext.builder()
                .sessionKey(sessionKey)
                .modelId(resolvedModelId)
                .userMessage(userMessage)
                .systemPrompt(systemPrompt)
                .messages(historyMessages)
                .maxTokens(4096)
                .temperature(0.7)
                .config(config)
                .build();

        final long requestTimestamp = System.currentTimeMillis();

        return agentRunner.runAsync(context)
                .thenApply(result -> {
                    String responseText;
                    if (result.isSuccess() && result.getFinalMessage() != null) {
                        responseText = result.getFinalMessage();
                    } else if (result.getError() != null) {
                        responseText = "⚠️ " + result.getError();
                    } else {
                        responseText = "No response generated.";
                    }

                    // --- Transcript persistence: save user message + assistant response ---
                    try {
                        TranscriptStore.appendUserMessage(
                                transcriptPath, sessionId, userMessage, requestTimestamp);
                        TranscriptStore.appendAssistantMessage(
                                transcriptPath, sessionId, responseText,
                                System.currentTimeMillis(),
                                result.getTotalUsage() != null
                                        ? usageToMap(result.getTotalUsage())
                                        : null,
                                result.getReasoningContent());
                        log.debug("Saved transcript for session {}", sessionKey);
                    } catch (Exception e) {
                        log.error("Failed to save transcript for session {}: {}",
                                sessionKey, e.getMessage());
                    }

                    // --- Usage tracking: record token usage ---
                    if (result.getTotalUsage() != null) {
                        try {
                            Path usagePath = UsageTracker.resolveUsagePath(transcriptPath);
                            UsageTracker.recordUsage(usagePath, sessionKey,
                                    resolvedModelId,
                                    result.getTotalUsage().getInputTokens(),
                                    result.getTotalUsage().getOutputTokens(),
                                    result.getTotalUsage().getCacheReadTokens(),
                                    result.getTotalUsage().getCacheWriteTokens());
                        } catch (Exception e) {
                            log.error("Failed to record usage for session {}: {}",
                                    sessionKey, e.getMessage());
                        }
                    }

                    return responseText;
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

    // =========================================================================
    // Transcript persistence helpers
    // =========================================================================

    /**
     * Resolve or create a session ID for the given session key.
     * Checks the persisted sessions.json for an existing mapping;
     * creates a new one if not found.
     */
    private String resolveOrCreateSessionId(String sessionKey, String agentId) {
        Path storePath = SessionPaths.resolveDefaultSessionStorePath(agentId);
        var store = SessionPersistence.loadSessionStore(storePath);
        var existing = store.get(sessionKey);
        if (existing != null) {
            return existing.sessionId();
        }

        // Create new session entry
        String sessionId = UUID.randomUUID().toString();
        SessionPersistence.SessionEntry entry = new SessionPersistence.SessionEntry(
                sessionId, sessionKey,
                SessionPaths.resolveSessionTranscriptPath(sessionId, agentId).toString(),
                System.getProperty("user.dir"),
                System.currentTimeMillis(), System.currentTimeMillis(),
                "telegram", null);
        SessionPersistence.updateSessionEntry(storePath, sessionKey, entry);
        log.info("Created new persistent session: key={} id={}", sessionKey, sessionId);
        return sessionId;
    }

    /**
     * Load conversation history from transcript file and convert to ChatMessages.
     */
    private List<ModelProvider.ChatMessage> loadTranscriptHistory(Path transcriptPath) {
        // Load up to 50 most recent messages for context
        List<Map<String, Object>> rawMessages = TranscriptStore.readMessages(transcriptPath, 50);
        List<ModelProvider.ChatMessage> chatMessages = new ArrayList<>();

        for (Map<String, Object> msg : rawMessages) {
            String role = (String) msg.get("role");
            if (role == null)
                continue;

            String text = "";
            Object content = msg.get("content");
            if (content instanceof List<?> contentList) {
                for (Object item : contentList) {
                    if (item instanceof Map<?, ?> contentItem) {
                        if ("text".equals(contentItem.get("type"))) {
                            text = String.valueOf(contentItem.get("text"));
                            break;
                        }
                    }
                }
            } else if (content instanceof String s) {
                text = s;
            }

            // Read reasoningContent for thinking model history replay
            String reasoningContent = null;
            Object reasoningObj = msg.get("reasoningContent");
            if (reasoningObj instanceof String rc && !rc.isEmpty()) {
                reasoningContent = rc;
            }

            if (!text.isEmpty()) {
                chatMessages.add(ModelProvider.ChatMessage.builder()
                        .role(role)
                        .content(text)
                        .reasoningContent(reasoningContent)
                        .build());
            }
        }

        return chatMessages;
    }

    /**
     * Convert ModelProvider.Usage to a simple map for transcript storage.
     */
    private Map<String, Object> usageToMap(ModelProvider.Usage usage) {
        if (usage == null)
            return null;
        return Map.of(
                "inputTokens", usage.getInputTokens(),
                "outputTokens", usage.getOutputTokens(),
                "totalTokens", usage.getInputTokens() + usage.getOutputTokens());
    }

    // =========================================================================
    // Command handler — /clear, /usage
    // =========================================================================

    /**
     * Handle slash commands. Returns reply text if handled, null to pass through.
     */
    private String handleCommand(String command, String sessionKey, OpenClawConfig config) {
        String cmd = command.trim().toLowerCase();
        // Strip bot username suffix (e.g. /clear@mybotname)
        int atIdx = cmd.indexOf('@');
        if (atIdx > 0) {
            cmd = cmd.substring(0, atIdx);
        }

        return switch (cmd) {
            case "/clear" -> handleClearCommand(sessionKey);
            case "/usage" -> handleUsageCommand(sessionKey);
            case "/help" -> handleHelpCommand();
            default -> null; // Not a known command — pass to LLM
        };
    }

    private String handleClearCommand(String sessionKey) {
        String agentId = AgentDirs.DEFAULT_AGENT_ID;
        Path storePath = SessionPaths.resolveDefaultSessionStorePath(agentId);
        var store = SessionPersistence.loadSessionStore(storePath);
        var existing = store.get(sessionKey);

        if (existing != null) {
            Path transcriptPath = SessionPaths.resolveSessionTranscriptPath(
                    existing.sessionId(), agentId);
            TranscriptStore.clearTranscript(transcriptPath, existing.sessionId());

            // Also clear usage for this session
            Path usagePath = UsageTracker.resolveUsagePath(transcriptPath);
            try {
                if (java.nio.file.Files.exists(usagePath)) {
                    java.nio.file.Files.delete(usagePath);
                }
            } catch (Exception e) {
                log.warn("Failed to clear usage file: {}", e.getMessage());
            }

            log.info("Cleared session history: {}", sessionKey);
            return "✅ 对话历史已清除。新的对话将从头开始。";
        }

        return "✅ 当前没有对话历史。";
    }

    private String handleUsageCommand(String sessionKey) {
        String agentId = AgentDirs.DEFAULT_AGENT_ID;
        Path storePath = SessionPaths.resolveDefaultSessionStorePath(agentId);
        var store = SessionPersistence.loadSessionStore(storePath);
        var existing = store.get(sessionKey);

        if (existing == null) {
            return "📊 当前尚无用量记录。";
        }

        Path transcriptPath = SessionPaths.resolveSessionTranscriptPath(
                existing.sessionId(), agentId);
        Path usagePath = UsageTracker.resolveUsagePath(transcriptPath);
        UsageTracker.UsageSummary summary = UsageTracker.summarizeUsage(usagePath);

        if (summary.callCount() == 0) {
            return "📊 当前尚无用量记录。";
        }

        int msgCount = TranscriptStore.countMessages(transcriptPath);

        StringBuilder sb = new StringBuilder();
        sb.append("📊 *用量统计*\n\n");
        sb.append(String.format("💬 对话轮数: %d\n", msgCount / 2));
        sb.append(String.format("🤖 LLM 调用: %d 次\n", summary.callCount()));
        sb.append(String.format("📥 输入 tokens: %,d\n", summary.totalInputTokens()));
        sb.append(String.format("📤 输出 tokens: %,d\n", summary.totalOutputTokens()));
        if (summary.totalCacheReadTokens() > 0) {
            sb.append(String.format("♻️ 缓存读取: %,d\n", summary.totalCacheReadTokens()));
        }
        sb.append(String.format("📈 总 tokens: %,d\n", summary.totalTokens()));
        sb.append(String.format("💰 估算成本: $%.4f\n", summary.totalEstimatedCost()));
        if (summary.lastModel() != null) {
            sb.append(String.format("🏷️ 模型: %s", summary.lastModel()));
        }

        return sb.toString();
    }

    private String handleHelpCommand() {
        return """
                🤖 *可用命令*

                /clear - 清除当前对话历史
                /usage - 查看用量统计
                /help - 显示此帮助信息

                其他消息将直接与 AI 对话。""";
    }
}
