package com.openclaw.app.bridge;

import com.openclaw.app.commands.CommandProcessor;
import com.openclaw.agent.autoreply.reply.GetReply;
import com.openclaw.agent.models.ModelProvider;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.agent.runtime.AgentRunner;
import com.openclaw.agent.tools.ToolRegistry;
import com.openclaw.agent.tools.builtin.TelegramImageTool;
import com.openclaw.channel.telegram.TelegramBotMessageDispatch;
import com.openclaw.channel.telegram.TelegramDownload;
import com.openclaw.channel.telegram.TelegramSend;
import com.openclaw.channel.telegram.TelegramToken;
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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
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
    private final ToolRegistry toolRegistry;
    private final ConfigService configService;
    private final ModelProviderRegistry modelProviderRegistry;
    private final CommandProcessor commandProcessor;

    public TelegramAgentWiring(
            ChatAgentBridge chatAgentBridge,
            ModelProviderRegistry modelProviderRegistry,
            ToolRegistry toolRegistry,
            ConfigService configService,
            CommandProcessor commandProcessor) {
        this.chatAgentBridge = chatAgentBridge;
        this.agentRunner = new AgentRunner(modelProviderRegistry, toolRegistry);
        this.toolRegistry = toolRegistry;
        this.configService = configService;
        this.modelProviderRegistry = modelProviderRegistry;
        this.commandProcessor = commandProcessor;
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

        // 5. Wire callback query handler for inline button presses (pagination)
        TelegramBotMessageDispatch.setCallbackQueryHandler(this::handleCallbackQuery);

        // 6. Wire send_image tool — resolve bot token and inject image sender
        wireTelegramImageSender();

        log.info("Telegram auto-reply pipeline wired successfully");
    }

    /**
     * Resolve the Telegram bot token and wire TelegramImageTool.ImageSender.
     */
    @SuppressWarnings("unchecked")
    private void wireTelegramImageSender() {
        try {
            OpenClawConfig config = configService.loadConfig();
            Map<String, Object> channels = config.getChannels() != null
                    ? config.getChannels().getProviders()
                    : null;
            TelegramToken.Resolution tokenRes = TelegramToken.resolve(channels, "default");

            if (tokenRes.token() != null && !tokenRes.token().isBlank()) {
                final String botToken = tokenRes.token();
                TelegramImageTool.setImageSender((chatId, imageBytes, fileName, caption) -> {
                    TelegramSend.sendPhoto(botToken, chatId, imageBytes, fileName,
                            caption, null, null);
                });
                // Register the tool into the shared ToolRegistry
                toolRegistry.register(new TelegramImageTool());
                log.info("TelegramImageTool wired (token source: {})", tokenRes.source());
            } else {
                log.warn("No Telegram bot token found — send_image tool will be unavailable");
            }
        } catch (Exception e) {
            log.warn("Failed to wire TelegramImageTool: {}", e.getMessage());
        }
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
            OpenClawConfig config,
            Map<String, String> mediaInfo) {

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

        // --- Build image content parts if media is attached ---
        List<ModelProvider.ContentPart> imageContentParts = null;
        if (mediaInfo != null && mediaInfo.get("mediaType") != null
                && mediaInfo.get("mediaType").startsWith("image/")) {
            imageContentParts = buildImageContentParts(
                    userMessage, mediaInfo.get("mediaFileId"),
                    mediaInfo.get("mediaType"), mediaInfo.get("botToken"));
        }

        AgentRunner.AgentRunContext.AgentRunContextBuilder ctxBuilder = AgentRunner.AgentRunContext.builder()
                .sessionKey(sessionKey)
                .modelId(resolvedModelId)
                .userMessage(userMessage != null && !userMessage.isBlank() ? userMessage : "请描述这张图片")
                .systemPrompt(systemPrompt)
                .messages(historyMessages)
                .maxTokens(4096)
                .temperature(0.7)
                .config(config);

        if (imageContentParts != null) {
            ctxBuilder.imageContentParts(imageContentParts);
        }

        AgentRunner.AgentRunContext context = ctxBuilder.build();

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
    // Command handler — delegates to channel-agnostic CommandProcessor
    // =========================================================================

    /**
     * Handle slash commands. Returns CommandHandlerResult if handled, null to pass
     * through.
     * Bridges app-layer CommandResult → channel-layer CommandHandlerResult.
     */
    private TelegramBotMessageDispatch.CommandHandlerResult handleCommand(
            String command, String sessionKey, String senderId, OpenClawConfig config) {
        var result = commandProcessor.handleCommand(command, sessionKey, senderId, config);
        if (result == null)
            return null;
        return toHandlerResult(result);
    }

    /**
     * Handle callback query (inline button press).
     * Currently supports commands_page_N for /commands pagination.
     */
    private TelegramBotMessageDispatch.CommandHandlerResult handleCallbackQuery(
            String callbackData, OpenClawConfig config) {
        if (callbackData == null)
            return null;

        // Handle commands pagination: commands_page_N
        if (callbackData.startsWith("commands_page_")) {
            String suffix = callbackData.substring("commands_page_".length());
            if ("noop".equals(suffix)) {
                return null; // Page counter button — do nothing
            }
            try {
                int page = Integer.parseInt(suffix);
                var result = commandProcessor.getInfoCommands().handleCommandsPage(page);
                if (result != null) {
                    return toHandlerResult(result);
                }
            } catch (NumberFormatException e) {
                log.debug("Invalid page number in callback data: {}", callbackData);
            }
        }

        return null;
    }

    /**
     * Convert app-layer CommandResult to channel-layer CommandHandlerResult.
     */
    private static TelegramBotMessageDispatch.CommandHandlerResult toHandlerResult(
            com.openclaw.app.commands.CommandResult result) {
        List<List<TelegramSend.InlineButton>> buttons = null;
        if (result.hasButtons()) {
            buttons = new java.util.ArrayList<>();
            for (var row : result.buttons()) {
                var btnRow = new java.util.ArrayList<TelegramSend.InlineButton>();
                for (var btn : row) {
                    btnRow.add(new TelegramSend.InlineButton(btn.text(), btn.callbackData()));
                }
                buttons.add(btnRow);
            }
        }
        return new TelegramBotMessageDispatch.CommandHandlerResult(result.text(), buttons);
    }

    // =========================================================================
    // Image content part builder
    // =========================================================================

    /**
     * Download an image from Telegram and build multimodal ContentParts.
     * Returns a list containing a text part (user message) and an image_url part
     * (base64).
     */
    private List<ModelProvider.ContentPart> buildImageContentParts(
            String userMessage, String fileId, String contentType, String botToken) {
        if (botToken == null || fileId == null) {
            log.warn("Cannot download image: missing botToken or fileId");
            return null;
        }

        try {
            // 1. Get file info from Telegram
            TelegramDownload.TelegramFileInfo fileInfo = TelegramDownload.getTelegramFile(botToken, fileId);
            if (fileInfo == null) {
                log.warn("Failed to get file info for fileId: {}", fileId);
                return null;
            }

            // 2. Download the file (max 10MB for images)
            TelegramDownload.SavedMedia savedMedia = TelegramDownload.downloadTelegramFile(botToken, fileInfo,
                    10_000_000L, null);

            // 3. Read file and encode to base64
            byte[] imageBytes = Files.readAllBytes(Path.of(savedMedia.getPath()));
            String base64Data = Base64.getEncoder().encodeToString(imageBytes);
            // Determine MIME type: prefer Telegram API's contentType (from
            // extractMediaRef),
            // then HTTP download header, then file extension detection
            String mediaType = contentType; // from Telegram API — most accurate
            if (mediaType == null || !mediaType.startsWith("image/")) {
                mediaType = savedMedia.getContentType(); // HTTP Content-Type header
            }
            if (mediaType == null || !mediaType.startsWith("image/")) {
                mediaType = detectImageMimeType(savedMedia.getPath()); // file extension
            }

            String dataUri = "data:" + mediaType + ";base64," + base64Data;

            log.info("Downloaded image: {} bytes, type={}", imageBytes.length, mediaType);

            // 4. Build content parts
            List<ModelProvider.ContentPart> parts = new ArrayList<>();

            // Image part first
            parts.add(ModelProvider.ContentPart.builder()
                    .type("image_url")
                    .imageUrl(ModelProvider.ImageUrl.builder().url(dataUri).build())
                    .build());

            // Text part (caption or default prompt)
            String text = (userMessage != null && !userMessage.isBlank())
                    ? userMessage
                    : "请描述这张图片";
            parts.add(ModelProvider.ContentPart.builder()
                    .type("text")
                    .text(text)
                    .build());

            // 5. Clean up downloaded file
            try {
                Files.deleteIfExists(Path.of(savedMedia.getPath()));
            } catch (IOException ignored) {
                // Non-critical cleanup failure
            }

            return parts;

        } catch (Exception e) {
            log.error("Failed to build image content parts: {}", e.getMessage(), e);
            return null;
        }
    }

    /**
     * Detect image MIME type from file path extension.
     * Falls back to image/jpeg (the most common Telegram photo format).
     */
    private static String detectImageMimeType(String filePath) {
        if (filePath != null) {
            String lower = filePath.toLowerCase();
            if (lower.endsWith(".png"))
                return "image/png";
            if (lower.endsWith(".gif"))
                return "image/gif";
            if (lower.endsWith(".webp"))
                return "image/webp";
            if (lower.endsWith(".jpg") || lower.endsWith(".jpeg"))
                return "image/jpeg";
        }
        return "image/jpeg"; // Telegram photos are typically JPEG
    }
}
