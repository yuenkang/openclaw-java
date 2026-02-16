package com.openclaw.app.commands;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Channel-agnostic command dispatcher.
 * Registers all command handlers and routes incoming commands to the
 * appropriate handler.
 * <p>
 * Mirrors TypeScript's {@code commands-core.ts} — a thin dispatcher that
 * delegates
 * to individual command modules ({@code commands-info.ts},
 * {@code commands-session.ts}, etc.).
 * </p>
 */
@Slf4j
@Component
public class CommandProcessor {

    private final Map<String, CommandHandler> handlers = new LinkedHashMap<>();
    private final InfoCommands infoCommands;
    private final ConfigService configService;

    public CommandProcessor(
            ConfigService configService,
            InfoCommands infoCommands,
            SessionCommands sessionCommands,
            ConfigCommands configCommands,
            ModelCommands modelCommands,
            ToolCommands toolCommands,
            BashCommands bashCommands,
            ApproveCommands approveCommands,
            AllowlistCommands allowlistCommands,
            PluginCommands pluginCommands,
            SubagentCommands subagentCommands,
            TtsCommands ttsCommands) {

        this.configService = configService;
        this.infoCommands = infoCommands;

        // Info commands — commands-info.ts
        handlers.put("help", infoCommands::handleHelp);
        handlers.put("status", infoCommands::handleStatus);
        handlers.put("commands", infoCommands::handleCommands);
        handlers.put("whoami", infoCommands::handleWhoami);
        handlers.put("context", infoCommands::handleContext);

        // Session commands — commands-session.ts
        handlers.put("clear", sessionCommands::handleClear);
        handlers.put("new", sessionCommands::handleNew);
        handlers.put("reset", sessionCommands::handleReset);
        handlers.put("stop", sessionCommands::handleStop);
        handlers.put("usage", sessionCommands::handleUsage);
        handlers.put("activate", sessionCommands::handleActivate);
        handlers.put("deactivate", sessionCommands::handleDeactivate);
        handlers.put("send", sessionCommands::handleSendPolicy);
        handlers.put("sendpolicy", sessionCommands::handleSendPolicy);
        handlers.put("restart", sessionCommands::handleRestart);
        handlers.put("abort", sessionCommands::handleAbort);

        // Config commands — commands-config.ts
        handlers.put("config", configCommands::handleConfig);
        handlers.put("debug", configCommands::handleDebug);

        // Model commands — commands-models.ts
        handlers.put("models", modelCommands::handleModels);
        handlers.put("model", modelCommands::handleModel);

        // Tool commands — commands-compact.ts + security
        handlers.put("compact", toolCommands::handleCompact);
        handlers.put("fix", toolCommands::handleFix);

        // Bash commands — commands-bash.ts
        handlers.put("bash", bashCommands::handleBash);

        // Approve commands — commands-approve.ts
        handlers.put("approve", approveCommands::handleApprove);

        // Allowlist commands — commands-allowlist.ts
        handlers.put("allowlist", allowlistCommands::handleAllowlist);

        // Plugin commands — commands-plugin.ts
        handlers.put("plugin", pluginCommands::handlePlugin);

        // Subagent commands — commands-subagents.ts
        handlers.put("subagents", subagentCommands::handleSubagents);

        // TTS commands — commands-tts.ts
        handlers.put("tts", ttsCommands::handleTts);
    }

    /** Get the InfoCommands instance for direct access (e.g. pagination). */
    public InfoCommands getInfoCommands() {
        return infoCommands;
    }

    /**
     * Handle a slash command. Returns reply text if handled, null to pass through.
     *
     * @param command    the full command text (e.g. "/config set model gpt-4")
     * @param sessionKey the session key
     * @param senderId   the sender's ID (e.g. Telegram user ID), nullable
     * @param config     the current config
     * @return CommandResult with reply text and optional buttons, or null if the
     *         command is unknown
     */
    public CommandResult handleCommand(String command, String sessionKey, @Nullable String senderId,
            OpenClawConfig config) {
        if (command == null || command.isBlank()) {
            return null;
        }

        String trimmed = command.trim();
        if (!trimmed.startsWith("/")) {
            return null;
        }

        // Parse: "/cmd args..." → name="cmd", args="args..."
        String withoutSlash = trimmed.substring(1);
        int spaceIdx = withoutSlash.indexOf(' ');
        String name;
        String args;
        if (spaceIdx < 0) {
            name = withoutSlash.toLowerCase();
            args = "";
        } else {
            name = withoutSlash.substring(0, spaceIdx).toLowerCase();
            args = withoutSlash.substring(spaceIdx + 1).trim();
        }

        CommandHandler handler = handlers.get(name);
        if (handler == null) {
            log.debug("Unknown command: /{}", name);
            return null;
        }

        try {
            boolean isAuthorized = CommandAuthorization.isAuthorizedSender(senderId, config, null);
            var ctx = new CommandContext(sessionKey, senderId, config, isAuthorized, configService);
            return handler.handle(args, ctx);
        } catch (Exception e) {
            log.error("Command /{} failed: {}", name, e.getMessage(), e);
            return CommandResult.text("❌ 命令执行失败: " + e.getMessage());
        }
    }
}
