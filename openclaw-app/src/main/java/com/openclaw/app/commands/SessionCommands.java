package com.openclaw.app.commands;

import com.openclaw.agent.autoreply.reply.Abort;
import com.openclaw.common.config.AgentDirs;
import com.openclaw.common.config.ConfigPaths;
import com.openclaw.common.config.ConfigRuntimeOverrides;

import com.openclaw.common.config.SessionPaths;
import com.openclaw.common.infra.Restart;
import com.openclaw.common.infra.RestartSentinel;
import com.openclaw.gateway.session.SessionPersistence;
import com.openclaw.gateway.session.TranscriptStore;
import com.openclaw.gateway.session.UsageTracker;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;

/**
 * Session management commands: /clear, /new, /reset, /stop, /usage.
 * Mirrors TypeScript's {@code commands-session.ts}.
 */
@Slf4j
@Component
public class SessionCommands {

    public CommandResult handleClear(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        String agentId = AgentDirs.DEFAULT_AGENT_ID;
        Path storePath = SessionPaths.resolveDefaultSessionStorePath(agentId);
        var store = SessionPersistence.loadSessionStore(storePath);
        var existing = store.get(sessionKey);

        if (existing != null) {
            Path transcriptPath = SessionPaths.resolveSessionTranscriptPath(
                    existing.sessionId(), agentId);
            TranscriptStore.clearTranscript(transcriptPath, existing.sessionId());

            Path usagePath = UsageTracker.resolveUsagePath(transcriptPath);
            try {
                if (Files.exists(usagePath)) {
                    Files.delete(usagePath);
                }
            } catch (Exception e) {
                log.warn("Failed to clear usage file: {}", e.getMessage());
            }

            log.info("Cleared session history: {}", sessionKey);
            return CommandResult.text("✅ 对话历史已清除。新的对话将从头开始。");
        }

        return CommandResult.text("✅ 当前没有对话历史。");
    }

    public CommandResult handleNew(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        String agentId = AgentDirs.DEFAULT_AGENT_ID;
        Path storePath = SessionPaths.resolveDefaultSessionStorePath(agentId);
        var store = SessionPersistence.loadSessionStore(storePath);
        var existing = store.get(sessionKey);

        if (existing != null) {
            Path oldTranscriptPath = SessionPaths.resolveSessionTranscriptPath(
                    existing.sessionId(), agentId);
            TranscriptStore.clearTranscript(oldTranscriptPath, existing.sessionId());
            Path oldUsagePath = UsageTracker.resolveUsagePath(oldTranscriptPath);
            try {
                if (Files.exists(oldUsagePath)) {
                    Files.delete(oldUsagePath);
                }
            } catch (Exception e) {
                log.warn("Failed to clear old usage file: {}", e.getMessage());
            }
        }

        String newSessionId = UUID.randomUUID().toString();
        SessionPersistence.SessionEntry entry = new SessionPersistence.SessionEntry(
                newSessionId, sessionKey,
                SessionPaths.resolveSessionTranscriptPath(newSessionId, agentId).toString(),
                System.getProperty("user.dir"),
                System.currentTimeMillis(), System.currentTimeMillis(),
                null, null);
        SessionPersistence.updateSessionEntry(storePath, sessionKey, entry);

        log.info("Created new session: key={} id={}", sessionKey, newSessionId);
        return CommandResult.text("🆕 新会话已创建。之前的对话历史已清除。");
    }

    public CommandResult handleReset(String args, CommandContext ctx) {
        handleClear(args, ctx);
        ConfigRuntimeOverrides.resetConfigOverrides();
        return CommandResult.text("🔄 会话已重置。对话历史和运行时配置覆盖已清除。");
    }

    public CommandResult handleStop(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        Abort.setAbortMemory(sessionKey, true);
        log.info("Abort flag set for session: {}", sessionKey);
        return CommandResult.text("🛑 已发送停止信号。当前运行将在下一个检查点中止。");
    }

    public CommandResult handleUsage(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        String agentId = AgentDirs.DEFAULT_AGENT_ID;
        Path storePath = SessionPaths.resolveDefaultSessionStorePath(agentId);
        var store = SessionPersistence.loadSessionStore(storePath);
        var existing = store.get(sessionKey);

        if (existing == null) {
            return CommandResult.text("📊 当前尚无用量记录。");
        }

        Path transcriptPath = SessionPaths.resolveSessionTranscriptPath(
                existing.sessionId(), agentId);
        Path usagePath = UsageTracker.resolveUsagePath(transcriptPath);
        UsageTracker.UsageSummary summary = UsageTracker.summarizeUsage(usagePath);

        if (summary.callCount() == 0) {
            return CommandResult.text("📊 当前尚无用量记录。");
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

        return CommandResult.text(sb.toString());
    }

    public CommandResult handleActivate(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        // Parse activation mode: mention or always
        String mode = args.isEmpty() ? null : args.trim().toLowerCase();
        if (mode == null || (!mode.equals("mention") && !mode.equals("always"))) {
            return CommandResult
                    .text("⚙️ 用法: `/activate mention|always`\n\n• `mention` — 仅在 @提及 时响应\n• `always` — 始终响应");
        }
        log.info("Group activation set to {} for session: {}", mode, sessionKey);
        // TODO: Persist to session entry when session entry model supports
        // groupActivation
        return CommandResult.text(String.format("⚙️ 群组激活模式已设置为: %s\n\n🚧 _此功能尚未完全实现_", mode));
    }

    public CommandResult handleDeactivate(String args, CommandContext ctx) {
        log.info("Deactivated for session: {}", ctx.sessionKey());
        return CommandResult.text("⚙️ Bot 已在当前会话中停用。使用 `/activate` 重新启用。\n\n🚧 _此功能尚未完全实现_");
    }

    public CommandResult handleSendPolicy(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        if (args.isEmpty()) {
            return CommandResult
                    .text("⚙️ 用法: `/send on|off|inherit`\n\n• `on` — 允许发送\n• `off` — 禁止发送\n• `inherit` — 继承默认策略");
        }

        String mode = args.trim().toLowerCase();
        return CommandResult.text(switch (mode) {
            case "on", "allow" -> {
                log.info("Send policy set to allow for session: {}", sessionKey);
                // TODO: Persist to session entry
                yield "⚙️ 发送策略已设置为: on\n\n🚧 _此功能尚未完全实现_";
            }
            case "off", "deny" -> {
                log.info("Send policy set to deny for session: {}", sessionKey);
                yield "⚙️ 发送策略已设置为: off\n\n🚧 _此功能尚未完全实现_";
            }
            case "inherit" -> {
                log.info("Send policy set to inherit for session: {}", sessionKey);
                yield "⚙️ 发送策略已设置为: inherit\n\n🚧 _此功能尚未完全实现_";
            }
            default -> "❌ 未知策略: " + mode + "\n用法: `/send on|off|inherit`";
        });
    }

    public CommandResult handleRestart(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        // Check if restart is enabled in config
        var commands = ctx.config().getCommands();
        if (commands == null || !Boolean.TRUE.equals(commands.getRestart())) {
            return CommandResult.text("⚠️ `/restart` 未启用。在配置中设置 `commands.restart: true` 以启用。");
        }

        log.info("Restart requested for session: {}", sessionKey);

        // Authorize restart (opens a 30s window)
        Restart.authorizeRestart();

        // Write restart sentinel for state transfer
        try {
            var stateDir = ConfigPaths.resolveStateDir();
            RestartSentinel.writeSentinel(stateDir, RestartSentinel.Payload.of(
                    "command", "restarting",
                    "Restart triggered by /restart command"));
        } catch (Exception e) {
            log.debug("Failed to write restart sentinel: {}", e.getMessage());
        }

        // Trigger platform-specific restart
        Restart.RestartAttempt attempt = Restart.triggerRestart();

        if (attempt.triggered()) {
            return CommandResult.text(String.format(
                    "⚙️ OpenClaw 正在重启中...\n方式: %s", attempt.method()));
        } else {
            return CommandResult.text(String.format(
                    "⚠️ 自动重启失败 (%s): %s\n请手动重启 OpenClaw。",
                    attempt.method(), attempt.message()));
        }
    }

    public CommandResult handleAbort(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        Abort.setAbortMemory(sessionKey, true);
        log.info("Abort triggered for session: {}", sessionKey);
        return CommandResult.text("⚙️ Agent 已被中止。");
    }
}
