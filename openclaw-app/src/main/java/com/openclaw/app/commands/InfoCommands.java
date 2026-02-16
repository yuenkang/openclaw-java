package com.openclaw.app.commands;

import com.openclaw.agent.autoreply.CommandsRegistryData;
import com.openclaw.agent.autoreply.Status;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.common.config.AgentDirs;
import com.openclaw.common.config.ConfigRuntimeOverrides;
import com.openclaw.common.config.SessionPaths;
import com.openclaw.gateway.session.SessionPersistence;
import com.openclaw.gateway.session.TranscriptStore;
import com.openclaw.gateway.session.UsageTracker;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Info commands: /help, /status, /commands, /whoami, /context.
 * Mirrors TypeScript's {@code commands-info.ts}.
 */
@Slf4j
@Component
public class InfoCommands {

    private final ModelProviderRegistry modelProviderRegistry;

    public InfoCommands(ModelProviderRegistry modelProviderRegistry) {
        this.modelProviderRegistry = modelProviderRegistry;
    }

    public CommandResult handleHelp(String args, CommandContext ctx) {
        return CommandResult.text("""
                🤖 *可用命令*

                *信息*
                /help — 显示此帮助信息
                /status — 查看当前状态（模型、会话、用量）
                /commands — 列出所有可用命令
                /whoami — 显示会话信息
                /context — 查看当前上下文详情

                *会话管理*
                /clear — 清除当前对话历史
                /new — 创建新会话
                /reset — 重置会话（含运行时配置）
                /compact — 压缩上下文（减少 token 消耗）
                /stop — 停止当前运行

                *配置*
                /config [show|set|unset|get|reset] — 管理运行时配置
                /debug [show|set|unset|reset] — 管理 debug 配置
                /model [model-id] — 查看或切换模型
                /models [provider] — 列出可用模型

                *工具*
                /usage — 查看用量统计
                /fix — 修复常见安全问题

                其他消息将直接与 AI 对话。""");
    }

    public CommandResult handleStatus(String args, CommandContext ctx) {
        var config = ctx.config();
        var sessionKey = ctx.sessionKey();
        StringBuilder sb = new StringBuilder();
        sb.append("📋 *状态信息*\n\n");

        // Model info
        String modelId = config.getModel() != null ? config.getModel() : "default";
        if (config.getModelAliases() != null && config.getModelAliases().containsKey(modelId)) {
            sb.append(String.format("🏷️ 模型: %s → %s\n", modelId, config.getModelAliases().get(modelId)));
        } else {
            sb.append(String.format("🏷️ 模型: %s\n", modelId));
        }

        // Provider info
        var providerIds = modelProviderRegistry.getProviderIds();
        sb.append(String.format("🔌 已注册 Provider: %s\n", String.join(", ", providerIds)));

        // Session info
        String agentId = AgentDirs.DEFAULT_AGENT_ID;
        Path storePath = SessionPaths.resolveDefaultSessionStorePath(agentId);
        var store = SessionPersistence.loadSessionStore(storePath);
        var existing = store.get(sessionKey);

        if (existing != null) {
            Path transcriptPath = SessionPaths.resolveSessionTranscriptPath(
                    existing.sessionId(), agentId);
            int msgCount = TranscriptStore.countMessages(transcriptPath);
            sb.append(String.format("💬 会话消息数: %d\n", msgCount));
            sb.append(String.format("🔑 会话 ID: %s\n", existing.sessionId().substring(0, 8) + "..."));

            Path usagePath = UsageTracker.resolveUsagePath(transcriptPath);
            UsageTracker.UsageSummary summary = UsageTracker.summarizeUsage(usagePath);
            if (summary.callCount() > 0) {
                sb.append(String.format("📈 Token 用量: %,d (输入 %,d + 输出 %,d)\n",
                        summary.totalTokens(), summary.totalInputTokens(), summary.totalOutputTokens()));
                sb.append(String.format("💰 估算成本: $%.4f\n", summary.totalEstimatedCost()));
            }
        } else {
            sb.append("💬 会话: 未初始化\n");
        }

        var overrides = ConfigRuntimeOverrides.getConfigOverrides();
        if (!overrides.isEmpty()) {
            sb.append(String.format("⚙️ 运行时覆盖: %d 项\n", overrides.size()));
        }

        return CommandResult.text(sb.toString());
    }

    /**
     * Handle /commands — returns first page with pagination buttons for Telegram.
     */
    public CommandResult handleCommands(String args, CommandContext ctx) {
        return handleCommandsPage(1);
    }

    /**
     * Handle /commands for a specific page.
     * Called directly for page 1 (/commands), or via callback query for other
     * pages.
     */
    public CommandResult handleCommandsPage(int page) {
        try {
            var result = Status.buildCommandsMessagePaginated(
                    Map.of(), List.of(), page, "telegram");
            if (result != null && !result.text().isBlank()) {
                if (result.totalPages() > 1) {
                    var buttons = buildPaginationKeyboard(
                            result.currentPage(), result.totalPages());
                    return CommandResult.withButtons(result.text(), buttons);
                }
                return CommandResult.text(result.text());
            }
        } catch (Exception e) {
            log.debug("Status.buildCommandsMessagePaginated failed: {}", e.getMessage());
        }

        // Fallback: plain list without pagination
        var commands = CommandsRegistryData.getChatCommands();
        StringBuilder sb = new StringBuilder();
        sb.append("📜 *可用命令列表*\n\n");
        for (var cmd : commands) {
            String name = cmd.nativeName() != null ? "/" + cmd.nativeName()
                    : cmd.textAliases().isEmpty() ? cmd.key() : cmd.textAliases().get(0);
            sb.append(String.format("%s — %s\n", name, cmd.description()));
        }
        return CommandResult.text(sb.toString());
    }

    /**
     * Build Telegram inline keyboard for commands pagination.
     * Layout: [◀ Prev] [1/4] [Next ▶]
     */
    public static List<List<CommandResult.InlineButton>> buildPaginationKeyboard(
            int currentPage, int totalPages) {
        List<CommandResult.InlineButton> row = new ArrayList<>();

        if (currentPage > 1) {
            row.add(new CommandResult.InlineButton(
                    "◀ Prev", "commands_page_" + (currentPage - 1)));
        }

        row.add(new CommandResult.InlineButton(
                currentPage + "/" + totalPages, "commands_page_noop"));

        if (currentPage < totalPages) {
            row.add(new CommandResult.InlineButton(
                    "Next ▶", "commands_page_" + (currentPage + 1)));
        }

        return List.of(row);
    }

    public CommandResult handleWhoami(String args, CommandContext ctx) {
        return CommandResult.text(String.format("🔑 *你的会话信息*\n\n会话 Key: `%s`", ctx.sessionKey()));
    }

    public CommandResult handleContext(String args, CommandContext ctx) {
        String agentId = AgentDirs.DEFAULT_AGENT_ID;
        Path storePath = SessionPaths.resolveDefaultSessionStorePath(agentId);
        var store = SessionPersistence.loadSessionStore(storePath);
        var existing = store.get(ctx.sessionKey());

        StringBuilder sb = new StringBuilder();
        sb.append("📝 *上下文信息*\n\n");

        if (existing == null) {
            sb.append("当前没有活跃的会话。发送任意消息开始对话。");
            return CommandResult.text(sb.toString());
        }

        Path transcriptPath = SessionPaths.resolveSessionTranscriptPath(
                existing.sessionId(), agentId);
        int msgCount = TranscriptStore.countMessages(transcriptPath);
        List<Map<String, Object>> messages = TranscriptStore.readMessages(transcriptPath, 100);

        long totalChars = 0;
        int userMsgCount = 0;
        int assistantMsgCount = 0;
        for (Map<String, Object> msg : messages) {
            String role = (String) msg.get("role");
            Object content = msg.get("content");
            String text = CommandUtils.extractTextFromContent(content);
            totalChars += text.length();
            if ("user".equals(role))
                userMsgCount++;
            else if ("assistant".equals(role))
                assistantMsgCount++;
        }
        long estimatedTokens = totalChars / 4;

        sb.append(String.format("💬 总消息数: %d\n", msgCount));
        sb.append(String.format("   👤 用户消息: %d\n", userMsgCount));
        sb.append(String.format("   🤖 助手消息: %d\n", assistantMsgCount));
        sb.append(String.format("📊 上下文大小: ~%,d 字符 (~%,d tokens)\n", totalChars, estimatedTokens));
        sb.append(String.format("🔑 会话 ID: %s\n", existing.sessionId().substring(0, 8) + "..."));

        return CommandResult.text(sb.toString());
    }
}
