package com.openclaw.app.commands;

import com.openclaw.common.config.AgentDirs;

import com.openclaw.common.config.SessionPaths;
import com.openclaw.common.security.SecurityFix;
import com.openclaw.gateway.session.SessionPersistence;
import com.openclaw.gateway.session.TranscriptStore;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;

/**
 * Tool commands: /compact, /fix.
 * /compact mirrors TypeScript's {@code commands-compact.ts}.
 */
@Slf4j
@Component
public class ToolCommands {

    public CommandResult handleCompact(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        String agentId = AgentDirs.DEFAULT_AGENT_ID;
        Path storePath = SessionPaths.resolveDefaultSessionStorePath(agentId);
        var store = SessionPersistence.loadSessionStore(storePath);
        var existing = store.get(sessionKey);

        if (existing == null) {
            return CommandResult.text("ℹ️ 当前没有活跃的会话，无需压缩。");
        }

        Path transcriptPath = SessionPaths.resolveSessionTranscriptPath(
                existing.sessionId(), agentId);
        List<Map<String, Object>> messages = TranscriptStore.readMessages(transcriptPath, 200);

        if (messages.isEmpty()) {
            return CommandResult.text("ℹ️ 当前会话没有消息，无需压缩。");
        }

        long totalChars = 0;
        for (Map<String, Object> msg : messages) {
            totalChars += CommandUtils.extractTextFromContent(msg.get("content")).length();
        }
        long estimatedTokens = totalChars / 4;

        int keepCount = Math.min(20, messages.size());
        if (messages.size() <= keepCount) {
            return CommandResult.text(String.format("ℹ️ 当前上下文较小 (%d 条消息, ~%,d tokens)，无需压缩。",
                    messages.size(), estimatedTokens));
        }

        TranscriptStore.clearTranscript(transcriptPath, existing.sessionId());
        List<Map<String, Object>> recent = messages.subList(messages.size() - keepCount, messages.size());
        for (Map<String, Object> msg : recent) {
            String role = (String) msg.get("role");
            String text = CommandUtils.extractTextFromContent(msg.get("content"));
            if (!text.isEmpty()) {
                if ("user".equals(role)) {
                    TranscriptStore.appendUserMessage(transcriptPath, existing.sessionId(),
                            text, System.currentTimeMillis());
                } else if ("assistant".equals(role)) {
                    TranscriptStore.appendAssistantMessage(transcriptPath, existing.sessionId(),
                            text, System.currentTimeMillis(), null, null);
                }
            }
        }

        long newChars = 0;
        for (Map<String, Object> msg : recent) {
            newChars += CommandUtils.extractTextFromContent(msg.get("content")).length();
        }
        long newTokens = newChars / 4;
        int removed = messages.size() - keepCount;

        return CommandResult.text(String.format("🗜️ 上下文已压缩\n\n"
                + "📉 移除 %d 条旧消息\n"
                + "📊 保留 %d 条最近消息\n"
                + "🔢 Token 估算: %,d → ~%,d (~%.0f%% 减少)",
                removed, keepCount, estimatedTokens, newTokens,
                estimatedTokens > 0 ? (1.0 - (double) newTokens / estimatedTokens) * 100 : 0));
    }

    public CommandResult handleFix(String args, CommandContext ctx) {
        try {
            SecurityFix.FixResult result = SecurityFix.fixSecurityFootguns();
            return CommandResult.text(SecurityFix.formatResult(result));
        } catch (Exception e) {
            log.error("Security fix failed: {}", e.getMessage(), e);
            return CommandResult.text("❌ 安全修复执行失败: " + e.getMessage());
        }
    }
}
