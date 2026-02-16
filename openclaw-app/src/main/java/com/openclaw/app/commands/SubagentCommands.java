package com.openclaw.app.commands;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Subagent commands: /subagents.
 * Mirrors TypeScript's {@code commands-subagents.ts}.
 * Manages spawned sub-agents (list, stop, log, info).
 */
@Slf4j
@Component
public class SubagentCommands {

    public CommandResult handleSubagents(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        if (args.isEmpty() || "help".equalsIgnoreCase(args)) {
            return CommandResult.text("""
                    🤖 *子 Agent 管理*

                    用法:
                    `/subagents list` — 列出运行中的子 agent
                    `/subagents stop [id]` — 停止子 agent
                    `/subagents log [id]` — 查看子 agent 日志
                    `/subagents info [id]` — 查看子 agent 详情
                    `/subagents send <id> <message>` — 向子 agent 发送消息

                    子 agent 由主 agent 在对话过程中自动创建。""");
        }

        String[] parts = args.split("\\s+", 2);
        String action = parts[0].toLowerCase();

        return CommandResult.text(switch (action) {
            case "list" -> {
                // TODO: Wire to SubagentRegistry when available
                yield "🤖 当前没有运行中的子 agent。\n\n🚧 _此功能尚未完全实现，需接入 SubagentRegistry_";
            }
            case "stop" -> {
                if (parts.length < 2) {
                    yield "❌ 用法: `/subagents stop <id>`";
                }
                String targetId = parts[1].trim();
                log.info("Subagent stop requested: id={}, session={}", targetId, sessionKey);
                // TODO: Wire to SubagentRegistry
                yield String.format("🛑 已发送停止信号给子 agent `%s`。\n\n🚧 _此功能尚未完全实现_", targetId);
            }
            case "log" -> {
                if (parts.length < 2) {
                    yield "❌ 用法: `/subagents log <id>`";
                }
                String targetId = parts[1].trim();
                // TODO: Wire to subagent session log reading
                yield String.format("📋 子 agent `%s` 的日志暂不可用。\n\n🚧 _此功能尚未完全实现_", targetId);
            }
            case "info" -> {
                if (parts.length < 2) {
                    yield "❌ 用法: `/subagents info <id>`";
                }
                String targetId = parts[1].trim();
                // TODO: Wire to SubagentRegistry
                yield String.format("🤖 子 agent `%s` 的详情暂不可用。\n\n🚧 _此功能尚未完全实现_", targetId);
            }
            case "send" -> {
                String[] sendParts = (parts.length > 1 ? parts[1] : "").split("\\s+", 2);
                if (sendParts.length < 2) {
                    yield "❌ 用法: `/subagents send <id> <message>`";
                }
                String targetId = sendParts[0].trim();
                // sendParts[1] contains the message to send
                log.info("Subagent message: id={}, session={}", targetId, sessionKey);
                // TODO: Wire to SubagentRegistry
                yield String.format("📨 消息已发送给子 agent `%s`。\n\n🚧 _此功能尚未完全实现_", targetId);
            }
            default -> "❌ 未知操作: " + action + "\n用法: /subagents list | stop | log | info | send";
        });
    }
}
