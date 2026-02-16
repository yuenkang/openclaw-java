package com.openclaw.app.commands;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Plugin command: /plugin.
 * Mirrors TypeScript's {@code commands-plugin.ts}.
 * Routes commands registered by plugins.
 */
@Slf4j
@Component
public class PluginCommands {

    public CommandResult handlePlugin(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        if (args.isEmpty() || "help".equalsIgnoreCase(args)) {
            return CommandResult.text("""
                    🔌 *插件命令*

                    用法:
                    `/plugin list` — 列出已注册的插件
                    `/plugin info <name>` — 查看插件详情
                    `/plugin reload` — 重新加载插件

                    插件可以注册自定义命令，这些命令会在内置命令之前处理。""");
        }

        String[] parts = args.split("\\s+", 2);
        String action = parts[0].toLowerCase();

        return CommandResult.text(switch (action) {
            case "list" -> {
                // TODO: Wire to actual plugin registry when available
                yield "🔌 当前没有已注册的插件。\n\n🚧 _此功能尚未完全实现，需接入插件注册表_";
            }
            case "info" -> {
                if (parts.length < 2) {
                    yield "❌ 用法: `/plugin info <name>`";
                }
                yield String.format("🔌 插件 `%s` 未找到。", parts[1]);
            }
            case "reload" -> {
                log.info("Plugin reload requested by session: {}", sessionKey);
                // TODO: Wire to plugin system reload
                yield "🔌 插件重新加载完成。\n\n🚧 _此功能尚未完全实现_";
            }
            default -> "❌ 未知操作: " + action + "\n用法: /plugin list | info | reload";
        });
    }
}
