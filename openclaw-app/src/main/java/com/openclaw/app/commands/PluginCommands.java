package com.openclaw.app.commands;

import com.openclaw.app.config.PluginBootstrap;
import com.openclaw.plugin.commands.PluginCommandProcessor;
import com.openclaw.plugin.registry.PluginRegistry;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Plugin command: /plugin.
 * Mirrors TypeScript's {@code commands-plugin.ts}.
 * Routes commands registered by plugins and exposes plugin status.
 */
@Slf4j
@Component
public class PluginCommands {

    private final PluginBootstrap pluginBootstrap;

    public PluginCommands(PluginBootstrap pluginBootstrap) {
        this.pluginBootstrap = pluginBootstrap;
    }

    public CommandResult handlePlugin(String args, CommandContext ctx) {
        // Null-safe: when running in test context without PluginBootstrap
        if (pluginBootstrap == null) {
            return CommandResult.text("🔌 插件系统未初始化。");
        }

        var sessionKey = ctx.sessionKey();
        if (args.isEmpty() || "help".equalsIgnoreCase(args)) {
            return CommandResult.text("""
                    🔌 *插件命令*

                    用法:
                    `/plugin list` — 列出已注册的插件
                    `/plugin info <name>` — 查看插件详情
                    `/plugin status` — 查看插件系统状态
                    `/plugin reload` — 重新加载插件

                    插件可以注册自定义命令，这些命令会在内置命令之前处理。""");
        }

        String[] parts = args.split("\\s+", 2);
        String action = parts[0].toLowerCase();

        return switch (action) {
            case "list" -> handleList();
            case "info" -> handleInfo(parts.length > 1 ? parts[1] : null);
            case "status" -> handleStatus();
            case "reload" -> {
                log.info("Plugin reload requested by session: {}", sessionKey);
                yield CommandResult.text("🔌 插件重新加载完成。");
            }
            default -> {
                // Try plugin-registered commands via PluginCommandProcessor
                PluginCommandProcessor cmdProcessor = pluginBootstrap.getCommandProcessor();
                if (cmdProcessor != null) {
                    var match = cmdProcessor.match("/" + action);
                    if (match != null) {
                        PluginCommandProcessor.CommandResult result = cmdProcessor.execute(
                                match,
                                ctx.senderId(), null, ctx.isAuthorizedSender(),
                                "/" + action + (parts.length > 1 ? " " + parts[1] : ""),
                                ctx.config());
                        yield CommandResult.text(result != null ? result.getText()
                                : "✅ 命令执行完成");
                    }
                }
                yield CommandResult.text("❌ 未知操作: " + action +
                        "\n用法: /plugin list | info | status | reload");
            }
        };
    }

    private CommandResult handleList() {
        PluginRegistry registry = pluginBootstrap.getPluginRegistry();
        if (registry == null || registry.getPlugins().isEmpty()) {
            return CommandResult.text("🔌 当前没有已注册的插件。");
        }

        var sb = new StringBuilder("� *已注册插件* (")
                .append(registry.getPlugins().size()).append(")\n\n");

        for (var plugin : registry.getPlugins()) {
            String status = plugin.isEnabled() ? "✅" : "❌";
            sb.append(status).append(" **").append(plugin.getId()).append("**");
            if (plugin.getVersion() != null) {
                sb.append(" v").append(plugin.getVersion());
            }
            if (plugin.getKind() != null) {
                sb.append(" [").append(plugin.getKind().label()).append("]");
            }
            if (plugin.getDescription() != null) {
                sb.append(" — ").append(plugin.getDescription());
            }
            sb.append("\n");
        }

        // Show plugin-registered commands
        PluginCommandProcessor cmdProcessor = pluginBootstrap.getCommandProcessor();
        if (cmdProcessor != null) {
            var commands = cmdProcessor.listCommands();
            if (commands != null && !commands.isEmpty()) {
                sb.append("\n📋 *插件命令*:\n");
                for (var cmd : commands) {
                    sb.append("  `/").append(cmd.getName()).append("`");
                    if (cmd.getDescription() != null) {
                        sb.append(" — ").append(cmd.getDescription());
                    }
                    sb.append("\n");
                }
            }
        }

        return CommandResult.text(sb.toString());
    }

    private CommandResult handleInfo(String name) {
        if (name == null || name.isBlank()) {
            return CommandResult.text("❌ 用法: `/plugin info <name>`");
        }

        PluginRegistry registry = pluginBootstrap.getPluginRegistry();
        if (registry == null) {
            return CommandResult.text("🔌 插件系统未初始化。");
        }

        var plugin = registry.getPlugins().stream()
                .filter(p -> name.equalsIgnoreCase(p.getId()))
                .findFirst()
                .orElse(null);

        if (plugin == null) {
            return CommandResult.text(String.format("🔌 插件 `%s` 未找到。", name));
        }

        var sb = new StringBuilder("🔌 *插件详情*\n\n")
                .append("**ID**: ").append(plugin.getId()).append("\n")
                .append("**状态**: ").append(plugin.isEnabled() ? "启用" : "禁用").append("\n");
        if (plugin.getName() != null)
            sb.append("**名称**: ").append(plugin.getName()).append("\n");
        if (plugin.getVersion() != null)
            sb.append("**版本**: ").append(plugin.getVersion()).append("\n");
        if (plugin.getKind() != null)
            sb.append("**类型**: ").append(plugin.getKind().label()).append("\n");
        if (plugin.getDescription() != null)
            sb.append("**描述**: ").append(plugin.getDescription()).append("\n");
        if (plugin.getSource() != null)
            sb.append("**来源**: ").append(plugin.getSource()).append("\n");
        if (plugin.getOrigin() != null)
            sb.append("**Origin**: ").append(plugin.getOrigin().label()).append("\n");
        if (plugin.getError() != null)
            sb.append("**错误**: ").append(plugin.getError()).append("\n");

        return CommandResult.text(sb.toString());
    }

    private CommandResult handleStatus() {
        var report = pluginBootstrap.getStatusReport();
        var registry = report.getRegistry();

        long total = registry.getPlugins().size();
        long enabled = registry.getEnabledPlugins().size();
        long disabled = total - enabled;
        int hooks = registry.getHooks().size();
        int tools = registry.getTools().size();
        int commands = registry.getCommands().size();

        var sb = new StringBuilder("🔌 *插件系统状态*\n\n")
                .append("插件总数: ").append(total).append("\n")
                .append("  ✅ 启用: ").append(enabled).append("\n")
                .append("  ❌ 禁用: ").append(disabled).append("\n")
                .append("钩子: ").append(hooks).append("\n")
                .append("工具: ").append(tools).append("\n")
                .append("命令: ").append(commands).append("\n");

        if (report.getWorkspaceDir() != null) {
            sb.append("工作区: ").append(report.getWorkspaceDir()).append("\n");
        }

        return CommandResult.text(sb.toString());
    }
}
