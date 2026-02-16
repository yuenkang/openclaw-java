package com.openclaw.app.commands;

import com.openclaw.common.config.ConfigRuntimeOverrides;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * Config commands: /config, /debug.
 * Mirrors TypeScript's {@code commands-config.ts}.
 */
@Slf4j
@Component
public class ConfigCommands {

    public CommandResult handleConfig(String args, CommandContext ctx) {
        if (args.isEmpty() || args.equalsIgnoreCase("show")) {
            var overrides = ConfigRuntimeOverrides.getConfigOverrides();
            if (overrides.isEmpty()) {
                return CommandResult
                        .text("⚙️ 当前没有运行时配置覆盖。\n\n用法:\n/config set <path> <value>\n/config unset <path>\n/config show");
            }
            StringBuilder sb = new StringBuilder();
            sb.append("⚙️ *运行时配置覆盖*\n\n");
            CommandUtils.formatOverrides(sb, overrides, "");
            return CommandResult.text(sb.toString());
        }

        String[] parts = args.split("\\s+", 3);
        String action = parts[0].toLowerCase();

        return CommandResult.text(switch (action) {
            case "set" -> {
                if (parts.length < 3) {
                    yield "❌ 用法: /config set <path> <value>\n例如: /config set model anthropic/claude-sonnet-4-5";
                }
                String path = parts[1];
                String value = parts[2];
                Object parsed = CommandUtils.parseConfigValue(value);
                var result = ConfigRuntimeOverrides.setConfigOverride(path, parsed);
                if (result.ok()) {
                    yield String.format("✅ 配置已设置: `%s` = `%s`", path, value);
                } else {
                    yield "❌ 设置失败: " + result.error();
                }
            }
            case "unset" -> {
                if (parts.length < 2) {
                    yield "❌ 用法: /config unset <path>";
                }
                String path = parts[1];
                var result = ConfigRuntimeOverrides.unsetConfigOverride(path);
                if (result.ok()) {
                    yield result.removed()
                            ? String.format("✅ 配置已移除: `%s`", path)
                            : String.format("ℹ️ 配置路径 `%s` 不存在", path);
                } else {
                    yield "❌ 移除失败: " + result.error();
                }
            }
            case "get" -> {
                if (parts.length < 2) {
                    yield "❌ 用法: /config get <path>";
                }
                var overrides = ConfigRuntimeOverrides.getConfigOverrides();
                String path = parts[1];
                Object val = CommandUtils.resolveNestedValue(overrides, path);
                yield val != null
                        ? String.format("⚙️ `%s` = `%s`", path, val)
                        : String.format("ℹ️ `%s` 未设置覆盖", path);
            }
            case "reset" -> {
                ConfigRuntimeOverrides.resetConfigOverrides();
                yield "✅ 所有运行时配置覆盖已重置。";
            }
            default -> "❌ 未知操作: " + action + "\n用法: /config show | set | unset | get | reset";
        });
    }

    public CommandResult handleDebug(String args, CommandContext ctx) {
        if (args.isEmpty() || args.equalsIgnoreCase("show")) {
            var overrides = ConfigRuntimeOverrides.getConfigOverrides();
            Object debugSection = overrides.get("debug");
            if (debugSection == null || (debugSection instanceof Map<?, ?> m && m.isEmpty())) {
                return CommandResult
                        .text("🔍 当前没有 debug 覆盖。\n\n用法:\n/debug set <key> <value>\n/debug unset <key>\n/debug reset");
            }
            StringBuilder sb = new StringBuilder();
            sb.append("🔍 *Debug 覆盖*\n\n");
            if (debugSection instanceof Map<?, ?> m) {
                for (var entry : m.entrySet()) {
                    sb.append(String.format("`%s` = `%s`\n", entry.getKey(), entry.getValue()));
                }
            } else {
                sb.append(String.format("`debug` = `%s`\n", debugSection));
            }
            return CommandResult.text(sb.toString());
        }

        String[] parts = args.split("\\s+", 3);
        String action = parts[0].toLowerCase();

        return CommandResult.text(switch (action) {
            case "set" -> {
                if (parts.length < 3) {
                    yield "❌ 用法: /debug set <key> <value>";
                }
                var result = ConfigRuntimeOverrides.setConfigOverride(
                        "debug." + parts[1], CommandUtils.parseConfigValue(parts[2]));
                yield result.ok()
                        ? String.format("✅ Debug 设置: `%s` = `%s`", parts[1], parts[2])
                        : "❌ 设置失败: " + result.error();
            }
            case "unset" -> {
                if (parts.length < 2) {
                    yield "❌ 用法: /debug unset <key>";
                }
                var result = ConfigRuntimeOverrides.unsetConfigOverride("debug." + parts[1]);
                yield result.ok()
                        ? String.format("✅ Debug 已移除: `%s`", parts[1])
                        : "❌ 移除失败: " + result.error();
            }
            case "reset" -> {
                ConfigRuntimeOverrides.unsetConfigOverride("debug");
                yield "✅ 所有 debug 覆盖已重置。";
            }
            default -> "❌ 未知操作: " + action + "\n用法: /debug show | set | unset | reset";
        });
    }
}
