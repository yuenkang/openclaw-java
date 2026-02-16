package com.openclaw.app.commands;

import com.openclaw.common.config.ConfigRuntimeOverrides;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * Allowlist command: /allowlist.
 * Manages the owner allow-from list (commands.ownerAllowFrom) for sender
 * authorization.
 * Only authorized senders can modify the allowlist.
 */
@Slf4j
@Component
public class AllowlistCommands {

    public CommandResult handleAllowlist(String args, CommandContext ctx) {
        if (args.isEmpty() || "help".equalsIgnoreCase(args)) {
            return CommandResult.text("""
                    🔐 *白名单管理*

                    用法:
                    `/allowlist list` — 查看当前白名单
                    `/allowlist add <id>` — 添加用户到白名单
                    `/allowlist remove <id>` — 从白名单移除用户
                    `/allowlist addme` — 将自己添加到白名单

                    白名单控制哪些用户可以与 bot 交互和执行命令。
                    白名单为空时，所有用户均可访问。""");
        }

        String[] parts = args.split("\\s+", 2);
        String action = parts[0].toLowerCase();

        return switch (action) {
            case "list" -> handleList(ctx);
            case "add" -> handleAdd(parts, ctx);
            case "remove" -> handleRemove(parts, ctx);
            case "addme" -> handleAddMe(ctx);
            default -> CommandResult.text("❌ 未知操作: " + action + "\n用法: /allowlist list | add | remove | addme");
        };
    }

    private CommandResult handleList(CommandContext ctx) {
        List<String> entries = CommandAuthorization.resolveAllowList(ctx.config(), null);
        if (entries.isEmpty()) {
            return CommandResult.text("🔐 白名单为空（所有用户均可访问）。");
        }
        StringBuilder sb = new StringBuilder("🔐 *白名单*\n\n");
        for (String entry : entries) {
            sb.append(String.format("• `%s`\n", entry));
        }
        sb.append(String.format("\n共 %d 个条目。", entries.size()));
        return CommandResult.text(sb.toString());
    }

    private CommandResult handleAdd(String[] parts, CommandContext ctx) {
        if (!ctx.isAuthorizedSender()) {
            return CommandResult.text("🚫 权限不足：只有白名单中的用户才能修改白名单。");
        }
        if (parts.length < 2 || parts[1].isBlank()) {
            return CommandResult.text("❌ 用法: `/allowlist add <user-id>`");
        }
        String userId = parts[1].trim();
        List<String> entries = getOverrideEntries();
        if (entries.contains(userId)) {
            // Also check original config
            List<String> all = CommandAuthorization.resolveAllowList(ctx.config(), null);
            if (all.contains(userId)) {
                return CommandResult.text(String.format("ℹ️ `%s` 已在白名单中。", userId));
            }
        }
        entries.add(userId);
        saveAllowlist(entries, ctx);
        return CommandResult.text(String.format("✅ 已添加 `%s` 到白名单。", userId));
    }

    private CommandResult handleRemove(String[] parts, CommandContext ctx) {
        if (!ctx.isAuthorizedSender()) {
            return CommandResult.text("🚫 权限不足：只有白名单中的用户才能修改白名单。");
        }
        if (parts.length < 2 || parts[1].isBlank()) {
            return CommandResult.text("❌ 用法: `/allowlist remove <user-id>`");
        }
        String userId = parts[1].trim();
        // Prevent removing yourself from the list (self-lockout)
        if (ctx.senderId() != null && userId.equals(ctx.senderId())) {
            return CommandResult.text("⚠️ 不能将自己从白名单中移除，否则会导致无法再使用命令。");
        }
        List<String> entries = getOverrideEntries();
        if (!entries.remove(userId)) {
            return CommandResult.text(String.format("ℹ️ `%s` 不在运行时白名单中（可能在配置文件中，请手动编辑）。", userId));
        }
        saveAllowlist(entries, ctx);
        return CommandResult.text(String.format("✅ 已从运行时白名单移除 `%s`。", userId));
    }

    private CommandResult handleAddMe(CommandContext ctx) {
        String senderId = ctx.senderId();
        if (senderId == null || senderId.isBlank()) {
            return CommandResult.text("❌ 无法识别你的用户 ID。");
        }
        // If allowlist is empty, anyone can addme (bootstrapping)
        List<String> allEntries = CommandAuthorization.resolveAllowList(ctx.config(), null);
        if (!allEntries.isEmpty() && !ctx.isAuthorizedSender()) {
            return CommandResult.text("🚫 权限不足：白名单已配置，只有白名单中的用户才能修改。");
        }
        if (allEntries.contains(senderId)) {
            return CommandResult.text(String.format("ℹ️ 你的 ID `%s` 已在白名单中。", senderId));
        }
        List<String> overrides = getOverrideEntries();
        overrides.add(senderId);
        saveAllowlist(overrides, ctx);
        return CommandResult.text(String.format("✅ 已将你的 ID `%s` 添加到白名单。", senderId));
    }

    @SuppressWarnings("unchecked")
    private List<String> getOverrideEntries() {
        var overrides = ConfigRuntimeOverrides.getConfigOverrides();
        Object allowFrom = CommandUtils.resolveNestedValue(overrides, "commands.ownerAllowFrom");
        List<String> result = new ArrayList<>();
        if (allowFrom instanceof List<?> list) {
            for (Object item : list) {
                result.add(String.valueOf(item));
            }
        }
        return result;
    }

    private void saveAllowlist(List<String> entries, CommandContext ctx) {
        // 1. Set runtime override so it takes effect immediately
        ConfigRuntimeOverrides.setConfigOverride("commands.ownerAllowFrom", new ArrayList<>(entries));

        // 2. Persist to disk if ConfigService is available
        if (ctx.configService() != null) {
            try {
                // Reload config (merges runtime overrides) then save
                var config = ctx.configService().reloadConfig();
                ctx.configService().saveConfig(config);
                log.info("Allowlist persisted to disk: {} entries", entries.size());
            } catch (Exception e) {
                log.error("Failed to persist allowlist to disk: {}", e.getMessage(), e);
            }
        } else {
            log.warn("ConfigService not available, allowlist changes are runtime-only");
        }
    }
}
