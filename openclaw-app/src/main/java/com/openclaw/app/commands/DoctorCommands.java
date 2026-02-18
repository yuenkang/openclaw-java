package com.openclaw.app.commands;

import com.openclaw.common.config.ConfigPaths;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.common.infra.Binaries;
import com.openclaw.common.infra.PortsInspect;
import com.openclaw.common.infra.UpdateCheck;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Doctor diagnostic command: /doctor.
 * <p>
 * Runs a series of health checks and reports ✅/⚠️/❌ results.
 * Integrates {@link Binaries} for binary detection and
 * {@link PortsInspect} for port diagnostics.
 * <p>
 * Mirrors a simplified version of TypeScript's commands/doctor.ts.
 */
@Slf4j
@Component
public class DoctorCommands {

    /**
     * Handle /doctor — run diagnostics and report results.
     */
    public CommandResult handleDoctor(String args, CommandContext ctx) {
        var config = ctx.config();
        List<String> lines = new ArrayList<>();
        int passed = 0;
        int warnings = 0;
        int errors = 0;

        lines.add("🩺 *OpenClaw 诊断报告*\n");

        // 1. Config file check
        var configResult = checkConfig();
        lines.add(configResult.line());
        passed += configResult.passed();
        warnings += configResult.warnings();
        errors += configResult.errors();

        // 2. Auth / API keys check
        var authResult = checkAuthKeys();
        lines.addAll(authResult.lines());
        passed += authResult.passed();
        warnings += authResult.warnings();
        errors += authResult.errors();

        // 3. Port availability check
        var portResult = checkPort(config);
        lines.add(portResult.line());
        passed += portResult.passed();
        warnings += portResult.warnings();
        errors += portResult.errors();

        // 4. Binary dependencies check
        var binResult = checkBinaries();
        lines.addAll(binResult.lines());
        passed += binResult.passed();
        warnings += binResult.warnings();
        errors += binResult.errors();

        // 5. Update check
        var updateResult = checkUpdate();
        lines.add(updateResult.line());
        passed += updateResult.passed();
        warnings += updateResult.warnings();
        errors += updateResult.errors();

        // Summary
        lines.add("");
        lines.add(String.format("*汇总*: ✅ %d 通过  ⚠️ %d 警告  ❌ %d 错误",
                passed, warnings, errors));

        if (errors > 0) {
            lines.add("\n运行 `/doctor` 查看详情并修复问题。");
        } else if (warnings > 0) {
            lines.add("\n大部分检查通过，有少量警告可关注。");
        } else {
            lines.add("\n🎉 所有检查通过！");
        }

        return CommandResult.text(String.join("\n", lines));
    }

    // =========================================================================
    // Individual checks
    // =========================================================================

    private record CheckResult(String line, int passed, int warnings, int errors) {
    }

    private record MultiCheckResult(List<String> lines, int passed, int warnings, int errors) {
    }

    /**
     * Check if config file exists and is valid.
     */
    private CheckResult checkConfig() {
        try {
            Path configPath = ConfigPaths.resolveCanonicalConfigPath();
            if (Files.exists(configPath)) {
                long size = Files.size(configPath);
                return new CheckResult(
                        String.format("✅ 配置文件: %s (%d bytes)", configPath.getFileName(), size),
                        1, 0, 0);
            } else {
                return new CheckResult(
                        "⚠️ 配置文件: 不存在（使用默认值）",
                        0, 1, 0);
            }
        } catch (Exception e) {
            return new CheckResult(
                    "❌ 配置文件: 读取失败 — " + e.getMessage(),
                    0, 0, 1);
        }
    }

    /**
     * Check if important API keys are set.
     */
    private MultiCheckResult checkAuthKeys() {
        List<String> lines = new ArrayList<>();
        int passed = 0;
        int warnings = 0;

        String[][] keyChecks = {
                { "OPENAI_API_KEY", "OpenAI" },
                { "ANTHROPIC_API_KEY", "Anthropic" },
                { "GEMINI_API_KEY", "Gemini" },
                { "TELEGRAM_BOT_TOKEN", "Telegram Bot" },
        };

        boolean anyFound = false;
        List<String> missing = new ArrayList<>();

        for (String[] kv : keyChecks) {
            String envVal = System.getenv(kv[0]);
            String propVal = System.getProperty(kv[0]);
            if ((envVal != null && !envVal.isBlank()) || (propVal != null && !propVal.isBlank())) {
                anyFound = true;
            } else {
                missing.add(kv[1]);
            }
        }

        if (anyFound) {
            lines.add("✅ API 密钥: 已检测到");
            passed++;
            if (!missing.isEmpty()) {
                lines.add("   ℹ️ 未设置: " + String.join(", ", missing));
            }
        } else {
            lines.add("⚠️ API 密钥: 未检测到任何 API key");
            warnings++;
        }

        return new MultiCheckResult(lines, passed, warnings, 0);
    }

    /**
     * Check gateway port availability.
     */
    private CheckResult checkPort(OpenClawConfig config) {
        try {
            int port = 3000;
            if (config.getGateway() != null && config.getGateway().getPort() != null) {
                port = config.getGateway().getPort();
            }

            PortsInspect.PortUsage usage = PortsInspect.inspectPort(port);
            if (usage.status() == PortsInspect.PortStatus.FREE) {
                return new CheckResult(
                        String.format("✅ 端口 %d: 可用", port),
                        1, 0, 0);
            } else if (usage.status() == PortsInspect.PortStatus.BUSY) {
                String owner = "";
                if (usage.listeners() != null && !usage.listeners().isEmpty()) {
                    var listener = usage.listeners().get(0);
                    owner = listener.command() != null
                            ? " (" + listener.command() + ")"
                            : "";
                }
                return new CheckResult(
                        String.format("⚠️ 端口 %d: 已被占用%s", port, owner),
                        0, 1, 0);
            } else {
                return new CheckResult(
                        String.format("🟡 端口 %d: 未知状态", port),
                        0, 1, 0);
            }
        } catch (Exception e) {
            return new CheckResult(
                    "🟡 端口检查: 跳过 — " + e.getMessage(),
                    0, 1, 0);
        }
    }

    /**
     * Check for required/optional binaries on PATH.
     */
    private MultiCheckResult checkBinaries() {
        List<String> lines = new ArrayList<>();
        int passed = 0;
        int warnings = 0;

        String[][] binaries = {
                { "git", "required" },
                { "ffmpeg", "optional" },
                { "node", "optional" },
        };

        for (String[] bin : binaries) {
            boolean found = Binaries.hasBinary(bin[0]);
            if (found) {
                lines.add(String.format("✅ %s: 已安装", bin[0]));
                passed++;
            } else if ("required".equals(bin[1])) {
                lines.add(String.format("❌ %s: 未找到（必需）", bin[0]));
                // Count as warning, not error — app can still run
                warnings++;
            } else {
                lines.add(String.format("ℹ️ %s: 未安装（可选）", bin[0]));
                passed++; // optional not found is still OK
            }
        }

        return new MultiCheckResult(lines, passed, warnings, 0);
    }

    /**
     * Check for available updates.
     */
    private CheckResult checkUpdate() {
        try {
            String projectRoot = System.getProperty("user.dir");
            String currentVersion = resolveCurrentVersion();
            UpdateCheck.UpdateCheckResult result = UpdateCheck.checkUpdateStatus(
                    projectRoot, currentVersion, 2000L, false);

            if (result.installKind() == UpdateCheck.InstallKind.GIT
                    && result.gitStatus() != null
                    && result.gitStatus().isGitRepo()) {
                var git = result.gitStatus();
                if (git.behind() > 0) {
                    return new CheckResult(
                            String.format("⚠️ 更新: %d 提交落后 origin/%s",
                                    git.behind(), git.branch()),
                            0, 1, 0);
                }
                return new CheckResult("✅ 更新: 已是最新版", 1, 0, 0);
            }

            return new CheckResult("ℹ️ 更新: 非 git 安装，跳过检查", 1, 0, 0);
        } catch (Exception e) {
            return new CheckResult("🟡 更新检查: 跳过", 0, 0, 0);
        }
    }

    private String resolveCurrentVersion() {
        Package pkg = getClass().getPackage();
        if (pkg != null && pkg.getImplementationVersion() != null) {
            return pkg.getImplementationVersion();
        }
        return "0.0.0-dev";
    }
}
