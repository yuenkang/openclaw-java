package com.openclaw.app.commands;

import com.openclaw.common.config.ConfigPaths;
import com.openclaw.common.config.ConfigRuntimeOverrides;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.common.config.SessionPaths;
import com.openclaw.common.infra.ChannelStatusIssues;
import com.openclaw.common.infra.ChannelSummary;
import com.openclaw.common.infra.PortsInspect;
import com.openclaw.common.infra.UpdateCheck;
import com.openclaw.gateway.session.SessionPersistence;
import com.openclaw.gateway.session.TranscriptStore;
import com.openclaw.gateway.session.UsageTracker;
import com.openclaw.common.config.AgentDirs;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Enhanced status commands: /status, /status all.
 * <p>
 * Integrates {@link ChannelSummary} and {@link ChannelStatusIssues}
 * from the infra layer for comprehensive status reporting.
 * <p>
 * Mirrors TypeScript's commands/status.summary.ts + status.command.ts.
 */
@Slf4j
@Component
public class StatusCommands {

    // =========================================================================
    // /status — enhanced overview
    // =========================================================================

    /**
     * Handle /status — enhanced status report with channels, ports, update info.
     */
    public CommandResult handleStatus(String args, CommandContext ctx) {
        if ("all".equalsIgnoreCase(args)) {
            return handleStatusAll(args, ctx);
        }

        var config = ctx.config();
        var sessionKey = ctx.sessionKey();
        StringBuilder sb = new StringBuilder();

        // Header
        sb.append("📋 *OpenClaw 状态*\n\n");

        // 1. Model info
        appendModelSection(sb, config);

        // 2. Session info
        appendSessionSection(sb, sessionKey);

        // 3. Channel summary
        appendChannelSection(sb, config);

        // 4. Port status (gateway port)
        appendPortSection(sb, config);

        // 5. Update status
        appendUpdateSection(sb);

        // 6. Runtime overrides
        var overrides = ConfigRuntimeOverrides.getConfigOverrides();
        if (!overrides.isEmpty()) {
            sb.append(String.format("⚙️ 运行时覆盖: %d 项\n", overrides.size()));
        }

        return CommandResult.text(sb.toString());
    }

    // =========================================================================
    // /status all — full report
    // =========================================================================

    /**
     * Handle /status all — full status report including all agents.
     */
    public CommandResult handleStatusAll(String args, CommandContext ctx) {
        var config = ctx.config();
        var sessionKey = ctx.sessionKey();
        StringBuilder sb = new StringBuilder();

        sb.append("📋 *OpenClaw 全量状态*\n\n");

        // 1. Model info
        appendModelSection(sb, config);

        // 2. Session info (full)
        appendSessionSection(sb, sessionKey);

        // 3. Channels — expanded
        appendChannelSection(sb, config);

        // 4. Channel issues
        appendChannelIssuesSection(sb, config);

        // 5. Port diagnostics
        appendPortSection(sb, config);

        // 6. Update check
        appendUpdateSection(sb);

        // 7. Runtime overrides
        var overrides = ConfigRuntimeOverrides.getConfigOverrides();
        if (!overrides.isEmpty()) {
            sb.append(String.format("⚙️ 运行时覆盖: %d 项\n", overrides.size()));
        }

        // 8. Config path
        Path configPath = ConfigPaths.resolveCanonicalConfigPath();
        sb.append(String.format("\n📁 配置: %s\n", configPath));

        return CommandResult.text(sb.toString());
    }

    // =========================================================================
    // Section builders
    // =========================================================================

    private void appendModelSection(StringBuilder sb, OpenClawConfig config) {
        String modelId = config.getModel() != null ? config.getModel() : "default";
        if (config.getModelAliases() != null && config.getModelAliases().containsKey(modelId)) {
            sb.append(String.format("🏷️ 模型: %s → %s\n", modelId, config.getModelAliases().get(modelId)));
        } else {
            sb.append(String.format("🏷️ 模型: %s\n", modelId));
        }
    }

    private void appendSessionSection(StringBuilder sb, String sessionKey) {
        String agentId = AgentDirs.DEFAULT_AGENT_ID;
        Path storePath = SessionPaths.resolveDefaultSessionStorePath(agentId);
        var store = SessionPersistence.loadSessionStore(storePath);
        var existing = store.get(sessionKey);

        if (existing != null) {
            Path transcriptPath = SessionPaths.resolveSessionTranscriptPath(
                    existing.sessionId(), agentId);
            int msgCount = TranscriptStore.countMessages(transcriptPath);
            sb.append(String.format("💬 会话消息: %d\n", msgCount));
            sb.append(String.format("🔑 会话 ID: %s\n", existing.sessionId().substring(0, 8) + "..."));

            Path usagePath = UsageTracker.resolveUsagePath(transcriptPath);
            UsageTracker.UsageSummary summary = UsageTracker.summarizeUsage(usagePath);
            if (summary.callCount() > 0) {
                sb.append(String.format("📈 Token: %,d (入 %,d + 出 %,d)\n",
                        summary.totalTokens(), summary.totalInputTokens(),
                        summary.totalOutputTokens()));
                sb.append(String.format("💰 成本: $%.4f\n", summary.totalEstimatedCost()));
            }
        } else {
            sb.append("💬 会话: 未初始化\n");
        }
    }

    @SuppressWarnings("unchecked")
    private void appendChannelSection(StringBuilder sb, OpenClawConfig config) {
        try {
            List<ChannelSummary.ChannelInfo> channels = resolveChannelInfos(config);
            if (channels.isEmpty()) {
                sb.append("📡 渠道: 无配置\n");
                return;
            }

            sb.append("📡 *渠道*:\n");
            for (ChannelSummary.ChannelInfo ch : channels) {
                String icon = ch.enabled() ? "✅" : "⛔";
                sb.append(String.format("  %s %s", icon, ch.displayName()));
                if (!ch.accounts().isEmpty()) {
                    long configured = ch.accounts().stream()
                            .filter(ChannelSummary.AccountSummary::configured).count();
                    sb.append(String.format(" (%d 账号)", configured));
                }
                sb.append('\n');
            }
        } catch (Exception e) {
            sb.append("📡 渠道: 获取失败\n");
            log.debug("Channel summary failed: {}", e.getMessage());
        }
    }

    @SuppressWarnings("unchecked")
    private void appendChannelIssuesSection(StringBuilder sb, OpenClawConfig config) {
        try {
            var channelsConfig = config.getChannels();
            if (channelsConfig == null || channelsConfig.getProviders() == null) {
                return;
            }

            List<ChannelStatusIssues.Issue> issues = ChannelStatusIssues.collectAll(channelsConfig.getProviders());

            if (!issues.isEmpty()) {
                sb.append("\n⚠️ *渠道问题*:\n");
                for (ChannelStatusIssues.Issue issue : issues) {
                    String icon = switch (issue.severity()) {
                        case ERROR -> "❌";
                        case WARNING -> "⚠️";
                        case INFO -> "ℹ️";
                    };
                    sb.append(String.format("  %s [%s] %s\n",
                            icon, issue.channelId(), issue.message()));
                }
            }
        } catch (Exception e) {
            log.debug("Channel issues check failed: {}", e.getMessage());
        }
    }

    private void appendPortSection(StringBuilder sb, OpenClawConfig config) {
        try {
            int port = resolveGatewayPort(config);
            PortsInspect.PortUsage usage = PortsInspect.inspectPort(port);

            String statusIcon = switch (usage.status()) {
                case FREE -> "🟢";
                case BUSY -> "🔴";
                case UNKNOWN -> "🟡";
            };
            sb.append(String.format("🔌 Gateway 端口 %d: %s %s\n", port,
                    statusIcon, usage.status().name().toLowerCase()));

            if (usage.status() == PortsInspect.PortStatus.BUSY
                    && usage.listeners() != null && !usage.listeners().isEmpty()) {
                var listener = usage.listeners().get(0);
                if (listener.command() != null) {
                    sb.append(String.format("   占用: %s (PID %s)\n",
                            listener.command(), listener.pid()));
                }
            }
        } catch (Exception e) {
            log.debug("Port inspection failed: {}", e.getMessage());
        }
    }

    private void appendUpdateSection(StringBuilder sb) {
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
                    sb.append(String.format("🔄 更新: %d 提交落后 origin/%s\n",
                            git.behind(), git.branch()));
                } else {
                    sb.append("🔄 更新: 已是最新\n");
                }
            }
        } catch (Exception e) {
            log.debug("Update check skipped: {}", e.getMessage());
        }
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    @SuppressWarnings("unchecked")
    private List<ChannelSummary.ChannelInfo> resolveChannelInfos(OpenClawConfig config) {
        var channelsConfig = config.getChannels();
        if (channelsConfig == null || channelsConfig.getProviders() == null) {
            return List.of();
        }

        List<ChannelSummary.ChannelInfo> channels = new ArrayList<>();
        for (Map.Entry<String, Object> entry : channelsConfig.getProviders().entrySet()) {
            String channelId = entry.getKey();
            Object raw = entry.getValue();

            List<ChannelSummary.AccountSummary> accounts = new ArrayList<>();
            boolean hasConfigured = false;

            if (raw instanceof List<?> accountList) {
                for (Object acc : accountList) {
                    if (acc instanceof Map<?, ?> accountMap) {
                        Object labelObj = accountMap.get("label");
                        String label = labelObj != null ? String.valueOf(labelObj) : channelId;
                        Object enabledObj = accountMap.get("enabled");
                        boolean enabled = enabledObj == null || !"false".equals(String.valueOf(enabledObj));
                        boolean configured = accountMap.containsKey("token")
                                || accountMap.containsKey("apiKey")
                                || accountMap.containsKey("botToken");
                        if (configured)
                            hasConfigured = true;

                        accounts.add(new ChannelSummary.AccountSummary(
                                label, enabled, configured, null, null, null));
                    }
                }
            }

            String displayName = channelId.substring(0, 1).toUpperCase() + channelId.substring(1);
            channels.add(new ChannelSummary.ChannelInfo(
                    channelId, displayName, hasConfigured, accounts, null));
        }
        return channels;
    }

    private int resolveGatewayPort(OpenClawConfig config) {
        if (config.getGateway() != null && config.getGateway().getPort() != null) {
            return config.getGateway().getPort();
        }
        String envPort = System.getenv("OPENCLAW_PORT");
        if (envPort != null && !envPort.isBlank()) {
            try {
                return Integer.parseInt(envPort.trim());
            } catch (NumberFormatException ignored) {
            }
        }
        return 3000; // default
    }

    private String resolveCurrentVersion() {
        Package pkg = getClass().getPackage();
        if (pkg != null && pkg.getImplementationVersion() != null) {
            return pkg.getImplementationVersion();
        }
        return "0.0.0-dev";
    }
}
