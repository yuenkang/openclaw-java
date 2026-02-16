package com.openclaw.common.security;

import com.openclaw.common.security.SecurityAuditTypes.*;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.*;

/**
 * Security audit engine — checks configuration, file permissions, gateway,
 * browser control, elevated privileges, logging, and channel security.
 * Translates TS security/audit.ts core logic.
 *
 */
public final class SecurityAudit {

    private SecurityAudit() {
    }

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /**
     * Run a full security audit and produce a report.
     */
    public static Report runSecurityAudit(AuditOptions opts) {
        List<Finding> findings = new ArrayList<>();

        // Filesystem checks
        if (opts.includeFilesystem()) {
            findings.addAll(collectFilesystemFindings(opts.stateDir(), opts.configPath()));
        }

        // Gateway config checks
        findings.addAll(collectGatewayConfigFindings());

        // Browser control checks
        findings.addAll(collectBrowserControlFindings(opts.config()));

        // Elevated privilege checks
        findings.addAll(collectElevatedFindings(opts.config()));

        // Logging checks
        findings.addAll(collectLoggingFindings(opts.config()));

        // Channel security checks
        if (opts.includeChannelSecurity()) {
            findings.addAll(collectChannelSecurityFindings(opts.config()));
        }

        Summary summary = Summary.fromFindings(findings);
        return new Report(System.currentTimeMillis(), summary, findings);
    }

    /**
     * Run with default options.
     */
    public static Report runSecurityAudit() {
        return runSecurityAudit(AuditOptions.defaults());
    }

    // -----------------------------------------------------------------------
    // Filesystem findings
    // -----------------------------------------------------------------------

    static List<Finding> collectFilesystemFindings(String stateDir, String configPath) {
        List<Finding> findings = new ArrayList<>();

        Path statePath = Path.of(stateDir);
        if (Files.exists(statePath)) {
            if (Files.isSymbolicLink(statePath)) {
                findings.add(new Finding("fs.state_dir.symlink", Severity.WARN,
                        "State dir is a symlink",
                        stateDir + " is a symlink; treat this as an extra trust boundary."));
            }
            checkPathPermissions(statePath, "fs.state_dir.perms",
                    "State directory permissions",
                    "State directory should not be world-readable or world-writable",
                    findings);
        } else {
            findings.add(new Finding("fs.state_dir.missing", Severity.INFO,
                    "State directory not found",
                    "State directory does not exist at: " + stateDir));
        }

        Path cfgPath = Path.of(configPath);
        if (Files.exists(cfgPath)) {
            if (Files.isSymbolicLink(cfgPath)) {
                findings.add(new Finding("fs.config.symlink", Severity.WARN,
                        "Config file is a symlink",
                        configPath + " is a symlink; make sure you trust its target."));
            }
            checkPathPermissions(cfgPath, "fs.config.perms",
                    "Config file permissions",
                    "Config file may contain API keys and should not be world-readable",
                    findings);
        }

        if (Files.isDirectory(statePath)) {
            checkSensitiveFiles(statePath, findings);
        }

        return findings;
    }

    private static void checkPathPermissions(Path path, String checkId,
            String title, String detail,
            List<Finding> findings) {
        try {
            Set<PosixFilePermission> perms = Files.getPosixFilePermissions(path);
            boolean worldWritable = perms.contains(PosixFilePermission.OTHERS_WRITE);
            boolean worldReadable = perms.contains(PosixFilePermission.OTHERS_READ);
            boolean groupWritable = perms.contains(PosixFilePermission.GROUP_WRITE);
            boolean groupReadable = perms.contains(PosixFilePermission.GROUP_READ);
            String permStr = PosixFilePermissions.toString(perms);

            if (worldWritable) {
                findings.add(new Finding(checkId + ".world_writable", Severity.CRITICAL, title,
                        detail + " — world-writable (" + permStr + ")",
                        "Run: chmod 700 " + path + " (directories) or chmod 600 " + path + " (files)"));
            } else if (groupWritable) {
                findings.add(new Finding(checkId + ".group_writable", Severity.WARN, title,
                        detail + " — group-writable (" + permStr + ")",
                        "Run: chmod 700 " + path + " (directories) or chmod 600 " + path + " (files)"));
            } else if (worldReadable || groupReadable) {
                findings.add(new Finding(checkId + ".readable", Severity.WARN, title,
                        detail + " — readable by others (" + permStr + "); consider restricting.",
                        "Run: chmod 700 " + path + " (directories) or chmod 600 " + path + " (files)"));
            }
        } catch (UnsupportedOperationException e) {
            findings.add(new Finding(checkId + ".unsupported", Severity.INFO,
                    title,
                    "Cannot check POSIX permissions on this filesystem: " + path));
        } catch (IOException e) {
            findings.add(new Finding(checkId + ".error", Severity.INFO,
                    title,
                    "Could not read permissions for: " + path + " — " + e.getMessage()));
        }
    }

    private static void checkSensitiveFiles(Path stateDir, List<Finding> findings) {
        String[] sensitivePatterns = { "credentials", "token", "secret", ".key", ".pem" };
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(stateDir)) {
            for (Path entry : stream) {
                String name = entry.getFileName().toString().toLowerCase();
                for (String pattern : sensitivePatterns) {
                    if (name.contains(pattern) && Files.isRegularFile(entry)) {
                        checkPathPermissions(entry, "fs.sensitive_file",
                                "Sensitive file found",
                                "File '" + name + "' may contain credentials",
                                findings);
                        break;
                    }
                }
            }
        } catch (IOException ignored) {
            // skip if directory is unreadable
        }
    }

    // -----------------------------------------------------------------------
    // Gateway config findings
    // -----------------------------------------------------------------------

    static List<Finding> collectGatewayConfigFindings() {
        List<Finding> findings = new ArrayList<>();

        // Check if running as root
        String user = System.getProperty("user.name");
        if ("root".equals(user)) {
            findings.add(new Finding("gateway.running_as_root", Severity.CRITICAL,
                    "Running as root user",
                    "The application is running as root. This increases the blast radius of any security issue.",
                    "Run the application as a non-root user."));
        }

        // Check for common environment variable leaks
        Map<String, String> env = System.getenv();
        for (String key : env.keySet()) {
            String upper = key.toUpperCase();
            if ((upper.contains("KEY") || upper.contains("SECRET") || upper.contains("TOKEN") ||
                    upper.contains("PASSWORD")) && !upper.startsWith("GPG_") && !upper.startsWith("SSH_")) {
                findings.add(new Finding("gateway.env_sensitive_var", Severity.INFO,
                        "Sensitive environment variable detected",
                        "Environment variable '" + key + "' appears to contain credentials. " +
                                "Ensure it is not logged or exposed."));
            }
        }

        return findings;
    }

    // -----------------------------------------------------------------------
    // Browser control findings — mirrors TS collectBrowserControlFindings
    // -----------------------------------------------------------------------

    /**
     * Check browser control configuration for security issues.
     *
     * @param config map of config keys (e.g. from JSON config); may be null
     */
    @SuppressWarnings("unchecked")
    static List<Finding> collectBrowserControlFindings(Map<String, Object> config) {
        List<Finding> findings = new ArrayList<>();
        if (config == null)
            return findings;

        Object browserObj = config.get("browser");
        if (!(browserObj instanceof Map))
            return findings;
        Map<String, Object> browser = (Map<String, Object>) browserObj;

        Object enabledObj = browser.get("enabled");
        if (Boolean.FALSE.equals(enabledObj))
            return findings;

        // Check for non-loopback CDP URLs
        String cdpUrl = (String) browser.get("cdpUrl");
        if (cdpUrl != null && !cdpUrl.isEmpty()) {
            try {
                java.net.URI uri = java.net.URI.create(cdpUrl);
                String host = uri.getHost();
                boolean isLoopback = host != null &&
                        (host.equals("localhost") || host.equals("127.0.0.1") || host.equals("::1"));
                if (!isLoopback) {
                    if ("http".equals(uri.getScheme())) {
                        findings.add(new Finding("browser.remote_cdp_http", Severity.WARN,
                                "Remote CDP uses HTTP",
                                "browser.cdpUrl uses HTTP (" + cdpUrl + "); " +
                                        "this is OK only if it's tailnet-only or behind an encrypted tunnel.",
                                "Prefer HTTPS/TLS or a tailnet-only endpoint for remote CDP."));
                    }
                }
            } catch (IllegalArgumentException ignored) {
                findings.add(new Finding("browser.cdp_url_invalid", Severity.WARN,
                        "Browser CDP URL looks invalid",
                        "browser.cdpUrl could not be parsed: " + cdpUrl));
            }
        }

        return findings;
    }

    // -----------------------------------------------------------------------
    // Elevated privilege findings — mirrors TS collectElevatedFindings
    // -----------------------------------------------------------------------

    @SuppressWarnings("unchecked")
    static List<Finding> collectElevatedFindings(Map<String, Object> config) {
        List<Finding> findings = new ArrayList<>();
        if (config == null)
            return findings;

        Object toolsObj = config.get("tools");
        if (!(toolsObj instanceof Map))
            return findings;
        Map<String, Object> tools = (Map<String, Object>) toolsObj;

        Object elevatedObj = tools.get("elevated");
        if (!(elevatedObj instanceof Map))
            return findings;
        Map<String, Object> elevated = (Map<String, Object>) elevatedObj;

        if (Boolean.FALSE.equals(elevated.get("enabled")))
            return findings;

        Object allowFromObj = elevated.get("allowFrom");
        if (!(allowFromObj instanceof Map))
            return findings;
        Map<String, Object> allowFrom = (Map<String, Object>) allowFromObj;

        for (Map.Entry<String, Object> entry : allowFrom.entrySet()) {
            String provider = entry.getKey();
            List<String> list = normalizeAllowFromList(entry.getValue());

            if (list.contains("*")) {
                findings.add(new Finding(
                        "tools.elevated.allowFrom." + provider + ".wildcard",
                        Severity.CRITICAL,
                        "Elevated exec allowlist contains wildcard",
                        "tools.elevated.allowFrom." + provider +
                                " includes \"*\" which effectively approves everyone on that channel for elevated mode."));
            } else if (list.size() > 25) {
                findings.add(new Finding(
                        "tools.elevated.allowFrom." + provider + ".large",
                        Severity.WARN,
                        "Elevated exec allowlist is large",
                        "tools.elevated.allowFrom." + provider + " has " + list.size() +
                                " entries; consider tightening elevated access."));
            }
        }

        return findings;
    }

    // -----------------------------------------------------------------------
    // Logging findings — mirrors TS collectLoggingFindings
    // -----------------------------------------------------------------------

    @SuppressWarnings("unchecked")
    static List<Finding> collectLoggingFindings(Map<String, Object> config) {
        List<Finding> findings = new ArrayList<>();

        // Check log directory permissions
        Path logDir = Path.of("/tmp/openclaw");
        if (Files.isDirectory(logDir)) {
            checkPathPermissions(logDir, "logging.log_dir_perms",
                    "Log directory permissions",
                    "Log files may contain sensitive data and should have restricted permissions",
                    findings);
        }

        // Check redaction setting
        if (config != null) {
            Object loggingObj = config.get("logging");
            if (loggingObj instanceof Map) {
                Map<String, Object> logging = (Map<String, Object>) loggingObj;
                Object redact = logging.get("redactSensitive");
                if ("off".equals(redact)) {
                    findings.add(new Finding("logging.redact_off", Severity.WARN,
                            "Tool summary redaction is disabled",
                            "logging.redactSensitive=\"off\" can leak secrets into logs and status output.",
                            "Set logging.redactSensitive=\"tools\"."));
                }
            }
        }

        return findings;
    }

    // -----------------------------------------------------------------------
    // Channel security findings — mirrors TS collectChannelSecurityFindings
    // -----------------------------------------------------------------------

    @SuppressWarnings("unchecked")
    static List<Finding> collectChannelSecurityFindings(Map<String, Object> config) {
        List<Finding> findings = new ArrayList<>();
        if (config == null)
            return findings;

        Object channelsObj = config.get("channels");
        if (!(channelsObj instanceof Map))
            return findings;
        Map<String, Object> channels = (Map<String, Object>) channelsObj;

        // Check each channel provider for open DM policies
        for (Map.Entry<String, Object> entry : channels.entrySet()) {
            String provider = entry.getKey();
            if ("defaults".equals(provider))
                continue;
            if (!(entry.getValue() instanceof Map))
                continue;
            Map<String, Object> providerCfg = (Map<String, Object>) entry.getValue();

            Object dmObj = providerCfg.get("dm");
            if (dmObj instanceof Map) {
                Map<String, Object> dm = (Map<String, Object>) dmObj;
                String dmPolicy = (String) dm.get("policy");
                if ("open".equals(dmPolicy)) {
                    findings.add(new Finding(
                            "channels." + provider + ".dm.open",
                            Severity.CRITICAL,
                            provider + " DMs are open",
                            "channels." + provider + ".dm.policy=\"open\" allows anyone to DM the bot.",
                            "Use pairing/allowlist or restrict DM access."));
                } else if ("disabled".equals(dmPolicy)) {
                    findings.add(new Finding(
                            "channels." + provider + ".dm.disabled",
                            Severity.INFO,
                            provider + " DMs are disabled",
                            "channels." + provider + ".dm.policy=\"disabled\" ignores inbound DMs."));
                }
            }

            // Check group policy
            String groupPolicy = (String) providerCfg.get("groupPolicy");
            if ("open".equals(groupPolicy)) {
                findings.add(new Finding(
                        "channels." + provider + ".group.open",
                        Severity.CRITICAL,
                        provider + " group access is open",
                        "channels." + provider + ".groupPolicy=\"open\" allows any group to interact with the bot.",
                        "Use groupPolicy=\"allowlist\" and configure groups."));
            }
        }

        return findings;
    }

    // -----------------------------------------------------------------------
    // Utility helpers
    // -----------------------------------------------------------------------

    @SuppressWarnings("unchecked")
    private static List<String> normalizeAllowFromList(Object raw) {
        if (raw instanceof List) {
            List<String> result = new ArrayList<>();
            for (Object item : (List<?>) raw) {
                String s = String.valueOf(item).trim();
                if (!s.isEmpty())
                    result.add(s);
            }
            return result;
        }
        return List.of();
    }

    public static Summary countBySeverity(List<Finding> findings) {
        return Summary.fromFindings(findings);
    }

    /**
     * Format a single finding as a human-readable string.
     */
    public static String formatFinding(Finding finding) {
        StringBuilder sb = new StringBuilder();
        String icon = switch (finding.severity()) {
            case CRITICAL -> "🔴";
            case WARN -> "🟡";
            case INFO -> "🔵";
        };
        sb.append(icon).append(" [").append(finding.checkId()).append("] ")
                .append(finding.title()).append("\n");
        sb.append("   ").append(finding.detail()).append("\n");
        if (finding.remediation() != null) {
            sb.append("   Fix: ").append(finding.remediation()).append("\n");
        }
        return sb.toString();
    }

    /**
     * Format a full report as a human-readable string.
     */
    public static String formatReport(Report report) {
        StringBuilder sb = new StringBuilder();
        sb.append("Security Audit Report\n");
        sb.append("=====================\n");
        sb.append("Critical: ").append(report.summary().critical())
                .append(" | Warnings: ").append(report.summary().warn())
                .append(" | Info: ").append(report.summary().info()).append("\n\n");
        for (Finding f : report.findings()) {
            sb.append(formatFinding(f));
        }
        return sb.toString();
    }
}
