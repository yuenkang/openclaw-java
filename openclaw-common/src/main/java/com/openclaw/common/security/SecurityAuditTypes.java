package com.openclaw.common.security;

import java.util.List;
import java.util.Map;

/**
 * Type definitions for security audit reports.
 * Translates TS security/audit.ts type definitions.
 *
 */
public final class SecurityAuditTypes {

    private SecurityAuditTypes() {
    }

    // -----------------------------------------------------------------------
    // Severity
    // -----------------------------------------------------------------------

    public enum Severity {
        CRITICAL, WARN, INFO
    }

    // -----------------------------------------------------------------------
    // Finding
    // -----------------------------------------------------------------------

    public record Finding(
            String checkId,
            Severity severity,
            String title,
            String detail,
            String remediation) {
        public Finding(String checkId, Severity severity, String title, String detail) {
            this(checkId, severity, title, detail, null);
        }
    }

    // -----------------------------------------------------------------------
    // Summary
    // -----------------------------------------------------------------------

    public record Summary(int critical, int warn, int info) {
        public static Summary fromFindings(List<Finding> findings) {
            int c = 0, w = 0, i = 0;
            for (Finding f : findings) {
                switch (f.severity()) {
                    case CRITICAL -> c++;
                    case WARN -> w++;
                    case INFO -> i++;
                }
            }
            return new Summary(c, w, i);
        }
    }

    // -----------------------------------------------------------------------
    // Report
    // -----------------------------------------------------------------------

    public record Report(
            long timestamp,
            Summary summary,
            List<Finding> findings) {
    }

    // -----------------------------------------------------------------------
    // Options
    // -----------------------------------------------------------------------

    public record AuditOptions(
            boolean deep,
            boolean includeFilesystem,
            boolean includeChannelSecurity,
            String stateDir,
            String configPath,
            long deepTimeoutMs,
            Map<String, Object> config) {
        public static AuditOptions defaults() {
            return new AuditOptions(false, true, true,
                    System.getProperty("user.home") + "/.openclaw",
                    System.getProperty("user.home") + "/.openclaw/config.json",
                    10_000, null);
        }
    }
}
