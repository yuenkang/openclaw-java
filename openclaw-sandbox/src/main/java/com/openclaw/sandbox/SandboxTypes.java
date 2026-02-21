package com.openclaw.sandbox;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * Core sandbox type definitions.
 * Corresponds to TypeScript sandbox/types.ts + sandbox/types.docker.ts.
 */
public final class SandboxTypes {

    private SandboxTypes() {
    }

    // ── Docker config ───────────────────────────────────────────────

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxDockerConfig {
        private String image;
        private String containerPrefix;
        private String workdir;
        @Builder.Default
        private boolean readOnlyRoot = true;
        @Builder.Default
        private List<String> tmpfs = List.of("/tmp", "/var/tmp", "/run");
        @Builder.Default
        private String network = "none";
        private String user;
        @Builder.Default
        private List<String> capDrop = List.of("ALL");
        private Map<String, String> env;
        private String setupCommand;
        private Integer pidsLimit;
        private String memory;
        private String memorySwap;
        private Double cpus;
        private Map<String, Object> ulimits; // value: String | Number | {soft,hard}
        private String seccompProfile;
        private String apparmorProfile;
        private List<String> dns;
        private List<String> extraHosts;
        private List<String> binds;
    }

    // ── Tool policy ─────────────────────────────────────────────────

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxToolPolicy {
        private List<String> allow;
        private List<String> deny;
    }

    public enum ToolPolicySourceType {
        AGENT, GLOBAL, DEFAULT
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxToolPolicySource {
        private ToolPolicySourceType source;
        private String key;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxToolPolicyResolved {
        private List<String> allow;
        private List<String> deny;
        private PolicySources sources;

        @Data
        @Builder
        @NoArgsConstructor
        @AllArgsConstructor
        public static class PolicySources {
            private SandboxToolPolicySource allow;
            private SandboxToolPolicySource deny;
        }
    }

    // ── Browser config ──────────────────────────────────────────────

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxBrowserConfig {
        @Builder.Default
        private boolean enabled = false;
        private String image;
        private String containerPrefix;
        @Builder.Default
        private int cdpPort = 9222;
        @Builder.Default
        private int vncPort = 5900;
        @Builder.Default
        private int noVncPort = 6080;
        @Builder.Default
        private boolean headless = false;
        @Builder.Default
        private boolean enableNoVnc = true;
        @Builder.Default
        private boolean allowHostControl = false;
        @Builder.Default
        private boolean autoStart = true;
        @Builder.Default
        private int autoStartTimeoutMs = 12_000;
    }

    // ── Prune config ────────────────────────────────────────────────

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxPruneConfig {
        @Builder.Default
        private int idleHours = 24;
        @Builder.Default
        private int maxAgeDays = 7;
    }

    // ── Enums ────────────────────────────────────────────────────────

    public enum SandboxScope {
        SESSION, AGENT, SHARED
    }

    public enum SandboxWorkspaceAccess {
        NONE, RO, RW
    }

    public enum SandboxMode {
        OFF, NON_MAIN, ALL
    }

    // ── Composite configs ───────────────────────────────────────────

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxConfig {
        @Builder.Default
        private SandboxMode mode = SandboxMode.OFF;
        @Builder.Default
        private SandboxScope scope = SandboxScope.AGENT;
        @Builder.Default
        private SandboxWorkspaceAccess workspaceAccess = SandboxWorkspaceAccess.NONE;
        private String workspaceRoot;
        private SandboxDockerConfig docker;
        private SandboxBrowserConfig browser;
        private SandboxToolPolicy tools;
        private SandboxPruneConfig prune;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxBrowserContext {
        private String bridgeUrl;
        private String noVncUrl;
        private String containerName;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxContext {
        private boolean enabled;
        private String sessionKey;
        private String workspaceDir;
        private String agentWorkspaceDir;
        private SandboxWorkspaceAccess workspaceAccess;
        private String containerName;
        private String containerWorkdir;
        private SandboxDockerConfig docker;
        private SandboxToolPolicy tools;
        private boolean browserAllowHostControl;
        private SandboxBrowserContext browser;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxWorkspaceInfo {
        private String workspaceDir;
        private String containerWorkdir;
    }
}
