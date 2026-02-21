package com.openclaw.hooks;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * Hook type definitions for workspace-level hooks.
 * Corresponds to TypeScript's hooks/types.ts.
 */
public final class HookTypes {

    private HookTypes() {
    }

    // =========================================================================
    // Hook source
    // =========================================================================

    public enum HookSource {
        OPENCLAW_BUNDLED,
        OPENCLAW_MANAGED,
        OPENCLAW_WORKSPACE,
        OPENCLAW_PLUGIN;

        public String label() {
            return name().toLowerCase().replace('_', '-');
        }

        public static HookSource fromLabel(String label) {
            if (label == null)
                return OPENCLAW_WORKSPACE;
            return switch (label.trim().toLowerCase()) {
                case "openclaw-bundled" -> OPENCLAW_BUNDLED;
                case "openclaw-managed" -> OPENCLAW_MANAGED;
                case "openclaw-plugin" -> OPENCLAW_PLUGIN;
                default -> OPENCLAW_WORKSPACE;
            };
        }
    }

    // =========================================================================
    // Install spec
    // =========================================================================

    public enum InstallKind {
        BUNDLED, NPM, GIT
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HookInstallSpec {
        private String id;
        private InstallKind kind;
        private String label;
        private String packageName;
        private String repository;
        private List<String> bins;
    }

    // =========================================================================
    // Hook metadata
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HookMetadata {
        private Boolean always;
        private String hookKey;
        private String emoji;
        private String homepage;
        private List<String> events;
        private String exportName;
        private List<String> os;
        private HookRequirements requires;
        private List<HookInstallSpec> install;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HookRequirements {
        private List<String> bins;
        private List<String> anyBins;
        private List<String> env;
        private List<String> config;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HookInvocationPolicy {
        private boolean enabled;
    }

    // =========================================================================
    // Hook definition
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Hook {
        private String name;
        private String description;
        private HookSource source;
        private String pluginId;
        private String filePath;
        private String baseDir;
        private String handlerPath;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HookEntry {
        private Hook hook;
        private Map<String, String> frontmatter;
        private HookMetadata metadata;
        private HookInvocationPolicy invocation;
    }

    // =========================================================================
    // Hook snapshot
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HookSnapshot {
        private List<HookSnapshotItem> hooks;
        private List<Hook> resolvedHooks;
        private Integer version;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HookSnapshotItem {
        private String name;
        private List<String> events;
    }
}
