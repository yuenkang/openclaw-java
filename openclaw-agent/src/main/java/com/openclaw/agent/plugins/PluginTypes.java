package com.openclaw.agent.plugins;

import com.openclaw.agent.hooks.HookTypes;
import com.openclaw.common.config.OpenClawConfig;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Plugin type definitions.
 * Corresponds to TypeScript's plugins/types.ts.
 */
public final class PluginTypes {

    private PluginTypes() {
    }

    // =========================================================================
    // Plugin kind
    // =========================================================================

    public enum PluginKind {
        CHANNEL("channel"),
        MEMORY("memory"),
        PROVIDER("provider"),
        TOOL("tool"),
        GENERAL("general");

        private final String label;

        PluginKind(String label) {
            this.label = label;
        }

        public String label() {
            return label;
        }

        public static PluginKind fromString(String s) {
            if (s == null)
                return GENERAL;
            return switch (s.toLowerCase()) {
                case "channel" -> CHANNEL;
                case "memory" -> MEMORY;
                case "provider" -> PROVIDER;
                case "tool" -> TOOL;
                default -> GENERAL;
            };
        }
    }

    public enum PluginOrigin {
        BUNDLED("bundled"),
        INSTALLED("installed"),
        WORKSPACE("workspace"),
        LOCAL("local");

        private final String label;

        PluginOrigin(String label) {
            this.label = label;
        }

        public String label() {
            return label;
        }
    }

    // =========================================================================
    // Plugin logger
    // =========================================================================

    public interface PluginLogger {
        default void debug(String message) {
        }

        void info(String message);

        void warn(String message);

        void error(String message);
    }

    // =========================================================================
    // Plugin config UI hints
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginConfigUiHint {
        private String label;
        private String help;
        private boolean advanced;
        private boolean sensitive;
        private String placeholder;
    }

    // =========================================================================
    // Plugin tool context
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginToolContext {
        private OpenClawConfig config;
        private String workspaceDir;
        private String agentDir;
        private String agentId;
        private String sessionKey;
        private String messageChannel;
        private String agentAccountId;
        private boolean sandboxed;
    }

    // =========================================================================
    // Plugin API (what plugins receive to register capabilities)
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginApi {
        private String id;
        private String source;
        private PluginLogger logger;
        private OpenClawConfig config;
        private Map<String, Object> pluginConfig;

        // Registration callbacks — set by the registry
        @Builder.Default
        private Consumer<HookRegistration> hookRegistrar = r -> {
        };
        @Builder.Default
        private Consumer<ToolRegistration> toolRegistrar = r -> {
        };
        @Builder.Default
        private Consumer<CommandRegistration> commandRegistrar = r -> {
        };
    }

    // =========================================================================
    // Registration types
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HookRegistration {
        private List<String> events;
        private HookTypes.HookEntry entry;
        private boolean register;
        private String name;
        private String description;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ToolRegistration {
        private String name;
        private List<String> names;
        private boolean optional;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CommandRegistration {
        private String name;
        private String description;
        private boolean acceptsArgs;
        private boolean requireAuth;
    }

    // =========================================================================
    // Plugin definition (what a plugin module exports)
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginDefinition {
        private String name;
        private Consumer<PluginApi> register;
    }

    // =========================================================================
    // Plugin diagnostic
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginDiagnostic {
        private String pluginId;
        private String level; // "info", "warn", "error"
        private String message;
    }
}
