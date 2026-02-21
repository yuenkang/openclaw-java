package com.openclaw.plugin;

import com.openclaw.common.config.OpenClawConfig;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Plugin type definitions — enums, records, lifecycle-hook event types,
 * and registration helpers.
 * Corresponds to TypeScript's plugins/types.ts.
 */
public final class PluginTypes {

    private PluginTypes() {
    }

    // =========================================================================
    // Enums
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
        LOCAL("local"),
        CONFIG("config");

        private final String label;

        PluginOrigin(String label) {
            this.label = label;
        }

        public String label() {
            return label;
        }
    }

    public enum PluginStatus {
        LOADED, DISABLED, ERROR
    }

    public enum DiagnosticLevel {
        WARN, ERROR
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
        private Object entry; // HookEntry when in agent context
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
    // Plugin lifecycle hook names
    // =========================================================================

    public enum PluginHookName {
        BEFORE_AGENT_START,
        AGENT_END,
        BEFORE_COMPACTION,
        AFTER_COMPACTION,
        MESSAGE_RECEIVED,
        MESSAGE_SENDING,
        MESSAGE_SENT,
        BEFORE_TOOL_CALL,
        AFTER_TOOL_CALL,
        TOOL_RESULT_PERSIST,
        SESSION_START,
        SESSION_END,
        GATEWAY_START,
        GATEWAY_STOP;

        public String key() {
            return name().toLowerCase();
        }
    }

    // =========================================================================
    // Plugin lifecycle hook events
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AgentContext {
        private String agentId;
        private String sessionKey;
        private String workspaceDir;
        private String messageProvider;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class BeforeAgentStartEvent {
        private String prompt;
        private List<Object> messages;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class BeforeAgentStartResult {
        private String systemPrompt;
        private String prependContext;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AgentEndEvent {
        private List<Object> messages;
        private boolean success;
        private String error;
        private Long durationMs;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class BeforeCompactionEvent {
        private int messageCount;
        private Integer tokenCount;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AfterCompactionEvent {
        private int messageCount;
        private Integer tokenCount;
        private int compactedCount;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MessageContext {
        private String channelId;
        private String accountId;
        private String conversationId;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MessageReceivedEvent {
        private String from;
        private String content;
        private Long timestamp;
        private Map<String, Object> metadata;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MessageSendingEvent {
        private String to;
        private String content;
        private Map<String, Object> metadata;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MessageSendingResult {
        private String content;
        private Boolean cancel;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MessageSentEvent {
        private String to;
        private String content;
        private boolean success;
        private String error;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class BeforeToolCallEvent {
        private String toolName;
        private Map<String, Object> params;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class BeforeToolCallResult {
        private Map<String, Object> params;
        private Boolean block;
        private String blockReason;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AfterToolCallEvent {
        private String toolName;
        private Map<String, Object> params;
        private Object result;
        private String error;
        private Long durationMs;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SessionStartEvent {
        private String sessionId;
        private String resumedFrom;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SessionEndEvent {
        private String sessionId;
        private int messageCount;
        private Long durationMs;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class GatewayStartEvent {
        private int port;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class GatewayStopEvent {
        private String reason;
    }

    // =========================================================================
    // Plugin command definition
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginCommandDefinition {
        private String name;
        private String description;
        private boolean acceptsArgs;
        private boolean requireAuth;
    }
}
