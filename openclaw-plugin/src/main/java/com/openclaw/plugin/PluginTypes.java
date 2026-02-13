package com.openclaw.plugin;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * Plugin type definitions — enums, records, and lifecycle-hook event types.
 * Corresponds to TypeScript's plugins/types.ts.
 */
public final class PluginTypes {

    private PluginTypes() {
    }

    // =========================================================================
    // Enums
    // =========================================================================

    public enum PluginOrigin {
        BUNDLED, GLOBAL, WORKSPACE, CONFIG
    }

    public enum PluginKind {
        MEMORY
    }

    public enum PluginStatus {
        LOADED, DISABLED, ERROR
    }

    public enum DiagnosticLevel {
        WARN, ERROR
    }

    // =========================================================================
    // Plugin record (registered plugin metadata)
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginRecord {
        private String id;
        private String name;
        private String version;
        private String description;
        private PluginKind kind;
        private String source;
        private PluginOrigin origin;
        private String workspaceDir;
        private boolean enabled;
        private PluginStatus status;
        private String error;
        private List<String> toolNames;
        private List<String> channels;
        private List<String> providers;
        private List<String> cliCommands;
        private List<String> services;
        private List<String> commands;
        private int httpHandlers;
        private int hookCount;
        private boolean configSchema;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginDiagnostic {
        private DiagnosticLevel level;
        private String message;
        private String pluginId;
        private String source;
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

    // =========================================================================
    // Plugin config UI hint
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginConfigUiHint {
        private String label;
        private String help;
        private Boolean advanced;
        private Boolean sensitive;
        private String placeholder;
    }
}
