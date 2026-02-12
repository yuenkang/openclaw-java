package com.openclaw.agent.runtime;

import java.util.List;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Types for embedded agent run results.
 * Corresponds to TypeScript's pi-embedded-runner/types.ts.
 */
public final class EmbeddedRunTypes {

    private EmbeddedRunTypes() {
    }

    // ── Agent-level metadata ───────────────────────────────────────────

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AgentMeta {
        private String sessionId;
        private String provider;
        private String model;
        @Builder.Default
        private int inputTokens = 0;
        @Builder.Default
        private int outputTokens = 0;
        @Builder.Default
        private int cacheReadTokens = 0;
        @Builder.Default
        private int cacheWriteTokens = 0;

        public int totalTokens() {
            return inputTokens + outputTokens;
        }
    }

    // ── Run-level metadata ─────────────────────────────────────────────

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RunMeta {
        private long durationMs;
        private AgentMeta agentMeta;
        @Builder.Default
        private boolean aborted = false;
        private String stopReason;
        private RunError error;
        private List<PendingToolCall> pendingToolCalls;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RunError {
        private ErrorKind kind;
        private String message;
    }

    public enum ErrorKind {
        CONTEXT_OVERFLOW,
        COMPACTION_FAILURE,
        ROLE_ORDERING,
        IMAGE_SIZE
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PendingToolCall {
        private String id;
        private String name;
        private String arguments;
    }

    // ── Run result ─────────────────────────────────────────────────────

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RunResult {
        private List<Payload> payloads;
        private RunMeta meta;
        @Builder.Default
        private boolean didSendViaMessagingTool = false;
        private List<String> messagingToolSentTexts;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Payload {
        private String text;
        private String mediaUrl;
        private List<String> mediaUrls;
        private String replyToId;
        @Builder.Default
        private boolean isError = false;
    }

    // ── Compaction result ──────────────────────────────────────────────

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CompactResult {
        @Builder.Default
        private boolean ok = false;
        @Builder.Default
        private boolean compacted = false;
        private String reason;
        private CompactDetail result;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CompactDetail {
        private String summary;
        private String firstKeptEntryId;
        @Builder.Default
        private int tokensBefore = 0;
        @Builder.Default
        private int tokensAfter = 0;
        private Map<String, Object> details;
    }

    // ── Sandbox info ───────────────────────────────────────────────────

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxInfo {
        @Builder.Default
        private boolean enabled = false;
        private String workspaceDir;
        private String workspaceAccess; // "none" | "ro" | "rw"
        private String agentWorkspaceMount;
        private String browserBridgeUrl;
        private String browserNoVncUrl;
        @Builder.Default
        private boolean hostBrowserAllowed = false;
        private ElevatedConfig elevated;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ElevatedConfig {
        @Builder.Default
        private boolean allowed = false;
        @Builder.Default
        private String defaultLevel = "off"; // "on" | "off" | "ask" | "full"
    }
}
