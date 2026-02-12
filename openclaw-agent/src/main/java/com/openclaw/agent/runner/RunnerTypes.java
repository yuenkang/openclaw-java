package com.openclaw.agent.runner;

import java.util.List;
import java.util.Map;

/**
 * Data types for the embedded pi-agent runner.
 * Mirrors {@code agents/pi-embedded-runner/types.ts}.
 */
public final class RunnerTypes {

    private RunnerTypes() {
    }

    /** Usage counters for a single agent run. */
    public record AgentUsage(int input, int output, int cacheRead, int cacheWrite, int total) {
    }

    /** Metadata about the agent that was executed. */
    public record EmbeddedPiAgentMeta(
            String sessionId,
            String provider,
            String model,
            AgentUsage usage) {
    }

    /** Error classification for the run result. */
    public enum ErrorKind {
        CONTEXT_OVERFLOW, COMPACTION_FAILURE, ROLE_ORDERING, IMAGE_SIZE;

        public String toValue() {
            return name().toLowerCase();
        }

        public static ErrorKind fromValue(String value) {
            if (value == null)
                return null;
            return switch (value) {
                case "context_overflow" -> CONTEXT_OVERFLOW;
                case "compaction_failure" -> COMPACTION_FAILURE;
                case "role_ordering" -> ROLE_ORDERING;
                case "image_size" -> IMAGE_SIZE;
                default -> null;
            };
        }
    }

    /** Error details for the run result. */
    public record RunError(ErrorKind kind, String message) {
    }

    /** Pending tool call from an incomplete run. */
    public record PendingToolCall(String id, String name, String arguments) {
    }

    /** Metadata about a single run. */
    public record EmbeddedPiRunMeta(
            long durationMs,
            EmbeddedPiAgentMeta agentMeta,
            boolean aborted,
            RunError error,
            String stopReason,
            List<PendingToolCall> pendingToolCalls) {
    }

    /** A single output payload. */
    public record OutputPayload(
            String text,
            String mediaUrl,
            List<String> mediaUrls,
            String replyToId,
            boolean isError) {
    }

    /** Complete result of a pi-embedded run. */
    public record EmbeddedPiRunResult(
            List<OutputPayload> payloads,
            EmbeddedPiRunMeta meta,
            boolean didSendViaMessagingTool,
            List<String> messagingToolSentTexts) {
    }

    /** Result of a compaction operation. */
    public record EmbeddedPiCompactResult(
            boolean ok,
            boolean compacted,
            String reason,
            CompactDetail result) {
    }

    public record CompactDetail(
            String summary,
            String firstKeptEntryId,
            int tokensBefore,
            Integer tokensAfter) {
    }

    /** Sandbox info for embedded agent runs. */
    public record EmbeddedSandboxInfo(
            boolean enabled,
            String workspaceDir,
            String workspaceAccess, // "none" | "ro" | "rw"
            String agentWorkspaceMount,
            String browserBridgeUrl,
            String browserNoVncUrl,
            boolean hostBrowserAllowed,
            ElevatedConfig elevated) {
    }

    public record ElevatedConfig(boolean allowed, String defaultLevel) {
    }
}
