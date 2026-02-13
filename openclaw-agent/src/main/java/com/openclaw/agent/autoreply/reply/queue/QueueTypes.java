package com.openclaw.agent.autoreply.reply.queue;

import java.util.List;
import java.util.Map;

/**
 * Queue system types — modes, settings, followup runs.
 * Mirrors {@code auto-reply/reply/queue/types.ts}.
 */
public final class QueueTypes {

    private QueueTypes() {
    }

    /** Queue operating mode. */
    public static final String MODE_STEER = "steer";
    public static final String MODE_FOLLOWUP = "followup";
    public static final String MODE_COLLECT = "collect";
    public static final String MODE_STEER_BACKLOG = "steer-backlog";
    public static final String MODE_INTERRUPT = "interrupt";
    public static final String MODE_QUEUE = "queue";

    /** Queue drop policy. */
    public static final String DROP_OLD = "old";
    public static final String DROP_NEW = "new";
    public static final String DROP_SUMMARIZE = "summarize";

    /** Queue dedupe mode. */
    public static final String DEDUPE_MESSAGE_ID = "message-id";
    public static final String DEDUPE_PROMPT = "prompt";
    public static final String DEDUPE_NONE = "none";

    /** Queue settings resolved for a session. */
    public record QueueSettings(
            String mode,
            Integer debounceMs,
            Integer cap,
            String dropPolicy) {
    }

    /** Parameters for resolving queue settings. */
    public record ResolveQueueSettingsParams(
            Map<String, Object> cfg,
            String channel,
            Map<String, Object> sessionEntry,
            String inlineMode,
            QueueSettings inlineOptions) {
    }

    /** A single followup run queued for replay. */
    public record FollowupRun(
            String prompt,
            String messageId,
            String summaryLine,
            long enqueuedAt,
            String originatingChannel,
            String originatingTo,
            String originatingAccountId,
            Object originatingThreadId,
            String originatingChatType,
            FollowupRunConfig run) {
    }

    /** Configuration snapshot for a followup run. */
    public record FollowupRunConfig(
            String agentId,
            String agentDir,
            String sessionId,
            String sessionKey,
            String messageProvider,
            String agentAccountId,
            String groupId,
            String groupChannel,
            String groupSpace,
            String senderId,
            String senderName,
            String senderUsername,
            String senderE164,
            String sessionFile,
            String workspaceDir,
            Map<String, Object> config,
            Map<String, Object> skillsSnapshot,
            String provider,
            String model,
            String authProfileId,
            String authProfileIdSource,
            String thinkLevel,
            String verboseLevel,
            String reasoningLevel,
            String elevatedLevel,
            Map<String, String> execOverrides,
            BashElevatedConfig bashElevated,
            long timeoutMs,
            String blockReplyBreak,
            List<String> ownerNumbers,
            String extraSystemPrompt,
            boolean enforceFinalTag) {
    }

    /** Bash elevated permissions snapshot. */
    public record BashElevatedConfig(
            boolean enabled,
            boolean allowed,
            String defaultLevel) {
    }
}
