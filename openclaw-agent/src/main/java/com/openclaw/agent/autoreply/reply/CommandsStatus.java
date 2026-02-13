package com.openclaw.agent.autoreply.reply;

import com.openclaw.agent.autoreply.AutoReplyTypes;

import java.util.ArrayList;
import java.util.List;

/**
 * Build /status reply with agent, session, model, context, and queue info.
 * Mirrors {@code auto-reply/reply/commands-status.ts}.
 */
public final class CommandsStatus {

    private CommandsStatus() {
    }

    /**
     * Format an API key snippet (first 4 + last 4 characters).
     */
    public static String formatApiKeySnippet(String apiKey) {
        if (apiKey == null || apiKey.length() < 12)
            return "***";
        return apiKey.substring(0, 4) + "…" + apiKey.substring(apiKey.length() - 4);
    }

    /** Parameters for building a status reply. */
    public record StatusParams(
            String provider,
            String model,
            int contextTokens,
            String sessionKey,
            String agentId,
            String agentDir,
            String thinkLevel,
            String verboseLevel,
            String reasoningLevel,
            String elevatedLevel,
            boolean isGroup,
            String groupActivation,
            String channel,
            String surface,
            Integer totalTokens,
            Integer inputTokens,
            Integer outputTokens,
            Integer compactionCount,
            Integer queueDepth,
            String queueMode) {
    }

    /**
     * Build a /status reply payload.
     */
    public static AutoReplyTypes.ReplyPayload buildStatusReply(StatusParams params) {
        List<String> lines = new ArrayList<>();

        // Header
        lines.add("📊 Session Status");
        lines.add("");

        // Model & provider
        lines.add("**Model**: " + (params.model() != null ? params.model() : "default"));
        lines.add("**Provider**: " + (params.provider() != null ? params.provider() : "default"));

        // Context
        String ctxLine = "**Context**: " + formatTokenCount(params.contextTokens());
        if (params.totalTokens() != null && params.totalTokens() > 0) {
            ctxLine += " (used: " + formatTokenCount(params.totalTokens()) + ")";
        }
        lines.add(ctxLine);

        // Agent
        if (params.agentId() != null && !params.agentId().isEmpty()) {
            lines.add("**Agent**: " + params.agentId());
        }

        // Session
        if (params.sessionKey() != null && !params.sessionKey().isEmpty()) {
            String shortKey = params.sessionKey().length() > 32
                    ? params.sessionKey().substring(0, 32) + "…"
                    : params.sessionKey();
            lines.add("**Session**: " + shortKey);
        }

        // Directives / levels
        if (params.thinkLevel() != null && !params.thinkLevel().isEmpty()) {
            lines.add("**Think**: " + params.thinkLevel());
        }
        if (params.elevatedLevel() != null && !params.elevatedLevel().isEmpty()) {
            lines.add("**Elevated**: " + params.elevatedLevel());
        }

        // Group info
        if (params.isGroup()) {
            lines.add("**Group**: " + (params.groupActivation() != null ? params.groupActivation() : "mention"));
        }

        // Channel
        if (params.channel() != null && !params.channel().isEmpty()) {
            lines.add("**Channel**: " + params.channel());
        }

        // Queue
        if (params.queueDepth() != null && params.queueDepth() > 0) {
            String qLine = "**Queue**: " + params.queueDepth() + " pending";
            if (params.queueMode() != null)
                qLine += " (" + params.queueMode() + ")";
            lines.add(qLine);
        }

        // Compaction
        if (params.compactionCount() != null && params.compactionCount() > 0) {
            lines.add("**Compacted**: " + params.compactionCount() + " time(s)");
        }

        String text = String.join("\n", lines);
        return new AutoReplyTypes.ReplyPayload(text, null, null, null, false, false, false, false, null);
    }

    /**
     * Format a token count for display.
     */
    public static String formatTokenCount(int tokens) {
        if (tokens >= 1_000_000) {
            return String.format("%.1fM", tokens / 1_000_000.0);
        }
        if (tokens >= 1_000) {
            return String.format("%.1fk", tokens / 1_000.0);
        }
        return String.valueOf(tokens);
    }

    /**
     * Format context usage as a short summary.
     */
    public static String formatContextUsageShort(Integer totalTokens, Integer contextTokens) {
        if (totalTokens == null && contextTokens == null)
            return "context: n/a";
        String used = totalTokens != null && totalTokens > 0 ? formatTokenCount(totalTokens) : "?";
        String limit = contextTokens != null && contextTokens > 0 ? formatTokenCount(contextTokens) : "?";
        return "context: " + used + "/" + limit;
    }
}
