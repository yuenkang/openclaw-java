package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

/**
 * Fast-lane inline directive application — determines whether a directive-only
 * message should short-circuit the reply flow and returns provider/model
 * overrides.
 * Mirrors {@code auto-reply/reply/directive-handling.fast-lane.ts}.
 */
public final class DirectiveHandlingFastLane {

    private static final Logger log = LoggerFactory.getLogger(DirectiveHandlingFastLane.class);

    private DirectiveHandlingFastLane() {
    }

    /** Result of fast-lane directive application. */
    public record FastLaneResult(
            AutoReplyTypes.ReplyPayload directiveAck,
            String provider,
            String model) {
    }

    /**
     * Apply inline directives via the fast lane.
     * <p>
     * If the message is directive-only (e.g. just {@code /thinking high}),
     * the fast lane returns a short acknowledgement and updated provider/model
     * without sending the message to the LLM.
     *
     * @param directives        parsed inline directives
     * @param commandAuthorized whether the sender is authorized for commands
     * @param isGroup           whether the message comes from a group chat
     * @param provider          current provider
     * @param model             current model
     * @param sessionEntry      current session entry (nullable)
     * @param agentCfg          agent-level config overrides (nullable)
     * @return fast-lane result with optional ack and updated provider/model
     */
    public static FastLaneResult applyInlineDirectivesFastLane(
            DirectiveHandlingParse.InlineDirectives directives,
            boolean commandAuthorized,
            boolean isGroup,
            String provider,
            String model,
            Map<String, Object> sessionEntry,
            Map<String, Object> agentCfg) {

        // Not authorized → no fast-lane
        if (!commandAuthorized) {
            return new FastLaneResult(null, provider, model);
        }

        // Check if the message is directive-only (no actual text content)
        boolean directiveOnly = DirectiveHandlingParse.isDirectiveOnly(
                directives, directives.cleaned(), isGroup);
        if (!directiveOnly) {
            return new FastLaneResult(null, provider, model);
        }

        // Directive-only: persist and ack
        // Resolve current levels from session
        String currentThinkLevel = sessionEntry != null
                ? (String) sessionEntry.getOrDefault("thinkingLevel", null)
                : null;
        String currentVerboseLevel = sessionEntry != null
                ? (String) sessionEntry.getOrDefault("verboseLevel", null)
                : null;
        String currentReasoningLevel = sessionEntry != null
                ? (String) sessionEntry.getOrDefault("reasoningLevel", "off")
                : "off";
        String currentElevatedLevel = sessionEntry != null
                ? (String) sessionEntry.getOrDefault("elevatedLevel", null)
                : null;

        // Build acknowledgement lines
        StringBuilder ack = new StringBuilder();
        if (directives.hasThinkDirective()) {
            String level = directives.thinkLevel() != null ? directives.thinkLevel() : "default";
            ack.append("Thinking → ").append(level);
            if (currentThinkLevel != null) {
                ack.append(" (was ").append(currentThinkLevel).append(")");
            }
            ack.append("\n");
        }
        if (directives.hasVerboseDirective()) {
            String level = directives.verboseLevel() != null ? directives.verboseLevel() : "default";
            ack.append("Verbose → ").append(level);
            if (currentVerboseLevel != null) {
                ack.append(" (was ").append(currentVerboseLevel).append(")");
            }
            ack.append("\n");
        }
        if (directives.hasReasoningDirective()) {
            String level = directives.reasoningLevel() != null ? directives.reasoningLevel() : "off";
            ack.append("Reasoning → ").append(level);
            if (currentReasoningLevel != null) {
                ack.append(" (was ").append(currentReasoningLevel).append(")");
            }
            ack.append("\n");
        }
        if (directives.hasElevatedDirective()) {
            String level = directives.elevatedLevel() != null ? directives.elevatedLevel() : "off";
            ack.append("Elevated → ").append(level);
            if (currentElevatedLevel != null) {
                ack.append(" (was ").append(currentElevatedLevel).append(")");
            }
            ack.append("\n");
        }
        if (directives.hasModelDirective()) {
            ack.append("Model → ").append(directives.rawModelDirective()).append("\n");
        }
        if (directives.hasStatusDirective()) {
            ack.append("(status display deferred)\n");
        }

        // Apply session overrides
        String resultProvider = provider;
        String resultModel = model;
        if (sessionEntry != null) {
            Object po = sessionEntry.get("providerOverride");
            if (po instanceof String s && !s.isEmpty())
                resultProvider = s;
            Object mo = sessionEntry.get("modelOverride");
            if (mo instanceof String s && !s.isEmpty())
                resultModel = s;
        }

        String ackText = ack.toString().trim();
        AutoReplyTypes.ReplyPayload ackPayload = ackText.isEmpty() ? null
                : new AutoReplyTypes.ReplyPayload(
                        DirectiveHandlingShared.formatDirectiveAck(ackText),
                        null, null, null, false, false, false, false, null);

        return new FastLaneResult(ackPayload, resultProvider, resultModel);
    }
}
