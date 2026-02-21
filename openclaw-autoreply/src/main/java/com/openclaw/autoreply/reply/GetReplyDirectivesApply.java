package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Apply inline directive overrides — determine whether a message is
 * directive-only (and return immediate reply) or extract directive
 * configuration for the agent run (model, think, verbose, elevated,
 * exec, queue overrides).
 * Mirrors {@code auto-reply/reply/get-reply-directives-apply.ts}.
 */
public final class GetReplyDirectivesApply {

    private static final Logger log = LoggerFactory.getLogger(GetReplyDirectivesApply.class);

    private GetReplyDirectivesApply() {
    }

    // --- Result types ---

    /** Result of applying directive overrides. */
    public sealed interface ApplyDirectiveResult {
        /** Directive-only: return this reply immediately. */
        record Reply(
                Object reply // ReplyPayload | ReplyPayload[] | null
        ) implements ApplyDirectiveResult {
        }

        /** Continue with agent run using resolved directive state. */
        record Continue(
                Map<String, Object> directives,
                String provider,
                String model,
                int contextTokens,
                Map<String, Object> directiveAck,
                String perMessageQueueMode,
                Map<String, Object> perMessageQueueOptions) implements ApplyDirectiveResult {
        }
    }

    // --- Main entry ---

    /**
     * Apply inline directive overrides for an inbound message.
     *
     * <ol>
     * <li>Strip directives from unauthorized senders</li>
     * <li>Check if message is directive-only → handle and return reply</li>
     * <li>Otherwise apply fast-lane directives (model/think/verbose/etc.)</li>
     * <li>Persist directive state to session</li>
     * <li>Return continue with resolved provider/model/contextTokens</li>
     * </ol>
     *
     * @param params directive application parameters
     * @return apply directive result
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<ApplyDirectiveResult> applyInlineDirectiveOverrides(
            Map<String, Object> params) {

        Map<String, Object> ctx = (Map<String, Object>) params.getOrDefault("ctx", Map.of());
        Map<String, Object> cfg = (Map<String, Object>) params.getOrDefault("cfg", Map.of());
        String agentId = (String) params.getOrDefault("agentId", "");
        Map<String, Object> command = (Map<String, Object>) params.getOrDefault("command", Map.of());
        Map<String, Object> directives = new LinkedHashMap<>(
                (Map<String, Object>) params.getOrDefault("directives", Map.of()));
        String provider = (String) params.getOrDefault("provider", "");
        String model = (String) params.getOrDefault("model", "");
        int contextTokens = params.get("contextTokens") instanceof Number n
                ? n.intValue()
                : 128_000;
        boolean isGroup = Boolean.TRUE.equals(params.get("isGroup"));

        boolean isAuthorizedSender = Boolean.TRUE.equals(command.get("isAuthorizedSender"));

        // 1. Strip directives from unauthorized senders
        if (!isAuthorizedSender) {
            directives.put("hasThinkDirective", false);
            directives.put("hasVerboseDirective", false);
            directives.put("hasReasoningDirective", false);
            directives.put("hasElevatedDirective", false);
            directives.put("hasExecDirective", false);
            directives.put("hasStatusDirective", false);
            directives.put("hasModelDirective", false);
            directives.put("hasQueueDirective", false);
            directives.put("queueReset", false);
        }

        // 2. Check directive-only
        String cleanedBody = directives.get("cleaned") != null
                ? directives.get("cleaned").toString().trim()
                : "";
        boolean isDirectiveOnly = isDirectiveOnlyMessage(directives, cleanedBody, ctx, cfg, agentId, isGroup);

        if (isDirectiveOnly) {
            if (!isAuthorizedSender) {
                return CompletableFuture.completedFuture(
                        new ApplyDirectiveResult.Reply(null));
            }
            // Full directive-only handling deferred
            log.debug("Directive-only message detected, handling");
            return CompletableFuture.completedFuture(
                    new ApplyDirectiveResult.Reply(null));
        }

        // 3. Apply fast-lane directives
        Map<String, Object> directiveAck = null;
        boolean hasAnyDirective = hasBoolFlag(directives, "hasThinkDirective")
                || hasBoolFlag(directives, "hasVerboseDirective")
                || hasBoolFlag(directives, "hasReasoningDirective")
                || hasBoolFlag(directives, "hasElevatedDirective")
                || hasBoolFlag(directives, "hasExecDirective")
                || hasBoolFlag(directives, "hasModelDirective")
                || hasBoolFlag(directives, "hasQueueDirective")
                || hasBoolFlag(directives, "hasStatusDirective");

        if (hasAnyDirective && isAuthorizedSender) {
            // Full applyInlineDirectivesFastLane integration deferred
            log.debug("Applying inline directives fast lane");
        }

        // 4. Persist inline directives (deferred)

        // 5. Resolve queue mode
        String perMessageQueueMode = hasBoolFlag(directives, "hasQueueDirective")
                && !hasBoolFlag(directives, "queueReset")
                        ? (String) directives.get("queueMode")
                        : null;
        Map<String, Object> perMessageQueueOptions = null;
        if (hasBoolFlag(directives, "hasQueueDirective") && !hasBoolFlag(directives, "queueReset")) {
            perMessageQueueOptions = new LinkedHashMap<>();
            perMessageQueueOptions.put("debounceMs", directives.get("debounceMs"));
            perMessageQueueOptions.put("cap", directives.get("cap"));
            perMessageQueueOptions.put("dropPolicy", directives.get("dropPolicy"));
        }

        return CompletableFuture.completedFuture(new ApplyDirectiveResult.Continue(
                directives, provider, model, contextTokens,
                directiveAck, perMessageQueueMode, perMessageQueueOptions));
    }

    // --- Directive-only detection ---

    static boolean isDirectiveOnlyMessage(
            Map<String, Object> directives, String cleanedBody,
            Map<String, Object> ctx, Map<String, Object> cfg,
            String agentId, boolean isGroup) {
        if (cleanedBody != null && !cleanedBody.trim().isEmpty()) {
            return false;
        }
        return hasBoolFlag(directives, "hasThinkDirective")
                || hasBoolFlag(directives, "hasVerboseDirective")
                || hasBoolFlag(directives, "hasReasoningDirective")
                || hasBoolFlag(directives, "hasElevatedDirective")
                || hasBoolFlag(directives, "hasExecDirective")
                || hasBoolFlag(directives, "hasModelDirective")
                || hasBoolFlag(directives, "hasStatusDirective")
                || hasBoolFlag(directives, "hasQueueDirective");
    }

    private static boolean hasBoolFlag(Map<String, Object> map, String key) {
        return Boolean.TRUE.equals(map.get(key));
    }
}
