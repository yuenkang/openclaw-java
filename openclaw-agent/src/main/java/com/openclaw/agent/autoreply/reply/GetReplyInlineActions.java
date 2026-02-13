package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Handle inline actions — resolve skill command invocations,
 * execute tool-dispatch skill commands, handle inline simple
 * commands, extract abort memory, and run the main command
 * handler before the agent turn.
 * Mirrors {@code auto-reply/reply/get-reply-inline-actions.ts}.
 */
public final class GetReplyInlineActions {

    private static final Logger log = LoggerFactory.getLogger(GetReplyInlineActions.class);

    private GetReplyInlineActions() {
    }

    /** Result of inline action handling. */
    public sealed interface InlineActionResult {
        record Reply(Object reply) implements InlineActionResult {
        }

        record Continue(Map<String, Object> directives, boolean abortedLastRun)
                implements InlineActionResult {
        }
    }

    @SuppressWarnings("unchecked")
    static String extractTextFromToolResult(Object result) {
        if (!(result instanceof Map<?, ?> map))
            return null;
        Object content = map.get("content");
        if (content instanceof String s) {
            String t = s.trim();
            return t.isEmpty() ? null : t;
        }
        if (content instanceof List<?> list) {
            StringBuilder sb = new StringBuilder();
            for (Object block : list) {
                if (block instanceof Map<?, ?> rec
                        && "text".equals(rec.get("type"))
                        && rec.get("text") instanceof String text) {
                    sb.append(text);
                }
            }
            String out = sb.toString().trim();
            return out.isEmpty() ? null : out;
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    public static CompletableFuture<InlineActionResult> handleInlineActions(
            Map<String, Object> params) {
        Map<String, Object> command = (Map<String, Object>) params.getOrDefault("command", Map.of());
        Map<String, Object> directives = new LinkedHashMap<>(
                (Map<String, Object>) params.getOrDefault("directives", Map.of()));
        boolean abortedLastRun = Boolean.TRUE.equals(params.get("abortedLastRun"));
        boolean allowTextCommands = Boolean.TRUE.equals(params.get("allowTextCommands"));
        boolean isAuthorized = Boolean.TRUE.equals(command.get("isAuthorizedSender"));
        String cmdNorm = command.get("commandBodyNormalized") instanceof String s ? s : "";

        // Skill invocation (deferred)
        if (cmdNorm.startsWith("/") && allowTextCommands) {
            log.debug("Checking skill commands for: {}", cmdNorm);
        }
        // Inline simple commands (deferred)
        if (allowTextCommands && isAuthorized) {
            log.debug("Checking inline simple commands");
        }
        // Inline status (deferred)
        if (Boolean.TRUE.equals(params.get("inlineStatusRequested"))) {
            directives.put("hasStatusDirective", false);
        }
        // Main command handler (deferred)
        log.debug("Running main command handler (integration deferred)");

        return CompletableFuture.completedFuture(
                new InlineActionResult.Continue(directives, abortedLastRun));
    }
}
