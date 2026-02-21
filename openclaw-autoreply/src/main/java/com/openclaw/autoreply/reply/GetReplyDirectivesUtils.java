package com.openclaw.autoreply.reply;

/**
 * Inline directive structure and factory for cleared/default state.
 * Mirrors {@code auto-reply/reply/get-reply-directives-utils.ts}.
 */
public final class GetReplyDirectivesUtils {

    private GetReplyDirectivesUtils() {
    }

    /** All inline directives extracted from a message body. */
    public record InlineDirectives(
            String cleaned,
            boolean hasThinkDirective,
            String thinkLevel,
            String rawThinkLevel,
            boolean hasVerboseDirective,
            String verboseLevel,
            String rawVerboseLevel,
            boolean hasReasoningDirective,
            String reasoningLevel,
            String rawReasoningLevel,
            boolean hasElevatedDirective,
            String elevatedLevel,
            String rawElevatedLevel,
            boolean hasExecDirective,
            String execHost,
            String execSecurity,
            String execAsk,
            String execNode,
            String rawExecHost,
            String rawExecSecurity,
            String rawExecAsk,
            String rawExecNode,
            boolean hasExecOptions,
            boolean invalidExecHost,
            boolean invalidExecSecurity,
            boolean invalidExecAsk,
            boolean invalidExecNode,
            boolean hasStatusDirective,
            boolean hasModelDirective,
            String rawModelDirective,
            boolean hasQueueDirective,
            String queueMode,
            boolean queueReset,
            String rawQueueMode,
            Integer debounceMs,
            Integer cap,
            String dropPolicy,
            String rawDebounce,
            String rawCap,
            String rawDrop,
            boolean hasQueueOptions) {
    }

    /** Create an InlineDirectives with all fields cleared (no directives). */
    public static InlineDirectives clearInlineDirectives(String cleaned) {
        return new InlineDirectives(
                cleaned, // cleaned
                false, null, null, // hasThinkDirective, thinkLevel, rawThinkLevel
                false, null, null, // hasVerboseDirective, verboseLevel, rawVerboseLevel
                false, null, null, // hasReasoningDirective, reasoningLevel, rawReasoningLevel
                false, null, null, // hasElevatedDirective, elevatedLevel, rawElevatedLevel
                false, // hasExecDirective
                null, null, null, null, // execHost, execSecurity, execAsk, execNode
                null, null, null, null, // rawExecHost, rawExecSecurity, rawExecAsk, rawExecNode
                false, // hasExecOptions
                false, false, false, false, // invalidExecHost/Security/Ask/Node
                false, // hasStatusDirective
                false, null, // hasModelDirective, rawModelDirective
                false, null, false, null, // hasQueueDirective, queueMode, queueReset, rawQueueMode
                null, null, null, // debounceMs, cap, dropPolicy
                null, null, null, // rawDebounce, rawCap, rawDrop
                false); // hasQueueOptions
    }
}
