package com.openclaw.agent.autoreply.reply;

/**
 * Parse inline directives from message text — think, verbose, reasoning,
 * elevated, exec, status, model, queue directives.
 * Mirrors {@code auto-reply/reply/directive-handling.parse.ts}.
 */
public final class DirectiveHandlingParse {

    private DirectiveHandlingParse() {
    }

    /** Result of parsing all inline directives. */
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
            boolean hasStatusDirective,
            boolean hasModelDirective,
            String rawModelDirective,
            boolean hasQueueDirective,
            String queueMode,
            boolean queueReset) {
    }

    /**
     * Parse inline directives from the message body.
     * <p>
     * Chains through each directive extractor, each consuming its tag
     * and returning the cleaned remainder.
     *
     * @param body            the raw message body
     * @param disableElevated whether to skip elevated directive parsing
     * @return parsed directives with cleaned text
     */
    public static InlineDirectives parseInlineDirectives(
            String body, boolean disableElevated) {

        // Think directive
        Directives.ExtractedLevel<String> think = Directives.extractThinkDirective(body);
        // Verbose directive
        Directives.ExtractedLevel<String> verbose = Directives.extractVerboseDirective(think.cleaned());
        // Reasoning directive
        Directives.ExtractedLevel<String> reasoning = Directives.extractReasoningDirective(verbose.cleaned());
        // Elevated directive
        Directives.ExtractedLevel<String> elevated;
        if (disableElevated) {
            elevated = new Directives.ExtractedLevel<>(reasoning.cleaned(), null, null, false);
        } else {
            elevated = Directives.extractElevatedDirective(reasoning.cleaned());
        }
        // Status directive
        Directives.SimpleDirectiveResult status = Directives.extractStatusDirective(elevated.cleaned());

        // Model & queue directive extraction deferred — no matching extractor yet
        String finalCleaned = status.cleaned();

        return new InlineDirectives(
                finalCleaned,
                think.hasDirective(),
                think.level(),
                think.rawLevel(),
                verbose.hasDirective(),
                verbose.level(),
                verbose.rawLevel(),
                reasoning.hasDirective(),
                reasoning.level(),
                reasoning.rawLevel(),
                elevated.hasDirective(),
                elevated.level(),
                elevated.rawLevel(),
                status.hasDirective(),
                false, // hasModelDirective — deferred
                null, // rawModelDirective — deferred
                false, // hasQueueDirective — deferred
                null, // queueMode — deferred
                false); // queueReset — deferred
    }

    /**
     * Check if a message is directive-only (no actual text content).
     */
    public static boolean isDirectiveOnly(InlineDirectives directives, String cleanedBody,
            boolean isGroup) {
        if (!directives.hasThinkDirective()
                && !directives.hasVerboseDirective()
                && !directives.hasReasoningDirective()
                && !directives.hasElevatedDirective()
                && !directives.hasModelDirective()
                && !directives.hasQueueDirective()) {
            return false;
        }
        String stripped = Mentions.stripStructuralPrefixes(cleanedBody != null ? cleanedBody : "");
        return stripped.isEmpty();
    }
}
