package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Persist inline directives to session store — model overrides,
 * elevated/reasoning levels, profile switches.
 * Mirrors {@code auto-reply/reply/directive-handling.persist.ts}.
 */
public final class DirectiveHandlingPersist {

    private static final Logger log = LoggerFactory.getLogger(DirectiveHandlingPersist.class);

    private DirectiveHandlingPersist() {
    }

    /** Result of persisting inline directives. */
    public record PersistResult(String provider, String model, int contextTokens) {
    }

    /**
     * Persist inline directives to the session entry.
     * <p>
     * Handles model directive resolution, profile override,
     * elevated/reasoning level updates, and session store write-back.
     * Full session store persistence deferred.
     *
     * @param provider                current provider
     * @param model                   current model
     * @param contextTokens           current context token count
     * @param directives              parsed inline directives
     * @param effectiveModelDirective effective model directive (may differ from
     *                                raw)
     * @return updated provider/model/contextTokens
     */
    public static PersistResult persistInlineDirectives(
            String provider, String model, int contextTokens,
            DirectiveHandlingParse.InlineDirectives directives,
            String effectiveModelDirective) {

        String resultProvider = provider;
        String resultModel = model;
        int resultContextTokens = contextTokens;

        // Model directive override
        if (directives.hasModelDirective() && effectiveModelDirective != null) {
            log.debug("Model directive: {}", effectiveModelDirective);
            // Full model resolution deferred — use the directive value
            String[] parts = effectiveModelDirective.split("/", 2);
            if (parts.length == 2) {
                resultProvider = parts[0];
                resultModel = parts[1];
            } else {
                resultModel = effectiveModelDirective;
            }
        }

        // Elevated level
        if (directives.hasElevatedDirective()) {
            String level = directives.elevatedLevel();
            if (level != null) {
                log.debug("Elevated level: {}", level);
                String event = DirectiveHandlingShared.formatElevatedEvent(level);
                log.debug("Elevated event: {}", event);
            }
        }

        // Reasoning level
        if (directives.hasReasoningDirective()) {
            String level = directives.reasoningLevel();
            if (level != null) {
                log.debug("Reasoning level: {}", level);
                String event = DirectiveHandlingShared.formatReasoningEvent(level);
                log.debug("Reasoning event: {}", event);
            }
        }

        return new PersistResult(resultProvider, resultModel, resultContextTokens);
    }

    /**
     * Resolve default model from config.
     *
     * @return default provider/model (simplified — full resolution deferred)
     */
    public static PersistResult resolveDefaultModel(String provider, String model) {
        return new PersistResult(
                provider != null ? provider : "openai",
                model != null ? model : "gpt-4o",
                128_000);
    }
}
