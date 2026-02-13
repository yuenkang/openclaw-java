package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Directive-only handler — processes directive-only messages (no body text),
 * resolving model selection, persisting levels, and building ack replies.
 * Mirrors {@code auto-reply/reply/directive-handling.impl.ts}.
 */
public final class DirectiveHandlingImpl {

    private static final Logger log = LoggerFactory.getLogger(DirectiveHandlingImpl.class);

    private DirectiveHandlingImpl() {
    }

    /**
     * Handle a directive-only message — persist settings and return ack.
     *
     * @param cfg                   agent config
     * @param directives            parsed inline directives
     * @param sessionEntry          mutable session entry
     * @param sessionStore          mutable session store
     * @param sessionKey            current session key
     * @param storePath             session store file path (nullable)
     * @param elevatedEnabled       whether elevated mode is enabled
     * @param elevatedAllowed       whether elevated mode is allowed for this sender
     * @param defaultProvider       default LLM provider
     * @param defaultModel          default LLM model
     * @param provider              current provider
     * @param model                 current model
     * @param currentThinkLevel     current thinking level (nullable)
     * @param currentVerboseLevel   current verbose level (nullable)
     * @param currentReasoningLevel current reasoning level (nullable)
     * @param currentElevatedLevel  current elevated level (nullable)
     * @return reply payload or null
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<Map<String, Object>> handleDirectiveOnly(
            Map<String, Object> cfg,
            DirectiveHandlingParse.InlineDirectives directives,
            Map<String, Object> sessionEntry,
            Map<String, Object> sessionStore,
            String sessionKey,
            String storePath,
            boolean elevatedEnabled,
            boolean elevatedAllowed,
            String defaultProvider,
            String defaultModel,
            String provider,
            String model,
            String currentThinkLevel,
            String currentVerboseLevel,
            String currentReasoningLevel,
            String currentElevatedLevel) {

        String resolvedProvider = provider;
        String resolvedModel = model;

        // Thinking directive
        if (directives.hasThinkDirective()) {
            // No argument → show current
            if (directives.thinkLevel() == null && directives.rawThinkLevel() == null) {
                String level = currentThinkLevel != null ? currentThinkLevel : "off";
                return done(Map.of("text",
                        DirectiveHandlingShared.withOptions(
                                "Current thinking level: " + level + ".",
                                "off, low, medium, high, xhigh")));
            }
            if (directives.thinkLevel() == null) {
                return done(Map.of("text",
                        "Unrecognized thinking level \"" + directives.rawThinkLevel()
                                + "\". Valid levels: off, low, medium, high, xhigh."));
            }
        }

        // Verbose directive
        if (directives.hasVerboseDirective()) {
            if (directives.verboseLevel() == null && directives.rawVerboseLevel() == null) {
                String level = currentVerboseLevel != null ? currentVerboseLevel : "off";
                return done(Map.of("text",
                        DirectiveHandlingShared.withOptions(
                                "Current verbose level: " + level + ".", "on, full, off")));
            }
            if (directives.verboseLevel() == null) {
                return done(Map.of("text",
                        "Unrecognized verbose level \"" + directives.rawVerboseLevel()
                                + "\". Valid levels: off, on, full."));
            }
        }

        // Reasoning directive
        if (directives.hasReasoningDirective()) {
            if (directives.reasoningLevel() == null && directives.rawReasoningLevel() == null) {
                String level = currentReasoningLevel != null ? currentReasoningLevel : "off";
                return done(Map.of("text",
                        DirectiveHandlingShared.withOptions(
                                "Current reasoning level: " + level + ".", "on, off, stream")));
            }
            if (directives.reasoningLevel() == null) {
                return done(Map.of("text",
                        "Unrecognized reasoning level \"" + directives.rawReasoningLevel()
                                + "\". Valid levels: on, off, stream."));
            }
        }

        // Elevated directive
        if (directives.hasElevatedDirective()) {
            if (directives.elevatedLevel() == null && directives.rawElevatedLevel() == null) {
                if (!elevatedEnabled || !elevatedAllowed) {
                    return done(Map.of("text",
                            DirectiveHandlingShared.formatElevatedUnavailableText(false, List.of(), null)));
                }
                String level = currentElevatedLevel != null ? currentElevatedLevel : "off";
                return done(Map.of("text",
                        DirectiveHandlingShared.withOptions(
                                "Current elevated level: " + level + ".",
                                "on, off, ask, full")));
            }
            if (directives.elevatedLevel() == null) {
                return done(Map.of("text",
                        "Unrecognized elevated level \"" + directives.rawElevatedLevel()
                                + "\". Valid levels: off, on, ask, full."));
            }
        }

        if (directives.hasElevatedDirective() && (!elevatedEnabled || !elevatedAllowed)) {
            return done(Map.of("text",
                    DirectiveHandlingShared.formatElevatedUnavailableText(false, List.of(), null)));
        }

        // Persist levels
        if (directives.hasThinkDirective() && directives.thinkLevel() != null) {
            if ("off".equals(directives.thinkLevel())) {
                sessionEntry.remove("thinkingLevel");
            } else {
                sessionEntry.put("thinkingLevel", directives.thinkLevel());
            }
        }
        if (directives.hasVerboseDirective() && directives.verboseLevel() != null) {
            sessionEntry.put("verboseLevel", directives.verboseLevel());
        }
        if (directives.hasReasoningDirective() && directives.reasoningLevel() != null) {
            if ("off".equals(directives.reasoningLevel())) {
                sessionEntry.remove("reasoningLevel");
            } else {
                sessionEntry.put("reasoningLevel", directives.reasoningLevel());
            }
        }
        if (directives.hasElevatedDirective() && directives.elevatedLevel() != null) {
            sessionEntry.put("elevatedLevel", directives.elevatedLevel());
        }

        sessionEntry.put("updatedAt", System.currentTimeMillis());
        if (sessionStore != null && sessionKey != null) {
            sessionStore.put(sessionKey, sessionEntry);
        }

        // Build ack
        List<String> parts = new ArrayList<>();
        if (directives.hasThinkDirective() && directives.thinkLevel() != null) {
            parts.add("off".equals(directives.thinkLevel())
                    ? "Thinking disabled."
                    : "Thinking level set to " + directives.thinkLevel() + ".");
        }
        if (directives.hasVerboseDirective() && directives.verboseLevel() != null) {
            parts.add(DirectiveHandlingShared.formatDirectiveAck(
                    "off".equals(directives.verboseLevel())
                            ? "Verbose logging disabled."
                            : "full".equals(directives.verboseLevel())
                                    ? "Verbose logging set to full."
                                    : "Verbose logging enabled."));
        }
        if (directives.hasReasoningDirective() && directives.reasoningLevel() != null) {
            parts.add(DirectiveHandlingShared.formatDirectiveAck(
                    "off".equals(directives.reasoningLevel())
                            ? "Reasoning visibility disabled."
                            : "stream".equals(directives.reasoningLevel())
                                    ? "Reasoning stream enabled (Telegram only)."
                                    : "Reasoning visibility enabled."));
        }
        if (directives.hasElevatedDirective() && directives.elevatedLevel() != null) {
            parts.add(DirectiveHandlingShared.formatDirectiveAck(
                    "off".equals(directives.elevatedLevel())
                            ? "Elevated mode disabled."
                            : "full".equals(directives.elevatedLevel())
                                    ? "Elevated mode set to full (auto-approve)."
                                    : "Elevated mode set to ask (approvals may still apply)."));
        }
        if (directives.hasModelDirective() && directives.rawModelDirective() != null) {
            parts.add("Model set to " + directives.rawModelDirective() + ".");
        }

        String ack = String.join(" ", parts).trim();
        if (ack.isEmpty() && directives.hasStatusDirective()) {
            return done(null);
        }
        return done(Map.of("text", ack.isEmpty() ? "OK." : ack));
    }

    private static CompletableFuture<Map<String, Object>> done(Map<String, Object> payload) {
        return CompletableFuture.completedFuture(payload);
    }
}
