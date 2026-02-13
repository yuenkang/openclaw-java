package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Resolve reply directives — parse inline directives from the inbound
 * message, resolve model selection state, compute think/verbose/
 * reasoning/elevated levels, determine block streaming settings,
 * and delegate to {@link GetReplyDirectivesApply} for override
 * application.
 * Mirrors {@code auto-reply/reply/get-reply-directives.ts}.
 */
public final class GetReplyDirectives {

        private static final Logger log = LoggerFactory.getLogger(GetReplyDirectives.class);

        private GetReplyDirectives() {
        }

        // --- Result types ---

        /** Resolved directive state for agent run. */
        public record ReplyDirectiveContinuation(
                        String commandSource,
                        Map<String, Object> command,
                        boolean allowTextCommands,
                        List<Map<String, Object>> skillCommands,
                        Map<String, Object> directives,
                        String cleanedBody,
                        String messageProviderKey,
                        boolean elevatedEnabled,
                        boolean elevatedAllowed,
                        List<Map<String, String>> elevatedFailures,
                        Object defaultActivation,
                        String resolvedThinkLevel,
                        String resolvedVerboseLevel,
                        String resolvedReasoningLevel,
                        String resolvedElevatedLevel,
                        Map<String, Object> execOverrides,
                        boolean blockStreamingEnabled,
                        Map<String, Object> blockReplyChunking,
                        String resolvedBlockStreamingBreak,
                        String provider,
                        String model,
                        Object modelState,
                        int contextTokens,
                        boolean inlineStatusRequested,
                        Map<String, Object> directiveAck,
                        String perMessageQueueMode,
                        Map<String, Object> perMessageQueueOptions) {
        }

        /** Result: either immediate reply or continue with resolved state. */
        public sealed interface ReplyDirectiveResult {
                record Reply(Object reply) implements ReplyDirectiveResult {
                }

                record Continue(ReplyDirectiveContinuation result) implements ReplyDirectiveResult {
                }
        }

        // --- Exec overrides ---

        static Map<String, Object> resolveExecOverrides(
                        Map<String, Object> directives, Map<String, Object> sessionEntry) {
                String host = firstNonNull(
                                directives.get("execHost"),
                                sessionEntry != null ? sessionEntry.get("execHost") : null);
                String security = firstNonNull(
                                directives.get("execSecurity"),
                                sessionEntry != null ? sessionEntry.get("execSecurity") : null);
                String ask = firstNonNull(
                                directives.get("execAsk"),
                                sessionEntry != null ? sessionEntry.get("execAsk") : null);
                Object node = firstNonNull(
                                directives.get("execNode"),
                                sessionEntry != null ? sessionEntry.get("execNode") : null);
                if (host == null && security == null && ask == null && node == null) {
                        return null;
                }
                Map<String, Object> overrides = new LinkedHashMap<>();
                if (host != null)
                        overrides.put("host", host);
                if (security != null)
                        overrides.put("security", security);
                if (ask != null)
                        overrides.put("ask", ask);
                if (node != null)
                        overrides.put("node", node);
                return overrides;
        }

        // --- Main entry ---

        /**
         * Resolve reply directives for an inbound message.
         *
         * <ol>
         * <li>Determine command source (clean body vs raw body)</li>
         * <li>Build command context</li>
         * <li>Parse inline directives (think, verbose, model, queue, etc.)</li>
         * <li>Strip directives from unauthorized senders / group mentions</li>
         * <li>Resolve elevated permissions</li>
         * <li>Resolve model selection state</li>
         * <li>Apply inline directive overrides</li>
         * <li>Return continuation or immediate reply</li>
         * </ol>
         */
        @SuppressWarnings("unchecked")
        public static CompletableFuture<ReplyDirectiveResult> resolveReplyDirectives(
                        Map<String, Object> params) {

                Map<String, Object> ctx = (Map<String, Object>) params.getOrDefault("ctx", Map.of());
                Map<String, Object> cfg = (Map<String, Object>) params.getOrDefault("cfg", Map.of());
                Map<String, Object> sessionCtx = (Map<String, Object>) params.getOrDefault("sessionCtx", Map.of());
                Map<String, Object> sessionEntry = (Map<String, Object>) params.get("sessionEntry");
                Map<String, Object> agentCfg = (Map<String, Object>) params.get("agentCfg");
                String agentId = (String) params.getOrDefault("agentId", "");
                String agentDir = (String) params.getOrDefault("agentDir", "");
                boolean isGroup = Boolean.TRUE.equals(params.get("isGroup"));
                String defaultProvider = (String) params.getOrDefault("defaultProvider", "openai");
                String defaultModel = (String) params.getOrDefault("defaultModel", "gpt-4o");
                String initialProvider = (String) params.getOrDefault("provider", defaultProvider);
                String initialModel = (String) params.getOrDefault("model", defaultModel);

                // 1. Determine command source
                String commandSource = firstNonNullStr(
                                sessionCtx.get("BodyForCommands"),
                                sessionCtx.get("CommandBody"),
                                sessionCtx.get("RawBody"),
                                sessionCtx.get("Transcript"),
                                sessionCtx.get("BodyStripped"),
                                sessionCtx.get("Body"),
                                ctx.get("BodyForCommands"),
                                ctx.get("CommandBody"),
                                ctx.get("RawBody"));
                if (commandSource == null)
                        commandSource = "";

                String promptSource = firstNonNullStr(
                                sessionCtx.get("BodyForAgent"),
                                sessionCtx.get("BodyStripped"),
                                sessionCtx.get("Body"));
                if (promptSource == null)
                        promptSource = "";

                String commandText = commandSource.isEmpty() ? promptSource : commandSource;

                // 2. Build command context (deferred — placeholder)
                boolean commandAuthorized = Boolean.TRUE.equals(params.get("commandAuthorized"));
                Map<String, Object> command = new LinkedHashMap<>();
                command.put("isAuthorizedSender", commandAuthorized);
                command.put("commandBodyNormalized", commandText.trim().toLowerCase());

                // 3. Parse inline directives (deferred — placeholder)
                Map<String, Object> directives = new LinkedHashMap<>();
                directives.put("cleaned", commandText);

                // 4. Resolve elevated permissions
                String messageProviderKey = sessionCtx.get("Provider") != null
                                ? sessionCtx.get("Provider").toString().trim().toLowerCase()
                                : (ctx.get("Provider") != null
                                                ? ctx.get("Provider").toString().trim().toLowerCase()
                                                : "");
                boolean elevatedEnabled = false;
                boolean elevatedAllowed = false;
                List<Map<String, String>> elevatedFailures = List.of();

                // 5. Resolve think/verbose/reasoning/elevated levels
                String resolvedThinkLevel = firstNonNullStr(
                                directives.get("thinkLevel"),
                                sessionEntry != null ? sessionEntry.get("thinkingLevel") : null,
                                agentCfg != null ? agentCfg.get("thinkingDefault") : null);
                String resolvedVerboseLevel = firstNonNullStr(
                                directives.get("verboseLevel"),
                                sessionEntry != null ? sessionEntry.get("verboseLevel") : null,
                                agentCfg != null ? agentCfg.get("verboseDefault") : null);
                String resolvedReasoningLevel = firstNonNullStr(
                                directives.get("reasoningLevel"),
                                sessionEntry != null ? sessionEntry.get("reasoningLevel") : null);
                if (resolvedReasoningLevel == null)
                        resolvedReasoningLevel = "off";
                String resolvedElevatedLevel = elevatedAllowed
                                ? firstNonNullStr(
                                                directives.get("elevatedLevel"),
                                                sessionEntry != null ? sessionEntry.get("elevatedLevel") : null,
                                                agentCfg != null ? agentCfg.get("elevatedDefault") : null,
                                                "on")
                                : "off";

                // 6. Resolve block streaming
                Map<String, Object> opts = (Map<String, Object>) params.get("opts");
                boolean disableBlockStreaming = opts != null && Boolean.TRUE.equals(opts.get("disableBlockStreaming"));
                boolean enableBlockStreaming = opts != null && Boolean.FALSE.equals(opts.get("disableBlockStreaming"));
                String blockStreamDefault = agentCfg != null
                                ? (String) agentCfg.getOrDefault("blockStreamingDefault", "off")
                                : "off";
                String resolvedBlockStreaming = disableBlockStreaming ? "off"
                                : enableBlockStreaming ? "on"
                                                : "on".equals(blockStreamDefault) ? "on" : "off";
                String resolvedBlockStreamingBreak = agentCfg != null
                                && "message_end".equals(agentCfg.get("blockStreamingBreak"))
                                                ? "message_end"
                                                : "text_end";
                boolean blockStreamingEnabled = "on".equals(resolvedBlockStreaming) && !disableBlockStreaming;

                // 7. Resolve model selection state (deferred)
                String provider = initialProvider;
                String model = initialModel;
                int contextTokens = agentCfg != null && agentCfg.get("contextTokens") instanceof Number n
                                ? n.intValue()
                                : 128000;

                // 8. Apply inline directive overrides (deferred)
                Map<String, Object> execOverrides = resolveExecOverrides(directives, sessionEntry);

                // 9. Build result
                ReplyDirectiveContinuation continuation = new ReplyDirectiveContinuation(
                                commandText, command, true, List.of(),
                                directives, commandText, messageProviderKey,
                                elevatedEnabled, elevatedAllowed, elevatedFailures,
                                null, resolvedThinkLevel, resolvedVerboseLevel,
                                resolvedReasoningLevel, resolvedElevatedLevel,
                                execOverrides, blockStreamingEnabled, null,
                                resolvedBlockStreamingBreak, provider, model, null,
                                contextTokens, false, null, null, null);

                return CompletableFuture.completedFuture(
                                new ReplyDirectiveResult.Continue(continuation));
        }

        // --- Utility ---

        private static String firstNonNull(Object... values) {
                for (Object v : values) {
                        if (v != null)
                                return v.toString();
                }
                return null;
        }

        private static String firstNonNullStr(Object... values) {
                for (Object v : values) {
                        if (v instanceof String s && !s.isEmpty())
                                return s;
                }
                return null;
        }
}
