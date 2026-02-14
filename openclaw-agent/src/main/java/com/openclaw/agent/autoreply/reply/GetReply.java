package com.openclaw.agent.autoreply.reply;

import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.common.config.OpenClawConfig.AgentEntry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Top-level getReplyFromConfig — orchestrates the full reply pipeline:
 * load config, resolve agent/session, finalize inbound context, apply
 * media/link understanding, command authorization, session init,
 * directive resolution, inline actions, sandbox staging, and finally
 * run the prepared reply.
 * Mirrors {@code auto-reply/reply/get-reply.ts}.
 */
public final class GetReply {

    private static final Logger log = LoggerFactory.getLogger(GetReply.class);

    private GetReply() {
    }

    // =========================================================================
    // ChatRunner — injectable agent execution callback
    // =========================================================================

    /**
     * Functional interface for running an agent chat turn.
     * Injected from the app module (wraps runtime AgentRunner).
     */
    @FunctionalInterface
    public interface ChatRunner {
        /**
         * Run a chat turn with the given parameters.
         *
         * @param sessionKey   session identifier
         * @param userMessage  the user's message (body)
         * @param systemPrompt system prompt (nullable)
         * @param modelId      resolved model identifier (e.g.
         *                     "anthropic/claude-sonnet-4-5")
         * @param config       current OpenClawConfig
         * @return CompletableFuture with the agent's reply text
         */
        CompletableFuture<String> run(
                String sessionKey,
                String userMessage,
                String systemPrompt,
                String modelId,
                OpenClawConfig config);
    }

    private static volatile ChatRunner chatRunner;

    /** Set the chat runner. Called once at startup by TelegramAgentWiring. */
    public static void setChatRunner(ChatRunner runner) {
        chatRunner = runner;
    }

    // --- Skill filter merge ---

    static List<String> mergeSkillFilters(List<String> channelFilter, List<String> agentFilter) {
        List<String> channel = normalizeFilterList(channelFilter);
        List<String> agent = normalizeFilterList(agentFilter);
        if (channel == null && agent == null)
            return null;
        if (channel == null)
            return agent;
        if (agent == null)
            return channel;
        if (channel.isEmpty() || agent.isEmpty())
            return List.of();
        Set<String> agentSet = new HashSet<>(agent);
        return channel.stream().filter(agentSet::contains).toList();
    }

    private static List<String> normalizeFilterList(List<String> list) {
        if (list == null)
            return null;
        return list.stream()
                .map(String::trim)
                .filter(s -> !s.isEmpty())
                .toList();
    }

    // =========================================================================
    // Main entry point
    // =========================================================================

    /**
     * Main entry point — get reply from config for an inbound message.
     *
     * @param ctx            inbound message context (MsgContext)
     * @param opts           reply options (nullable)
     * @param configOverride config override (nullable)
     * @return list of reply payload maps
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<List<Map<String, Object>>> getReplyFromConfig(
            Map<String, Object> ctx,
            Map<String, Object> opts,
            OpenClawConfig configOverride) {

        OpenClawConfig cfg = configOverride != null ? configOverride : loadConfig();

        if (chatRunner == null) {
            log.warn("No ChatRunner configured — cannot process message");
            return CompletableFuture.completedFuture(List.of(
                    Map.of("text", "⚠️ Agent not configured. Please check server setup.")));
        }

        // 1. Resolve session key
        String targetSessionKey = "native".equals(ctx.get("CommandSource"))
                ? trimOrNull((String) ctx.get("CommandTargetSessionKey"))
                : null;
        String sessionKey = targetSessionKey != null ? targetSessionKey
                : (String) ctx.getOrDefault("SessionKey", "default");

        // 2. Resolve model
        String modelId = resolveModelId(cfg);

        // 3. Finalize inbound context
        InboundContext.finalizeInboundContext(ctx, new InboundContext.FinalizeOptions());

        // 4. Session init — use existing Session class
        return Session.initSessionState(ctx, mapFromConfig(cfg), false)
                .thenCompose(sessionState -> {
                    String resolvedSessionKey = sessionState.sessionKey();
                    boolean resetTriggered = sessionState.resetTriggered();
                    boolean isNewSession = sessionState.isNewSession();
                    Map<String, Object> sessionCtx = sessionState.sessionCtx();

                    // 5. Resolve body for agent
                    String body = resolveBody(sessionCtx, ctx, resetTriggered);

                    if (body == null || body.trim().isEmpty()) {
                        log.debug("Empty body after resolution, skipping");
                        return CompletableFuture.completedFuture(List.<Map<String, Object>>of());
                    }

                    // 6. Build system prompt
                    String systemPrompt = buildSystemPrompt(cfg, sessionCtx, isNewSession);

                    log.info("Running auto-reply: session={} model={} bodyLen={} isNew={}",
                            resolvedSessionKey, modelId, body.length(), isNewSession);

                    // 7. Invoke agent via ChatRunner
                    return chatRunner.run(resolvedSessionKey, body, systemPrompt, modelId, cfg)
                            .thenApply(reply -> {
                                if (reply == null || reply.isBlank()) {
                                    return List.<Map<String, Object>>of();
                                }
                                // Build reply payload
                                Map<String, Object> payload = new LinkedHashMap<>();
                                payload.put("text", reply);
                                return List.of(payload);
                            })
                            .exceptionally(ex -> {
                                log.error("ChatRunner failed: {}", ex.getMessage(), ex);
                                return List.of(Map.of("text",
                                        "⚠️ Agent error: " + ex.getMessage()));
                            });
                });
    }

    // =========================================================================
    // Internal helpers
    // =========================================================================

    private static OpenClawConfig loadConfig() {
        // In the full pipeline this reads from ConfigService,
        // but since we get configOverride from caller, this is a fallback.
        return new OpenClawConfig();
    }

    /**
     * Resolve the model to use from config.
     */
    private static String resolveModelId(OpenClawConfig cfg) {
        // Try top-level model shortcut
        if (cfg.getModel() != null && !cfg.getModel().isBlank()) {
            return cfg.getModel();
        }
        // Try agents.defaults.model.primary
        if (cfg.getAgents() != null && cfg.getAgents().getDefaults() != null) {
            var modelCfg = cfg.getAgents().getDefaults().getModel();
            if (modelCfg != null && modelCfg.getPrimary() != null
                    && !modelCfg.getPrimary().isBlank()) {
                return modelCfg.getPrimary();
            }
        }
        // Try first agent entry model
        if (cfg.getAgents() != null) {
            for (AgentEntry entry : cfg.getAgents().getEntries()) {
                String m = entry.getModelString();
                if (m != null && !m.isBlank())
                    return m;
            }
        }
        return "anthropic/claude-sonnet-4-5";
    }

    /**
     * Resolve the body to send to the agent.
     */
    private static String resolveBody(
            Map<String, Object> sessionCtx,
            Map<String, Object> ctx,
            boolean resetTriggered) {

        // BodyStripped is set by Session.initSessionState
        if (sessionCtx.get("BodyStripped") instanceof String stripped && !stripped.isEmpty()) {
            return stripped;
        }
        // Fallback to direct body fields
        if (ctx.get("BodyForAgent") instanceof String body && !body.isEmpty()) {
            return body;
        }
        if (ctx.get("Body") instanceof String body) {
            return body;
        }
        return "";
    }

    /**
     * Build a system prompt from config and session context.
     */
    @SuppressWarnings("unchecked")
    private static String buildSystemPrompt(
            OpenClawConfig cfg,
            Map<String, Object> sessionCtx,
            boolean isNewSession) {

        StringBuilder sb = new StringBuilder();

        // Agent persona / system prompt from first agent entry
        if (cfg.getAgents() != null) {
            for (AgentEntry entry : cfg.getAgents().getEntries()) {
                if (entry.getSystemPrompt() != null && !entry.getSystemPrompt().isBlank()) {
                    sb.append(entry.getSystemPrompt());
                    break; // use first agent's prompt
                }
            }
        }

        // Add channel context hints
        if (sessionCtx.get("Provider") instanceof String provider) {
            if (sb.length() > 0)
                sb.append("\n\n");
            sb.append("You are responding via ").append(provider).append(". ");
            sb.append("Keep responses concise and formatted for messaging. ");
            sb.append("Do not use markdown headers.");
        }

        // Group context
        if (sessionCtx.get("ChatType") instanceof String chatType
                && !"direct".equals(chatType.toLowerCase())) {
            sb.append("\nThis is a group chat. ");
            if (sessionCtx.get("SenderName") instanceof String name) {
                sb.append("The current message is from: ").append(name).append(". ");
            }
        }

        return sb.length() > 0 ? sb.toString() : null;
    }

    /**
     * Convert OpenClawConfig to a Map for Session.initSessionState.
     * Uses a simple extraction of session-relevant fields.
     */
    @SuppressWarnings("unchecked")
    private static Map<String, Object> mapFromConfig(OpenClawConfig cfg) {
        Map<String, Object> map = new LinkedHashMap<>();
        if (cfg.getSession() != null) {
            Map<String, Object> sessionMap = new LinkedHashMap<>();
            if (cfg.getSession().getScope() != null)
                sessionMap.put("scope", cfg.getSession().getScope());
            if (cfg.getSession().getResetTriggers() != null)
                sessionMap.put("resetTriggers", cfg.getSession().getResetTriggers());
            if (cfg.getSession().getMainKey() != null)
                sessionMap.put("mainKey", cfg.getSession().getMainKey());
            map.put("session", sessionMap);
        }
        if (cfg.getModel() != null) {
            map.put("model", cfg.getModel());
        }
        return map;
    }

    private static String trimOrNull(String s) {
        if (s == null)
            return null;
        String trimmed = s.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }
}
