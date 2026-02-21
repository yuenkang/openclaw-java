package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Run prepared reply — build the final agent run context from
 * resolved directives+session state, apply session hints, group
 * intro, media notes, think-level resolution, skill snapshots,
 * queue settings, and invoke {@link AgentRunner#runReplyAgent}.
 * Mirrors {@code auto-reply/reply/get-reply-run.ts}.
 */
public final class GetReplyRun {

    private static final Logger log = LoggerFactory.getLogger(GetReplyRun.class);

    private static final String BARE_SESSION_RESET_PROMPT = "A new session was started via /new or /reset. Greet the user in your configured persona, if one is provided. Be yourself - use your defined voice, mannerisms, and mood. Keep it to 1-3 sentences and ask what they want to do. If the runtime model differs from default_model in the system prompt, mention the default model. Do not mention internal steps, files, tools, or reasoning.";

    private GetReplyRun() {
    }

    /**
     * Run the prepared reply pipeline.
     *
     * <ol>
     * <li>Resolve typing mode, group intro, extra system prompt</li>
     * <li>Detect bare /new or /reset → use reset prompt</li>
     * <li>Apply session hints (abort, system events)</li>
     * <li>Append media notes</li>
     * <li>Resolve think level from body prefix or defaults</li>
     * <li>Ensure skill snapshot</li>
     * <li>Resolve queue settings + lane state</li>
     * <li>Build followup run descriptor</li>
     * <li>Invoke runReplyAgent</li>
     * </ol>
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<Object> runPreparedReply(
            Map<String, Object> params) {

        Map<String, Object> ctx = (Map<String, Object>) params.getOrDefault("ctx", Map.of());
        Map<String, Object> sessionCtx = (Map<String, Object>) params.getOrDefault("sessionCtx", Map.of());
        Map<String, Object> cfg = (Map<String, Object>) params.getOrDefault("cfg", Map.of());
        Map<String, Object> agentCfg = (Map<String, Object>) params.get("agentCfg");
        Map<String, Object> sessionCfg = (Map<String, Object>) params.get("sessionCfg");
        String agentId = (String) params.getOrDefault("agentId", "");
        String agentDir = (String) params.getOrDefault("agentDir", "");
        String provider = (String) params.getOrDefault("provider", "openai");
        String model = (String) params.getOrDefault("model", "gpt-4o");
        String defaultProvider = (String) params.getOrDefault("defaultProvider", "openai");
        String defaultModel = (String) params.getOrDefault("defaultModel", "gpt-4o");
        String sessionKey = (String) params.getOrDefault("sessionKey", "");
        String sessionId = (String) params.get("sessionId");
        String storePath = (String) params.get("storePath");
        String workspaceDir = (String) params.getOrDefault("workspaceDir", "");
        boolean isNewSession = Boolean.TRUE.equals(params.get("isNewSession"));
        boolean resetTriggered = Boolean.TRUE.equals(params.get("resetTriggered"));
        boolean systemSent = Boolean.TRUE.equals(params.get("systemSent"));
        int timeoutMs = params.get("timeoutMs") instanceof Number n ? n.intValue() : 120_000;
        Map<String, Object> opts = (Map<String, Object>) params.get("opts");
        Map<String, Object> directives = (Map<String, Object>) params.getOrDefault("directives", Map.of());
        Map<String, Object> sessionEntry = (Map<String, Object>) params.get("sessionEntry");
        Map<String, Object> sessionStore = (Map<String, Object>) params.get("sessionStore");
        boolean blockStreamingEnabled = Boolean.TRUE.equals(params.get("blockStreamingEnabled"));
        String resolvedBlockStreamingBreak = (String) params.getOrDefault("resolvedBlockStreamingBreak", "text_end");
        String resolvedVerboseLevel = (String) params.get("resolvedVerboseLevel");
        String resolvedElevatedLevel = (String) params.getOrDefault("resolvedElevatedLevel", "off");
        boolean elevatedEnabled = Boolean.TRUE.equals(params.get("elevatedEnabled"));
        boolean elevatedAllowed = Boolean.TRUE.equals(params.get("elevatedAllowed"));

        // 1. Resolve typing mode + group intro
        String chatType = sessionCtx.get("ChatType") instanceof String s ? s : "";
        boolean isGroupChat = "group".equals(chatType);
        boolean isHeartbeat = opts != null && Boolean.TRUE.equals(opts.get("isHeartbeat"));
        boolean isFirstTurnInSession = isNewSession || !systemSent;
        boolean shouldInjectGroupIntro = isGroupChat && isFirstTurnInSession;
        String extraSystemPrompt = shouldInjectGroupIntro
                ? resolveGroupIntro(cfg, sessionCtx, sessionEntry)
                : "";

        // 2. Detect bare /new or /reset
        String baseBody = sessionCtx.get("BodyStripped") instanceof String s
                ? s
                : (sessionCtx.get("Body") instanceof String s2 ? s2 : "");
        String rawBody = (ctx.get("CommandBody") instanceof String s
                ? s
                : (ctx.get("RawBody") instanceof String s2
                        ? s2
                        : (ctx.get("Body") instanceof String s3 ? s3 : "")))
                .trim();
        boolean isBareNewOrReset = "/new".equals(rawBody) || "/reset".equals(rawBody);
        boolean isBareSessionReset = isNewSession
                && ((baseBody.trim().isEmpty() && !rawBody.isEmpty()) || isBareNewOrReset);
        String baseBodyFinal = isBareSessionReset ? BARE_SESSION_RESET_PROMPT : baseBody;

        if (baseBodyFinal.trim().isEmpty()) {
            log.debug("Inbound body empty after normalization; skipping agent run");
            return CompletableFuture.completedFuture(Map.of(
                    "text", "I didn't receive any text in your message. Please resend or add a caption."));
        }

        // 3. Apply session hints (deferred)
        String prefixedCommandBody = baseBodyFinal;

        // 4. Resolve think level
        String resolvedThinkLevel = (String) params.get("resolvedThinkLevel");

        // 5. Ensure skill snapshot (deferred)

        // 6. Resolve queue settings + build followup run
        String sessionIdFinal = sessionId != null ? sessionId : UUID.randomUUID().toString();
        String queueKey = sessionKey.isEmpty() ? sessionIdFinal : sessionKey;

        Map<String, Object> run = new LinkedHashMap<>();
        run.put("agentId", agentId);
        run.put("agentDir", agentDir);
        run.put("sessionId", sessionIdFinal);
        run.put("sessionKey", sessionKey);
        run.put("provider", provider);
        run.put("model", model);
        run.put("thinkLevel", resolvedThinkLevel);
        run.put("verboseLevel", resolvedVerboseLevel);
        run.put("reasoningLevel", params.get("resolvedReasoningLevel"));
        run.put("elevatedLevel", resolvedElevatedLevel);
        run.put("config", cfg);
        run.put("workspaceDir", workspaceDir);
        run.put("timeoutMs", timeoutMs);
        run.put("extraSystemPrompt", extraSystemPrompt.isEmpty() ? null : extraSystemPrompt);
        run.put("bashElevated", Map.of(
                "enabled", elevatedEnabled,
                "allowed", elevatedAllowed,
                "defaultLevel", resolvedElevatedLevel != null ? resolvedElevatedLevel : "off"));

        Map<String, Object> followupRun = new LinkedHashMap<>();
        followupRun.put("prompt", prefixedCommandBody);
        followupRun.put("messageId", sessionCtx.get("MessageSidFull") != null
                ? sessionCtx.get("MessageSidFull")
                : sessionCtx.get("MessageSid"));
        followupRun.put("run", run);

        // 7. Invoke runReplyAgent
        String typingMode = resolveTypingMode(sessionCfg, agentCfg, isGroupChat, isHeartbeat);

        AgentRunner.RunReplyAgentParams agentParams = new AgentRunner.RunReplyAgentParams(
                prefixedCommandBody, followupRun, queueKey,
                Map.of("mode", "direct"), false, false,
                false, false, opts != null ? opts : Map.of(),
                Map.of(), sessionEntry, sessionStore,
                sessionKey, storePath, defaultModel, null,
                resolvedVerboseLevel != null ? resolvedVerboseLevel : "off",
                isNewSession, blockStreamingEnabled, null,
                resolvedBlockStreamingBreak, sessionCtx,
                shouldInjectGroupIntro, typingMode);

        return AgentRunner.runReplyAgent(agentParams);
    }

    // --- Helpers ---

    private static String resolveGroupIntro(
            Map<String, Object> cfg,
            Map<String, Object> sessionCtx,
            Map<String, Object> sessionEntry) {
        String groupSystemPrompt = sessionCtx.get("GroupSystemPrompt") instanceof String s
                ? s.trim()
                : "";
        return groupSystemPrompt;
    }

    private static String resolveTypingMode(
            Map<String, Object> sessionCfg,
            Map<String, Object> agentCfg,
            boolean isGroupChat, boolean isHeartbeat) {
        String configured = null;
        if (sessionCfg != null && sessionCfg.get("typingMode") instanceof String s)
            configured = s;
        else if (agentCfg != null && agentCfg.get("typingMode") instanceof String s)
            configured = s;
        if (configured != null)
            return configured;
        return isHeartbeat ? "silent" : (isGroupChat ? "on-mention" : "eager");
    }
}
