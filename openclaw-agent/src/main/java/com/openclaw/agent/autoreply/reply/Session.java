package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Session state initialization — resolve session key, reset triggers,
 * session freshness, delivery fields, and build the initial session context.
 * Mirrors {@code auto-reply/reply/session.ts}.
 */
public final class Session {

    private static final Logger log = LoggerFactory.getLogger(Session.class);

    private Session() {
    }

    /** Default reset trigger commands. */
    public static final List<String> DEFAULT_RESET_TRIGGERS = List.of("/new", "/reset");

    /** Result of session initialization. */
    public record SessionInitResult(
            Map<String, Object> sessionCtx,
            Map<String, Object> sessionEntry,
            Map<String, Object> previousSessionEntry,
            Map<String, Object> sessionStore,
            String sessionKey,
            String sessionId,
            boolean isNewSession,
            boolean resetTriggered,
            boolean systemSent,
            boolean abortedLastRun,
            String storePath,
            String sessionScope,
            boolean isGroup,
            String bodyStripped,
            String triggerBodyNormalized) {
    }

    /**
     * Initialize session state from the inbound message context.
     * Full integration deferred — returns a minimal result.
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<SessionInitResult> initSessionState(
            Map<String, Object> ctx,
            Map<String, Object> cfg,
            boolean commandAuthorized) {

        String sessionKey = (String) ctx.getOrDefault("SessionKey", "default");
        String sessionScope = "per-sender";

        // Check reset triggers
        String commandSource = coalesce(
                (String) ctx.get("BodyForCommands"),
                (String) ctx.get("CommandBody"),
                (String) ctx.get("RawBody"),
                (String) ctx.get("Body"),
                "");
        String triggerBodyNormalized = Mentions.stripStructuralPrefixes(commandSource).trim();

        boolean resetTriggered = false;
        boolean isNewSession = false;
        String bodyStripped = null;
        String trimmedLower = commandSource.trim().toLowerCase();

        for (String trigger : DEFAULT_RESET_TRIGGERS) {
            if (trigger == null || trigger.isEmpty())
                continue;
            String tLower = trigger.toLowerCase();
            if (trimmedLower.equals(tLower)) {
                isNewSession = true;
                bodyStripped = "";
                resetTriggered = true;
                break;
            }
            String prefix = tLower + " ";
            if (trimmedLower.startsWith(prefix)) {
                isNewSession = true;
                bodyStripped = commandSource.trim().substring(trigger.length()).trim();
                resetTriggered = true;
                break;
            }
        }

        // Resolve chat type
        Object chatType = ctx.get("ChatType");
        String ct = chatType instanceof String s ? s.toLowerCase() : null;
        boolean isGroup = ct != null && !"direct".equals(ct);

        // Build session entry
        String sessionId = UUID.randomUUID().toString();
        Map<String, Object> sessionStore = new HashMap<>();
        Map<String, Object> sessionEntry = new HashMap<>();
        sessionEntry.put("sessionId", sessionId);
        sessionEntry.put("updatedAt", System.currentTimeMillis());
        sessionEntry.put("systemSent", false);
        sessionEntry.put("abortedLastRun", false);

        if (isNewSession) {
            sessionEntry.put("compactionCount", 0);
        }

        sessionStore.put(sessionKey, sessionEntry);

        // Build session context
        Map<String, Object> sessionCtx = new HashMap<>(ctx);
        String strippedBody = bodyStripped != null ? bodyStripped
                : coalesce(
                        (String) ctx.get("BodyForAgent"),
                        (String) ctx.get("Body"),
                        (String) ctx.get("CommandBody"),
                        (String) ctx.get("RawBody"),
                        "");
        sessionCtx.put("BodyStripped", InboundSenderMeta.formatInboundBodyWithSenderMeta(
                InboundText.normalizeInboundTextNewlines(strippedBody), ctx));
        sessionCtx.put("SessionId", sessionId);
        sessionCtx.put("IsNewSession", isNewSession ? "true" : "false");

        Map<String, Object> previousSessionEntry = resetTriggered ? Map.of() : null;

        return CompletableFuture.completedFuture(new SessionInitResult(
                sessionCtx, sessionEntry, previousSessionEntry,
                sessionStore, sessionKey, sessionId,
                isNewSession, resetTriggered, false, false,
                "", sessionScope, isGroup, bodyStripped,
                triggerBodyNormalized));
    }

    private static String coalesce(String... values) {
        for (String v : values) {
            if (v != null)
                return v;
        }
        return "";
    }
}
