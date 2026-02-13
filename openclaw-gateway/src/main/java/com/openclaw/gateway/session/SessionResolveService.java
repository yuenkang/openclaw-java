package com.openclaw.gateway.session;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

/**
 * Resolves a session key from various input parameters (key, sessionId, label).
 * <p>
 * Corresponds to TypeScript's {@code sessions-resolve.ts} (140 lines).
 */
@Slf4j
public class SessionResolveService {

    private final ConfigService configService;
    private final SessionStore sessionStore;

    public SessionResolveService(ConfigService configService, SessionStore sessionStore) {
        this.configService = configService;
        this.sessionStore = sessionStore;
    }

    /**
     * Resolve a session key from the given parameters.
     * Exactly one of key, sessionId, or label must be provided.
     *
     * @param key       session key (e.g. "agent:default:main")
     * @param sessionId UUID session ID
     * @param label     session label
     * @return ResolveResult with either the resolved key or an error
     */
    public ResolveResult resolve(String key, String sessionId, String label) {
        var cfg = configService.loadConfig();

        boolean hasKey = key != null && !key.isBlank();
        boolean hasSessionId = sessionId != null && !sessionId.isBlank();
        boolean hasLabel = label != null && !label.isBlank();

        int selectionCount = (hasKey ? 1 : 0) + (hasSessionId ? 1 : 0) + (hasLabel ? 1 : 0);

        if (selectionCount > 1) {
            return ResolveResult.error("INVALID_REQUEST",
                    "Provide either key, sessionId, or label (not multiple)");
        }
        if (selectionCount == 0) {
            return ResolveResult.error("INVALID_REQUEST",
                    "Either key, sessionId, or label is required");
        }

        if (hasKey) {
            return resolveByKey(cfg, key.trim());
        }

        if (hasSessionId) {
            return resolveBySessionId(sessionId.trim());
        }

        return resolveByLabel(label.trim());
    }

    private ResolveResult resolveByKey(OpenClawConfig cfg, String key) {
        String canonicalKey = SessionUtils.resolveSessionStoreKey(cfg, key);

        // Check if the session exists in the in-memory store
        var session = sessionStore.findBySessionKey(canonicalKey);
        if (session.isEmpty()) {
            // Also try the raw key
            session = sessionStore.findBySessionKey(key);
        }

        if (session.isEmpty()) {
            return ResolveResult.error("INVALID_REQUEST", "No session found: " + key);
        }

        return ResolveResult.ok(canonicalKey);
    }

    private ResolveResult resolveBySessionId(String sessionId) {
        var session = sessionStore.getSession(sessionId);
        if (session.isEmpty()) {
            return ResolveResult.error("INVALID_REQUEST",
                    "No session found: " + sessionId);
        }
        return ResolveResult.ok(session.get().getSessionKey());
    }

    private ResolveResult resolveByLabel(String label) {
        // Search all sessions for matching label
        var matches = sessionStore.listSessions().stream()
                .filter(s -> label.equals(s.getLabel()))
                .toList();

        if (matches.isEmpty()) {
            return ResolveResult.error("INVALID_REQUEST",
                    "No session found with label: " + label);
        }
        if (matches.size() > 1) {
            var keys = matches.stream()
                    .map(s -> s.getSessionKey())
                    .toList();
            return ResolveResult.error("INVALID_REQUEST",
                    "Multiple sessions found with label: " + label
                            + " (" + String.join(", ", keys) + ")");
        }

        return ResolveResult.ok(matches.get(0).getSessionKey());
    }

    // ─── Result types ───────────────────────────────────────────────────

    public sealed interface ResolveResult permits ResolveResult.Ok, ResolveResult.Err {
        static ResolveResult ok(String key) {
            return new Ok(key);
        }

        static ResolveResult error(String code, String message) {
            return new Err(code, message);
        }

        record Ok(String key) implements ResolveResult {
        }

        record Err(String code, String message) implements ResolveResult {
        }
    }
}
