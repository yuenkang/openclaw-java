package com.openclaw.agent.extensions;

import java.util.Map;
import java.util.WeakHashMap;
import java.util.function.Predicate;

/**
 * Session-scoped context pruning runtime registry.
 * Uses WeakHashMap (Java equivalent of TS WeakMap) keyed by session manager
 * identity.
 * Mirrors pi-extensions/context-pruning/runtime.ts.
 */
public final class ContextPruningRuntime {

    private ContextPruningRuntime() {
    }

    /**
     * Runtime value attached to a session.
     */
    public record Value(
            ContextPruningSettings.Effective settings,
            Integer contextWindowTokens,
            Predicate<String> isToolPrunable,
            Long lastCacheTouchAt) {
        public Value withLastCacheTouchAt(Long ts) {
            return new Value(settings, contextWindowTokens, isToolPrunable, ts);
        }
    }

    private static final Map<Object, Value> REGISTRY = new WeakHashMap<>();

    public static synchronized void set(Object sessionManager, Value value) {
        if (sessionManager == null)
            return;
        if (value == null) {
            REGISTRY.remove(sessionManager);
        } else {
            REGISTRY.put(sessionManager, value);
        }
    }

    public static synchronized Value get(Object sessionManager) {
        if (sessionManager == null)
            return null;
        return REGISTRY.get(sessionManager);
    }

    public static synchronized void remove(Object sessionManager) {
        if (sessionManager != null)
            REGISTRY.remove(sessionManager);
    }
}
