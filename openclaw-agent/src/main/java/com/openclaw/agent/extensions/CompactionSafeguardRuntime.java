package com.openclaw.agent.extensions;

import java.util.Map;
import java.util.WeakHashMap;

/**
 * Session-scoped compaction safeguard runtime registry.
 * Mirrors pi-extensions/compaction-safeguard-runtime.ts.
 */
public final class CompactionSafeguardRuntime {

    private CompactionSafeguardRuntime() {
    }

    public record Value(
            Double maxHistoryShare,
            Integer contextWindowTokens) {
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
}
