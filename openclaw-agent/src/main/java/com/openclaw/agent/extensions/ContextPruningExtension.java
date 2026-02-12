package com.openclaw.agent.extensions;

import java.util.List;
import java.util.Map;

/**
 * Context pruning extension entry point.
 * Registers as a "context" event handler that prunes messages in-memory before
 * they are sent to the model, without rewriting session history on disk.
 * Mirrors pi-extensions/context-pruning/extension.ts.
 */
public final class ContextPruningExtension {

    private ContextPruningExtension() {
    }

    /**
     * Process context event: check runtime settings, apply pruning if needed.
     *
     * @param messages           current context messages
     * @param sessionManager     session manager object (used as WeakMap key)
     * @param modelContextWindow model's context window in tokens (nullable)
     * @return pruned messages, or the original list if no pruning is needed
     */
    public static List<Map<String, Object>> onContext(
            List<Map<String, Object>> messages,
            Object sessionManager,
            Integer modelContextWindow) {

        ContextPruningRuntime.Value runtime = ContextPruningRuntime.get(sessionManager);
        if (runtime == null)
            return messages;

        ContextPruningSettings.Effective settings = runtime.settings();

        // For cache-ttl mode, check if cache has expired
        if (settings.getMode() == ContextPruningSettings.Mode.CACHE_TTL) {
            long ttlMs = settings.getTtlMs();
            Long lastTouch = runtime.lastCacheTouchAt();
            if (lastTouch == null || ttlMs <= 0)
                return messages;
            if (ttlMs > 0 && System.currentTimeMillis() - lastTouch < ttlMs)
                return messages;
        }

        List<Map<String, Object>> next = ContextPruner.pruneContextMessages(
                messages,
                settings,
                runtime.contextWindowTokens(),
                modelContextWindow,
                runtime.isToolPrunable());

        if (next == messages)
            return messages;

        // Update cache touch time
        if (settings.getMode() == ContextPruningSettings.Mode.CACHE_TTL) {
            ContextPruningRuntime.set(sessionManager,
                    runtime.withLastCacheTouchAt(System.currentTimeMillis()));
        }

        return next;
    }
}
