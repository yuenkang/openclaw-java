package com.openclaw.hooks;

/**
 * Hook engine facade — re-exports InternalHookRegistry operations
 * under simplified aliases.
 * Corresponds to TypeScript's hooks/hooks.ts which re-exports
 * internal-hooks.ts.
 */
public final class HookEngine {

    private final InternalHookRegistry registry;

    public HookEngine() {
        this.registry = new InternalHookRegistry();
    }

    public HookEngine(InternalHookRegistry registry) {
        this.registry = registry;
    }

    // --- Delegate methods ---

    public void registerHook(String eventKey, InternalHookRegistry.HookHandler handler) {
        registry.register(eventKey, handler);
    }

    public void unregisterHook(String eventKey, InternalHookRegistry.HookHandler handler) {
        registry.unregister(eventKey, handler);
    }

    public void clearHooks() {
        registry.clearAll();
    }

    public java.util.Set<String> getRegisteredHookEventKeys() {
        return registry.getRegisteredEventKeys();
    }

    public void triggerHook(InternalHookRegistry.HookEvent event) {
        registry.trigger(event);
    }

    public InternalHookRegistry.HookEvent createHookEvent(
            InternalHookRegistry.HookEventType type, String action, String sessionKey) {
        return InternalHookRegistry.createEvent(type, action, sessionKey);
    }

    public InternalHookRegistry.HookEvent createHookEvent(
            InternalHookRegistry.HookEventType type, String action,
            String sessionKey, java.util.Map<String, Object> context) {
        return InternalHookRegistry.createEvent(type, action, sessionKey, context);
    }

    /**
     * Get the underlying registry.
     */
    public InternalHookRegistry getRegistry() {
        return registry;
    }
}
