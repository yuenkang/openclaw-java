package com.openclaw.hooks;

import lombok.extern.slf4j.Slf4j;

import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Internal hook event system for OpenClaw agent lifecycle events.
 * Provides an extensible event-driven system for agent events like
 * command processing, session lifecycle, agent bootstrap, etc.
 * Corresponds to TypeScript's internal-hooks.ts + loader.ts.
 */
@Slf4j
public class InternalHookRegistry {

    // =========================================================================
    // Event types
    // =========================================================================

    /**
     * Top-level hook event categories.
     */
    public enum HookEventType {
        COMMAND,
        SESSION,
        AGENT,
        GATEWAY;

        public String key() {
            return name().toLowerCase();
        }
    }

    // =========================================================================
    // Event model
    // =========================================================================

    /**
     * A hook event carrying type, action, session context, and mutable messages
     * list.
     */
    public static class HookEvent {
        private final HookEventType type;
        private final String action;
        private final String sessionKey;
        private final Map<String, Object> context;
        private final Instant timestamp;
        private final List<String> messages;

        public HookEvent(HookEventType type, String action, String sessionKey,
                Map<String, Object> context) {
            this.type = type;
            this.action = action;
            this.sessionKey = sessionKey;
            this.context = context != null ? context : Map.of();
            this.timestamp = Instant.now();
            this.messages = new ArrayList<>();
        }

        public HookEventType getType() {
            return type;
        }

        public String getAction() {
            return action;
        }

        public String getSessionKey() {
            return sessionKey;
        }

        public Map<String, Object> getContext() {
            return context;
        }

        public Instant getTimestamp() {
            return timestamp;
        }

        public List<String> getMessages() {
            return messages;
        }

        /** Composite event key: "type:action" */
        public String eventKey() {
            return type.key() + ":" + action;
        }
    }

    // =========================================================================
    // Handler interface
    // =========================================================================

    /**
     * Functional interface for hook handlers.
     */
    @FunctionalInterface
    public interface HookHandler {
        void handle(HookEvent event) throws Exception;
    }

    // =========================================================================
    // Registry
    // =========================================================================

    private final Map<String, List<HookHandler>> handlers = new ConcurrentHashMap<>();

    /**
     * Register a hook handler for a specific event key.
     * <p>
     * The event key can be a type-level key (e.g. "agent") to receive all events
     * of that type, or a specific "type:action" key (e.g. "agent:bootstrap") to
     * receive only that specific action.
     *
     * @param eventKey event type or type:action string
     * @param handler  the handler to register
     */
    public void register(String eventKey, HookHandler handler) {
        handlers.computeIfAbsent(eventKey, k -> new CopyOnWriteArrayList<>()).add(handler);
    }

    /**
     * Unregister a specific hook handler.
     *
     * @param eventKey the event key the handler was registered for
     * @param handler  the handler to remove
     */
    public void unregister(String eventKey, HookHandler handler) {
        List<HookHandler> list = handlers.get(eventKey);
        if (list == null)
            return;
        list.remove(handler);
        if (list.isEmpty()) {
            handlers.remove(eventKey);
        }
    }

    /**
     * Clear all registered hooks.
     */
    public void clearAll() {
        handlers.clear();
    }

    /**
     * Get all registered event keys (for debugging).
     */
    public Set<String> getRegisteredEventKeys() {
        return Set.copyOf(handlers.keySet());
    }

    // =========================================================================
    // Trigger
    // =========================================================================

    /**
     * Trigger a hook event. Calls all handlers registered for:
     * 1. The general event type (e.g. "agent")
     * 2. The specific type:action combination (e.g. "agent:bootstrap")
     * <p>
     * Handlers are called in registration order. Errors are caught and logged
     * but do not prevent other handlers from running.
     */
    public void trigger(HookEvent event) {
        String typeKey = event.getType().key();
        String specificKey = event.eventKey();

        List<HookHandler> typeHandlers = handlers.getOrDefault(typeKey, List.of());
        List<HookHandler> specificHandlers = handlers.getOrDefault(specificKey, List.of());

        if (typeHandlers.isEmpty() && specificHandlers.isEmpty()) {
            return;
        }

        List<HookHandler> allHandlers = new ArrayList<>(typeHandlers.size() + specificHandlers.size());
        allHandlers.addAll(typeHandlers);
        allHandlers.addAll(specificHandlers);

        for (HookHandler handler : allHandlers) {
            try {
                handler.handle(event);
            } catch (Exception e) {
                log.error("Hook error [{}]: {}", specificKey,
                        e.getMessage() != null ? e.getMessage() : e.toString());
            }
        }
    }

    // =========================================================================
    // Factory methods
    // =========================================================================

    /**
     * Create a hook event with common fields.
     */
    public static HookEvent createEvent(HookEventType type, String action,
            String sessionKey) {
        return createEvent(type, action, sessionKey, null);
    }

    /**
     * Create a hook event with context.
     */
    public static HookEvent createEvent(HookEventType type, String action,
            String sessionKey,
            Map<String, Object> context) {
        return new HookEvent(type, action, sessionKey, context);
    }

    /**
     * Create an agent bootstrap event.
     */
    public static HookEvent createAgentBootstrapEvent(
            String sessionKey, String workspaceDir, String agentId) {
        return createEvent(HookEventType.AGENT, "bootstrap", sessionKey,
                Map.of("workspaceDir", workspaceDir, "agentId", agentId));
    }

    /**
     * Check if an event is an agent bootstrap event.
     */
    public static boolean isAgentBootstrapEvent(HookEvent event) {
        return event.getType() == HookEventType.AGENT
                && "bootstrap".equals(event.getAction())
                && event.getContext().containsKey("workspaceDir");
    }
}
