package com.openclaw.agent.runtime.subscribe;

import java.util.Set;

/**
 * Messaging tool detection — core messaging tools and provider docking.
 * Corresponds to TypeScript pi-embedded-messaging.ts.
 */
public final class EmbeddedMessaging {

    private EmbeddedMessaging() {
    }

    private static final Set<String> CORE_MESSAGING_TOOLS = Set.of("sessions_send", "message");

    /**
     * Check whether a tool name is a messaging tool.
     */
    public static boolean isMessagingTool(String toolName) {
        if (toolName == null)
            return false;
        if (CORE_MESSAGING_TOOLS.contains(toolName))
            return true;
        // TODO: provider docking via channel plugin registry
        return false;
    }

    /**
     * Check whether a messaging tool invocation is a "send" action.
     */
    public static boolean isMessagingToolSendAction(String toolName, java.util.Map<String, Object> args) {
        if (toolName == null || args == null)
            return false;
        String action = args.get("action") instanceof String s ? s.trim() : "";
        if ("sessions_send".equals(toolName))
            return true;
        if ("message".equals(toolName))
            return "send".equals(action) || "thread-reply".equals(action);
        // TODO: provider docking
        return false;
    }
}
