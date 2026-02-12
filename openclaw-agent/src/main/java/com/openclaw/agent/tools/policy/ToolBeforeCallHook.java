package com.openclaw.agent.tools.policy;

import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * Before-tool-call hook — runs plugin hooks before tool execution, can block or
 * modify params.
 * Corresponds to TypeScript pi-tools.before-tool-call.ts.
 */
@Slf4j
public final class ToolBeforeCallHook {

    private ToolBeforeCallHook() {
    }

    /** Outcome of a before-tool-call hook. */
    public sealed interface HookOutcome permits Blocked, Allowed {
    }

    public record Blocked(String reason) implements HookOutcome {
    }

    public record Allowed(Map<String, Object> params) implements HookOutcome {
    }

    /** Context for the hook. */
    public record HookContext(String agentId, String sessionKey) {
    }

    /**
     * Run the before-tool-call hook.
     * Currently stubbed — plugins not yet integrated in Java.
     */
    @SuppressWarnings("unchecked")
    public static HookOutcome runBeforeToolCallHook(
            String toolName, Object params, String toolCallId, HookContext ctx) {
        // TODO: integrate with plugin hook runner when available
        Map<String, Object> paramsMap = params instanceof Map<?, ?> m
                ? (Map<String, Object>) m
                : Map.of();
        return new Allowed(paramsMap);
    }

    /**
     * Check if any hooks are registered for before_tool_call.
     * Currently always returns false (stubbed).
     */
    public static boolean hasBeforeToolCallHooks() {
        // TODO: check global hook runner
        return false;
    }
}
