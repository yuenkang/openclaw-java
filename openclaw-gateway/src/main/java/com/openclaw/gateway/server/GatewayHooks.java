package com.openclaw.gateway.server;

/**
 * Gateway hooks request handler — dispatches incoming HTTP hook
 * requests to the appropriate handler (wake or agent).
 * <p>
 * Mirrors {@code server/hooks.ts :: createGatewayHooksRequestHandler}.
 * The Java implementation uses dependency-injection style: callers
 * provide the dispatch functions and the handler orchestrates them.
 */
public final class GatewayHooks {

    private GatewayHooks() {
    }

    /**
     * Parameters for the wake hook dispatch.
     */
    public record WakeHookValue(String text, String mode) {
    }

    /**
     * Parameters for the agent hook dispatch.
     */
    public record AgentHookValue(
            String message,
            String name,
            String wakeMode,
            String sessionKey,
            boolean deliver,
            String channel,
            String to,
            String model,
            String thinking,
            Integer timeoutSeconds,
            Boolean allowUnsafeExternalContent) {
    }

    /**
     * Functional interface for dispatching wake hooks.
     */
    @FunctionalInterface
    public interface WakeHookDispatcher {
        void dispatch(WakeHookValue value);
    }

    /**
     * Functional interface for dispatching agent hooks.
     * Returns a run-ID string.
     */
    @FunctionalInterface
    public interface AgentHookDispatcher {
        String dispatch(AgentHookValue value);
    }
}
