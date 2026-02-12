package com.openclaw.agent.runtime;

/**
 * Default constants for agent metadata when upstream does not supply them.
 * Corresponds to TypeScript agents/defaults.ts.
 */
public final class AgentDefaults {

    /** Default LLM provider. */
    public static final String DEFAULT_PROVIDER = "anthropic";

    /** Default model id (pi-ai's built-in Anthropic catalog). */
    public static final String DEFAULT_MODEL = "claude-opus-4-6";

    /** Conservative fallback when model metadata is unavailable. */
    public static final int DEFAULT_CONTEXT_TOKENS = 200_000;

    private AgentDefaults() {
    }
}
