package com.openclaw.agent.auth;

/**
 * Auth profile constants.
 * Corresponds to TypeScript auth-profiles/constants.ts.
 */
public final class AuthProfileConstants {

    private AuthProfileConstants() {
    }

    public static final int AUTH_STORE_VERSION = 1;
    public static final String AUTH_PROFILE_FILENAME = "auth-profiles.json";
    public static final String LEGACY_AUTH_FILENAME = "auth.json";

    // Well-known CLI profile IDs
    public static final String CLAUDE_CLI_PROFILE_ID = "anthropic:claude-cli";
    public static final String CODEX_CLI_PROFILE_ID = "openai-codex:codex-cli";
    public static final String QWEN_CLI_PROFILE_ID = "qwen-portal:qwen-cli";
    public static final String MINIMAX_CLI_PROFILE_ID = "minimax-portal:minimax-cli";

    // Lock options
    public static final int LOCK_MAX_RETRIES = 10;
    public static final long LOCK_MIN_TIMEOUT_MS = 100;
    public static final long LOCK_MAX_TIMEOUT_MS = 10_000;
    public static final long LOCK_STALE_MS = 30_000;

    // External CLI sync
    public static final long EXTERNAL_CLI_SYNC_TTL_MS = 15 * 60 * 1000L;
    public static final long EXTERNAL_CLI_NEAR_EXPIRY_MS = 10 * 60 * 1000L;
}
