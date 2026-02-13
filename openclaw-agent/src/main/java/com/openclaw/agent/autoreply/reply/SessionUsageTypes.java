package com.openclaw.agent.autoreply.reply;

/**
 * Parameters for persisting session usage updates.
 * Mirrors the parameter shape from {@code auto-reply/reply/session-usage.ts}.
 */
public final class SessionUsageTypes {

    private SessionUsageTypes() {
    }

    /** Normalized usage token breakdown. */
    public record NormalizedUsage(
            int input,
            int output,
            int cacheRead,
            int cacheWrite,
            int total) {

        public boolean hasNonzeroUsage() {
            return input > 0 || output > 0 || cacheRead > 0 || cacheWrite > 0 || total > 0;
        }
    }

    /** System prompt report to store alongside session entry. */
    public record SessionSystemPromptReport(
            int tokens,
            int characters,
            String hash) {
    }

    /** All params required by persistSessionUsageUpdate. */
    public record PersistUsageParams(
            String storePath,
            String sessionKey,
            NormalizedUsage usage,
            String modelUsed,
            String providerUsed,
            Integer contextTokensUsed,
            SessionSystemPromptReport systemPromptReport,
            String cliSessionId,
            String logLabel) {
    }
}
