package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Persist session usage updates (tokens, model, context) to the session store.
 * Mirrors {@code auto-reply/reply/session-usage.ts}.
 */
public final class SessionUsage {

    private static final Logger log = LoggerFactory.getLogger(SessionUsage.class);

    private SessionUsage() {
    }

    /** Usage data from a single model run. */
    public record NormalizedUsage(
            int input,
            int output,
            int cacheRead,
            int cacheWrite,
            int total) {
    }

    /** Params for persisting a usage update. */
    public record PersistUsageParams(
            String storePath,
            String sessionKey,
            NormalizedUsage usage,
            String modelUsed,
            String providerUsed,
            Integer contextTokensUsed,
            String cliSessionId,
            String logLabel) {
    }

    /**
     * Check if a usage report has non-zero values.
     */
    public static boolean hasNonzeroUsage(NormalizedUsage usage) {
        if (usage == null)
            return false;
        return usage.input() > 0 || usage.output() > 0
                || usage.cacheRead() > 0 || usage.cacheWrite() > 0
                || usage.total() > 0;
    }

    /**
     * Persist a session usage update.
     * <p>
     * In the full implementation this would update the session store on disk;
     * here we log the update for downstream integration.
     */
    public static void persistSessionUsageUpdate(PersistUsageParams params) {
        if (params.storePath() == null || params.storePath().isBlank()
                || params.sessionKey() == null || params.sessionKey().isBlank()) {
            return;
        }

        String label = params.logLabel() != null ? params.logLabel() + " " : "";

        if (hasNonzeroUsage(params.usage())) {
            try {
                int input = params.usage().input();
                int output = params.usage().output();
                int promptTokens = input + params.usage().cacheRead() + params.usage().cacheWrite();

                log.debug("{}usage: input={} output={} prompt={} model={} provider={}",
                        label, input, output, promptTokens,
                        params.modelUsed(), params.providerUsed());
                // Full session store update deferred to session subsystem integration
            } catch (Exception e) {
                log.warn("failed to persist {}usage update: {}", label, String.valueOf(e));
            }
            return;
        }

        if (params.modelUsed() != null || params.contextTokensUsed() != null) {
            try {
                log.debug("{}model/context update: model={} context={}",
                        label, params.modelUsed(), params.contextTokensUsed());
                // Full session store update deferred to session subsystem integration
            } catch (Exception e) {
                log.warn("failed to persist {}model/context update: {}", label, String.valueOf(e));
            }
        }
    }
}
