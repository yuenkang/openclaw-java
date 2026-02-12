package com.openclaw.agent.auth;

import com.openclaw.agent.auth.AuthProfileTypes.*;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

/**
 * Session-level auth profile override — auto-rotation per session/compaction.
 * Corresponds to TypeScript auth-profiles/session-override.ts.
 */
@Slf4j
public final class AuthProfileSessionOverride {

    private AuthProfileSessionOverride() {
    }

    /**
     * Session auth override state.
     */
    public record SessionAuthState(
            String authProfileOverride,
            String authProfileOverrideSource,
            Integer authProfileOverrideCompactionCount,
            Integer compactionCount) {
    }

    /**
     * Resolve which auth profile to use for a session.
     *
     * @param cfg          Config
     * @param provider     Provider ID
     * @param agentDir     Agent directory
     * @param currentState Current session auth state (or null)
     * @param isNewSession Whether this is a new session
     * @return Profile ID to use, or null if no profiles available
     */
    public static String resolveSessionAuthProfileOverride(
            OpenClawConfig cfg, String provider, String agentDir,
            SessionAuthState currentState, boolean isNewSession) {

        AuthProfileStoreData store = AuthProfileStoreManager.ensureAuthProfileStore(agentDir);
        List<String> order = AuthProfileOrder.resolveAuthProfileOrder(
                cfg, store, provider, null);

        if (order.isEmpty())
            return null;

        String current = currentState != null ? currentState.authProfileOverride() : null;

        // Validate current profile still exists and matches provider
        if (current != null) {
            AuthProfileCredential cred = store.getProfiles().get(current);
            if (cred == null || !AuthProfileOrder.normalizeProviderId(cred.getProvider())
                    .equals(AuthProfileOrder.normalizeProviderId(provider))) {
                current = null;
            }
            if (current != null && !order.contains(current)) {
                current = null;
            }
        }

        // User-selected override: keep it unless new session
        String source = currentState != null ? currentState.authProfileOverrideSource() : null;
        if ("user".equals(source) && current != null && !isNewSession) {
            return current;
        }

        // Pick next profile
        String next = current;
        if (isNewSession) {
            next = current != null ? pickNext(order, current, store) : pickFirst(order, store);
        } else if (current != null) {
            int compaction = currentState != null && currentState.compactionCount() != null
                    ? currentState.compactionCount()
                    : 0;
            int storedCompaction = currentState != null
                    && currentState.authProfileOverrideCompactionCount() != null
                            ? currentState.authProfileOverrideCompactionCount()
                            : compaction;
            if (compaction > storedCompaction) {
                next = pickNext(order, current, store);
            } else if (AuthProfileUsage.isProfileInCooldown(store, current)) {
                next = pickFirst(order, store);
            }
        } else {
            next = pickFirst(order, store);
        }

        return next != null ? next : current;
    }

    // ── Helpers ──────────────────────────────────────────────────────

    private static String pickFirst(List<String> order, AuthProfileStoreData store) {
        for (String id : order) {
            if (!AuthProfileUsage.isProfileInCooldown(store, id))
                return id;
        }
        return order.isEmpty() ? null : order.get(0);
    }

    private static String pickNext(
            List<String> order, String active, AuthProfileStoreData store) {
        int startIndex = order.indexOf(active);
        if (startIndex < 0)
            return pickFirst(order, store);
        for (int offset = 1; offset <= order.size(); offset++) {
            String candidate = order.get((startIndex + offset) % order.size());
            if (!AuthProfileUsage.isProfileInCooldown(store, candidate))
                return candidate;
        }
        return order.get(startIndex);
    }
}
