package com.openclaw.agent.auth;

import com.openclaw.agent.auth.AuthProfileTypes.*;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Auth profile usage tracking — cooldown, failure/success marking, exponential
 * backoff.
 * Corresponds to TypeScript auth-profiles/usage.ts.
 */
@Slf4j
public final class AuthProfileUsage {

    private AuthProfileUsage() {
    }

    // ── Cooldown check ──────────────────────────────────────────────

    /**
     * Check if a profile is currently in cooldown.
     */
    public static boolean isProfileInCooldown(AuthProfileStoreData store, String profileId) {
        if (store.getUsageStats() == null)
            return false;
        ProfileUsageStats stats = store.getUsageStats().get(profileId);
        if (stats == null)
            return false;
        long unusable = resolveUnusableUntil(stats);
        return unusable > 0 && System.currentTimeMillis() < unusable;
    }

    /**
     * Get the unusable-until timestamp for display purposes.
     */
    public static Long resolveProfileUnusableUntilForDisplay(
            AuthProfileStoreData store, String profileId) {
        if (store.getUsageStats() == null)
            return null;
        ProfileUsageStats stats = store.getUsageStats().get(profileId);
        if (stats == null)
            return null;
        long val = resolveUnusableUntil(stats);
        return val > 0 ? val : null;
    }

    // ── Mark used ───────────────────────────────────────────────────

    /**
     * Mark a profile as successfully used. Resets error count and cooldown.
     */
    public static void markAuthProfileUsed(
            AuthProfileStoreData store, String profileId, String agentDir) {
        AuthProfileStoreData updated = AuthProfileStoreManager.updateWithLock(agentDir, fresh -> {
            if (fresh.getProfiles().get(profileId) == null)
                return false;
            ensureUsageStats(fresh);
            fresh.getUsageStats().put(profileId, ProfileUsageStats.builder()
                    .lastUsed(System.currentTimeMillis())
                    .errorCount(0)
                    .build());
            return true;
        });
        if (updated != null) {
            store.setUsageStats(updated.getUsageStats());
            return;
        }
        // Fallback in-memory
        if (store.getProfiles().get(profileId) == null)
            return;
        ensureUsageStats(store);
        store.getUsageStats().put(profileId, ProfileUsageStats.builder()
                .lastUsed(System.currentTimeMillis())
                .errorCount(0)
                .build());
        AuthProfileStoreManager.saveAuthProfileStore(store, agentDir);
    }

    // ── Mark failure ────────────────────────────────────────────────

    /**
     * Mark a profile as failed for a specific reason.
     */
    public static void markAuthProfileFailure(
            AuthProfileStoreData store, String profileId,
            AuthProfileFailureReason reason, OpenClawConfig cfg, String agentDir) {

        AuthProfileStoreData updated = AuthProfileStoreManager.updateWithLock(agentDir, fresh -> {
            if (fresh.getProfiles().get(profileId) == null)
                return false;
            ensureUsageStats(fresh);
            ProfileUsageStats existing = fresh.getUsageStats().getOrDefault(
                    profileId, new ProfileUsageStats());
            fresh.getUsageStats().put(profileId,
                    computeNextStats(existing, reason, cfg, fresh.getProfiles().get(profileId)));
            return true;
        });
        if (updated != null) {
            store.setUsageStats(updated.getUsageStats());
            return;
        }
        // Fallback in-memory
        if (store.getProfiles().get(profileId) == null)
            return;
        ensureUsageStats(store);
        ProfileUsageStats existing = store.getUsageStats().getOrDefault(
                profileId, new ProfileUsageStats());
        store.getUsageStats().put(profileId,
                computeNextStats(existing, reason, cfg, store.getProfiles().get(profileId)));
        AuthProfileStoreManager.saveAuthProfileStore(store, agentDir);
    }

    /**
     * Shorthand: mark cooldown (unknown reason).
     */
    public static void markAuthProfileCooldown(
            AuthProfileStoreData store, String profileId, String agentDir) {
        markAuthProfileFailure(store, profileId, AuthProfileFailureReason.unknown, null, agentDir);
    }

    /**
     * Clear cooldown for a profile.
     */
    public static void clearAuthProfileCooldown(
            AuthProfileStoreData store, String profileId, String agentDir) {
        AuthProfileStoreData updated = AuthProfileStoreManager.updateWithLock(agentDir, fresh -> {
            if (fresh.getUsageStats() == null)
                return false;
            ProfileUsageStats stats = fresh.getUsageStats().get(profileId);
            if (stats == null)
                return false;
            stats.setErrorCount(0);
            stats.setCooldownUntil(null);
            return true;
        });
        if (updated != null) {
            store.setUsageStats(updated.getUsageStats());
            return;
        }
        if (store.getUsageStats() == null)
            return;
        ProfileUsageStats stats = store.getUsageStats().get(profileId);
        if (stats == null)
            return;
        stats.setErrorCount(0);
        stats.setCooldownUntil(null);
        AuthProfileStoreManager.saveAuthProfileStore(store, agentDir);
    }

    // ── Cooldown calculation ────────────────────────────────────────

    /**
     * Calculate cooldown duration in ms. Exponential: 1m, 5m, 25m, max 1h.
     */
    public static long calculateCooldownMs(int errorCount) {
        int normalized = Math.max(1, errorCount);
        long base = 60_000L; // 1 minute
        long raw = (long) (base * Math.pow(5, Math.min(normalized - 1, 3)));
        return Math.min(3600_000L, raw); // max 1 hour
    }

    // ── Internal ────────────────────────────────────────────────────

    private static long resolveUnusableUntil(ProfileUsageStats stats) {
        long cd = (stats.getCooldownUntil() != null && stats.getCooldownUntil() > 0)
                ? stats.getCooldownUntil()
                : 0;
        long dis = (stats.getDisabledUntil() != null && stats.getDisabledUntil() > 0)
                ? stats.getDisabledUntil()
                : 0;
        return Math.max(cd, dis);
    }

    private static void ensureUsageStats(AuthProfileStoreData store) {
        if (store.getUsageStats() == null) {
            store.setUsageStats(new LinkedHashMap<>());
        }
    }

    private static ProfileUsageStats computeNextStats(
            ProfileUsageStats existing,
            AuthProfileFailureReason reason,
            OpenClawConfig cfg,
            AuthProfileCredential cred) {

        long now = System.currentTimeMillis();

        // Defaults
        long failureWindowMs = 24 * 3600_000L;
        long billingBackoffMs = 5 * 3600_000L;
        long billingMaxMs = 24 * 3600_000L;

        boolean windowExpired = existing.getLastFailureAt() != null
                && existing.getLastFailureAt() > 0
                && (now - existing.getLastFailureAt()) > failureWindowMs;

        int baseErrors = windowExpired ? 0 : (existing.getErrorCount() != null ? existing.getErrorCount() : 0);
        int nextErrors = baseErrors + 1;

        Map<AuthProfileFailureReason, Integer> counts = windowExpired
                ? new LinkedHashMap<>()
                : (existing.getFailureCounts() != null
                        ? new LinkedHashMap<>(existing.getFailureCounts())
                        : new LinkedHashMap<>());
        counts.merge(reason, 1, Integer::sum);

        ProfileUsageStats next = existing.toBuilder()
                .errorCount(nextErrors)
                .failureCounts(counts)
                .lastFailureAt(now)
                .build();

        if (reason == AuthProfileFailureReason.billing) {
            int billingCount = counts.getOrDefault(AuthProfileFailureReason.billing, 1);
            long bMs = calculateBillingDisableMs(billingCount, billingBackoffMs, billingMaxMs);
            next.setDisabledUntil(now + bMs);
            next.setDisabledReason(AuthProfileFailureReason.billing);
        } else {
            long cdMs = calculateCooldownMs(nextErrors);
            next.setCooldownUntil(now + cdMs);
        }

        return next;
    }

    private static long calculateBillingDisableMs(int errorCount, long baseMs, long maxMs) {
        int normalized = Math.max(1, errorCount);
        long base = Math.max(60_000L, baseMs);
        long max = Math.max(base, maxMs);
        int exponent = Math.min(normalized - 1, 10);
        long raw = (long) (base * Math.pow(2, exponent));
        return Math.min(max, raw);
    }
}
