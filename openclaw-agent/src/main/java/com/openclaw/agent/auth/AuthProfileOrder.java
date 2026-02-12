package com.openclaw.agent.auth;

import com.openclaw.agent.auth.AuthProfileTypes.*;
import com.openclaw.common.config.OpenClawConfig;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Auth profile ordering — round-robin rotation with cooldown awareness.
 * Corresponds to TypeScript auth-profiles/order.ts.
 */
public final class AuthProfileOrder {

    private AuthProfileOrder() {
    }

    /**
     * Resolve the ordered list of profiles to try for a provider.
     *
     * @param cfg              OpenClaw config (nullable)
     * @param store            Auth profile store
     * @param provider         Provider ID to resolve for
     * @param preferredProfile Optional preferred profile
     * @return Ordered list of profile IDs
     */
    public static List<String> resolveAuthProfileOrder(
            OpenClawConfig cfg,
            AuthProfileStoreData store,
            String provider,
            String preferredProfile) {

        String providerKey = normalizeProviderId(provider);
        long now = System.currentTimeMillis();

        // Resolve explicit order: store override > config > auto-discover
        List<String> explicitOrder = resolveExplicitOrder(store, providerKey);
        if (explicitOrder == null) {
            explicitOrder = resolveConfigOrder(cfg, providerKey);
        }

        List<String> baseOrder;
        if (explicitOrder != null) {
            baseOrder = explicitOrder;
        } else {
            baseOrder = AuthProfileProfiles.listProfilesForProvider(store, providerKey);
        }

        if (baseOrder.isEmpty())
            return List.of();

        // Filter: valid credentials only
        List<String> filtered = baseOrder.stream()
                .filter(id -> isProfileValid(store, cfg, id, providerKey, now))
                .collect(Collectors.toCollection(ArrayList::new));

        // Deduplicate
        List<String> deduped = dedupe(filtered);

        // If explicit order, respect it but sort cooldowns to end
        if (explicitOrder != null && !explicitOrder.isEmpty()) {
            List<String> available = new ArrayList<>();
            List<Map.Entry<String, Long>> cooldown = new ArrayList<>();

            for (String id : deduped) {
                long cdUntil = resolveUnusableUntil(store, id);
                if (cdUntil > 0 && now < cdUntil) {
                    cooldown.add(Map.entry(id, cdUntil));
                } else {
                    available.add(id);
                }
            }

            cooldown.sort(Comparator.comparingLong(Map.Entry::getValue));
            List<String> ordered = new ArrayList<>(available);
            cooldown.forEach(e -> ordered.add(e.getKey()));

            if (preferredProfile != null && ordered.contains(preferredProfile)) {
                return withPreferred(ordered, preferredProfile);
            }
            return ordered;
        }

        // Round-robin mode: sort by type then lastUsed
        List<String> sorted = orderByMode(deduped, store);

        if (preferredProfile != null && sorted.contains(preferredProfile)) {
            return withPreferred(sorted, preferredProfile);
        }
        return sorted;
    }

    // ── Helpers ──────────────────────────────────────────────────────

    private static List<String> resolveExplicitOrder(AuthProfileStoreData store, String providerKey) {
        if (store.getOrder() == null)
            return null;
        for (var entry : store.getOrder().entrySet()) {
            if (normalizeProviderId(entry.getKey()).equals(providerKey)) {
                return entry.getValue();
            }
        }
        return null;
    }

    private static List<String> resolveConfigOrder(OpenClawConfig cfg, String providerKey) {
        // TODO: read auth.order from OpenClawConfig when available
        return null;
    }

    private static boolean isProfileValid(
            AuthProfileStoreData store, OpenClawConfig cfg,
            String profileId, String providerKey, long now) {

        Map<String, AuthProfileCredential> profiles = store.getProfiles();
        if (profiles == null)
            return false;
        AuthProfileCredential cred = profiles.get(profileId);
        if (cred == null)
            return false;
        if (!normalizeProviderId(cred.getProvider()).equals(providerKey))
            return false;

        switch (cred.getType()) {
            case api_key:
                return cred.getKey() != null && !cred.getKey().isBlank();
            case token:
                if (cred.getToken() == null || cred.getToken().isBlank())
                    return false;
                if (cred.getExpires() != null && cred.getExpires() > 0 && now >= cred.getExpires())
                    return false;
                return true;
            case oauth:
                return (cred.getAccess() != null && !cred.getAccess().isBlank())
                        || (cred.getRefresh() != null && !cred.getRefresh().isBlank());
            default:
                return false;
        }
    }

    private static List<String> orderByMode(List<String> order, AuthProfileStoreData store) {
        long now = System.currentTimeMillis();

        List<String> available = new ArrayList<>();
        List<String> inCooldown = new ArrayList<>();

        for (String id : order) {
            if (AuthProfileUsage.isProfileInCooldown(store, id)) {
                inCooldown.add(id);
            } else {
                available.add(id);
            }
        }

        // Sort available: type preference (oauth>token>api_key), then lastUsed asc
        available.sort((a, b) -> {
            int typeA = typeScore(store, a);
            int typeB = typeScore(store, b);
            if (typeA != typeB)
                return Integer.compare(typeA, typeB);
            return Long.compare(lastUsed(store, a), lastUsed(store, b));
        });

        // Sort cooldown by expiry (soonest first)
        inCooldown.sort(Comparator.comparingLong(id -> resolveUnusableUntil(store, id)));

        List<String> result = new ArrayList<>(available);
        result.addAll(inCooldown);
        return result;
    }

    private static int typeScore(AuthProfileStoreData store, String profileId) {
        AuthProfileCredential cred = store.getProfiles().get(profileId);
        if (cred == null)
            return 3;
        return switch (cred.getType()) {
            case oauth -> 0;
            case token -> 1;
            case api_key -> 2;
            default -> 3;
        };
    }

    private static long lastUsed(AuthProfileStoreData store, String profileId) {
        if (store.getUsageStats() == null)
            return 0;
        ProfileUsageStats stats = store.getUsageStats().get(profileId);
        return stats != null && stats.getLastUsed() != null ? stats.getLastUsed() : 0;
    }

    static long resolveUnusableUntil(AuthProfileStoreData store, String profileId) {
        if (store.getUsageStats() == null)
            return 0;
        ProfileUsageStats stats = store.getUsageStats().get(profileId);
        if (stats == null)
            return 0;
        long cd = stats.getCooldownUntil() != null ? stats.getCooldownUntil() : 0;
        long dis = stats.getDisabledUntil() != null ? stats.getDisabledUntil() : 0;
        return Math.max(cd, dis);
    }

    private static List<String> withPreferred(List<String> list, String preferred) {
        List<String> result = new ArrayList<>();
        result.add(preferred);
        for (String e : list) {
            if (!e.equals(preferred))
                result.add(e);
        }
        return result;
    }

    private static List<String> dedupe(List<String> list) {
        List<String> result = new ArrayList<>();
        Set<String> seen = new HashSet<>();
        for (String e : list) {
            if (seen.add(e))
                result.add(e);
        }
        return result;
    }

    static String normalizeProviderId(String provider) {
        if (provider == null)
            return "";
        return provider.trim().toLowerCase(Locale.ROOT)
                .replace('_', '-')
                .replace(' ', '-');
    }
}
