package com.openclaw.agent.auth;

import com.openclaw.agent.auth.AuthProfileTypes.*;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Profile CRUD operations and provider filtering.
 * Corresponds to TypeScript auth-profiles/profiles.ts.
 */
public final class AuthProfileProfiles {

    private AuthProfileProfiles() {
    }

    /**
     * List all profile IDs associated with a provider.
     */
    public static List<String> listProfilesForProvider(AuthProfileStoreData store, String provider) {
        String providerKey = AuthProfileOrder.normalizeProviderId(provider);
        if (store.getProfiles() == null)
            return List.of();
        return store.getProfiles().entrySet().stream()
                .filter(e -> AuthProfileOrder.normalizeProviderId(e.getValue().getProvider())
                        .equals(providerKey))
                .map(Map.Entry::getKey)
                .collect(Collectors.toList());
    }

    /**
     * Add or update a credential in the store.
     */
    public static void upsertAuthProfile(
            String profileId, AuthProfileCredential credential, String agentDir) {
        AuthProfileStoreData store = AuthProfileStoreManager.ensureAuthProfileStore(agentDir);
        store.getProfiles().put(profileId, credential);
        AuthProfileStoreManager.saveAuthProfileStore(store, agentDir);
    }

    /**
     * Mark a profile as last-known-good for its provider.
     */
    public static void markAuthProfileGood(
            AuthProfileStoreData store, String provider, String profileId, String agentDir) {
        AuthProfileStoreData updated = AuthProfileStoreManager.updateWithLock(agentDir, fresh -> {
            AuthProfileCredential cred = fresh.getProfiles().get(profileId);
            if (cred == null || !cred.getProvider().equals(provider))
                return false;
            if (fresh.getLastGood() == null)
                fresh.setLastGood(new LinkedHashMap<>());
            fresh.getLastGood().put(provider, profileId);
            return true;
        });

        if (updated != null) {
            store.setLastGood(updated.getLastGood());
            return;
        }

        // Fallback: update in-memory
        AuthProfileCredential cred = store.getProfiles().get(profileId);
        if (cred == null || !cred.getProvider().equals(provider))
            return;
        if (store.getLastGood() == null)
            store.setLastGood(new LinkedHashMap<>());
        store.getLastGood().put(provider, profileId);
        AuthProfileStoreManager.saveAuthProfileStore(store, agentDir);
    }

    /**
     * Set the profile order for a provider.
     */
    public static AuthProfileStoreData setAuthProfileOrder(
            String agentDir, String provider, List<String> order) {
        String providerKey = AuthProfileOrder.normalizeProviderId(provider);

        List<String> sanitized = (order != null)
                ? order.stream()
                        .map(String::trim)
                        .filter(s -> !s.isEmpty())
                        .distinct()
                        .collect(Collectors.toList())
                : List.of();

        return AuthProfileStoreManager.updateWithLock(agentDir, store -> {
            if (store.getOrder() == null)
                store.setOrder(new LinkedHashMap<>());
            if (sanitized.isEmpty()) {
                if (!store.getOrder().containsKey(providerKey))
                    return false;
                store.getOrder().remove(providerKey);
                if (store.getOrder().isEmpty())
                    store.setOrder(null);
            } else {
                store.getOrder().put(providerKey, sanitized);
            }
            return true;
        });
    }
}
